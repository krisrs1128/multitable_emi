
################################################################################
# R script for writeup describing emi multitable data analysis
################################################################################

## ---- libraries ----
library("caret")
library("data.table")
library("emi")
library("expPCA")
library("imputation")
library("ggplot2")
library("plyr")
library("dplyr")
library("proxy")
library("reshape2")
library("xtable")

## ---- load-data ----
data(users)
data(words)
data(train)
data(test)

## ---- process-data ----
words_small <- words %>% filter(User %in% train$User[1:1000])
# imputation on words
words_num <- as.matrix(words_small[, -c(1:2), with = F])
words_hat <- SVDImpute(words_num, k = 5, verbose = FALSE)
words_small <- data.table(words_small[, 1:2, with = F], words_hat$x)
users_small <- users %>% filter(User %in% train$User[1:1000])
data_list <- list(train = train[1:10000, ], words = words_small, users = users_small)

# for inspecting one fold (not full cv)
folds_ix <- get_folds(n = 10000, K = 10)
train_list <- list(train = train[unlist(folds_ix[1:9]), ], words = words_small, users = users_small)
test_list <- list(train = train[folds_ix[[10]], ], words = words_small, users = users_small)

## ---- data-dims ----
data_dims <- data.frame(c("users", "words", "train", "test"),
                        rbind(dim(users), dim(words), dim(train), dim(test)))
colnames(data_dims) <- c("Data", "Rows", "Columns")
data_dims %>%
  xtable(caption = "The number of samples and features available in the different data sets. Note that each individual user may appear multiple times in the words and training andtesting data sets, since they are asked to rate many tracks.",
         label = "tab:data_dims")

## ---- ratings-hist ----
test$Rating <- NA
train_test <- rbind(data.table(type = "train", train),
                    data.table(type = "test", test))
ggplot(filter(train_test, type == "train")) +
  geom_histogram(aes(x = Rating), binwidth = 1) +
  ggtitle("Ratings in Training Data")

## ---- artist-counts ----
artist_counts <- train_test %>%
  group_by(type, Artist, Time) %>%
  summarise(artist_count = n())
ggplot(artist_counts) +
  geom_bar(aes(x = reorder(Artist, desc(artist_count)), y = artist_count, fill = Time), stat = "identity", position = "dodge") +
  facet_grid(type ~ .) +
  theme(axis.text.x = element_text(angle = -90)) +
  scale_x_discrete("Artist ID") +
  scale_y_continuous("count") +
  scale_fill_discrete("Survey Session") +
  ggtitle("Counts of Artists across Sessions")

## ---- track-counts ----
track_counts <- train_test %>%
  group_by(type, Track, Time) %>%
  summarise(track_count = n())
ggplot(track_counts) +
  geom_bar(aes(x = reorder(Track, desc(track_count)), y = track_count, fill = Time), stat = "identity", position = "dodge") +
  scale_x_discrete("Track ID") +
  scale_y_continuous("count") +
  scale_fill_discrete("Survey Session") +
  facet_grid(type ~ .) +
  theme(axis.text.x = element_text(angle = -90, size = 4)) +
  ggtitle("Counts of Tracks across Sessions")

## ---- user-counts ----
user_counts <- train_test %>%
  group_by(type, User) %>%
  summarise(user_count = n()) %>%
  dcast(User ~ type, fill = 0)
user_counts <- user_counts %>%
  group_by(test, train) %>%
  summarise(n_users = n())
ggplot(user_counts) +
  geom_point(aes(x = train, y = test, col = log(n_users, base = 10))) +
  scale_color_gradient2("Number of Users (log base 10)", midpoint = log(50, base = 10), low = "steelblue", high = "indianred", mid = "plum3") +
  scale_x_continuous("# Appearances in Training") +
  scale_y_continuous("# Appearances in Test") +
  ggtitle("Number of User Appearances in Training vs. Test") +
  coord_fixed()

## ---- user-pair-example ----
ggplot(users) +
  stat_density2d(aes(x = I.love.technology, y = People.often.ask.my.advice.on.music.what.to.listen.to,  fill = ..level..),
                 geom = "polygon", bins = 150, h = 10, alpha = 0.6) +
  scale_fill_gradient(low = "white", high = "indianred") +
  geom_point(aes(x = I.love.technology, y = People.often.ask.my.advice.on.music.what.to.listen.to), alpha = 0.1, size = 1) +
  ggtitle("Example pair of questions in users data set")

## ---- baseline-model ----
caret_opts <- list(train_control = trainControl(number = 5, verbose = FALSE),
                   method = "glmnet")
glmnet_res <- evaluate(data_list, caret_train, caret_opts, caret_predict)
glmnet_res

## ---- elnet-preds-plot ----
trained_model <- caret_train(data_list, caret_opts)
y_hat <- caret_predict(trained_model, data_list)
training_errs <- data.frame(y = data_list$train$Rating, y_hat = y_hat)
RMSE(training_errs[, 1], training_errs[, 2]) # training error

ggplot(training_errs) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(b = 1, a = 0, col = "red") +
  coord_fixed() +
  ggtitle("Training Errors for Benchmark Model")

ggplot((glmnet_res$pred_pairs[[1]])) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(b = 1, a = 0, col = "red") +
  coord_fixed() +
  ggtitle("Test Errors for Benchmark Model [Fold 1]")

## ---- baseline-with-shrink ----
# manually chosen point to shrink towards points chosen by eye
caret_opts$postprocess_fun <- function(y) shrink_to_centers(y, c(10, 30, 50, 70, 90), 0.1)
glmnet_res_shrink <- evaluate(data_list, caret_train, caret_opts, caret_predict)
glmnet_res_shrink

## ---- baseline-with-shrink-fig ----
trained_model <- caret_train(data_list, caret_opts)
y_hat <- caret_predict(trained_model, data_list)
training_errs <- data.frame(y = data_list$train$Rating, y_hat = y_hat)

ggplot(training_errs) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(b = 1, a = 0, col = "red") +
  coord_fixed() +
  ggtitle("Training Errors for Benchmark Model, with Shrinkage")

ggplot((glmnet_res_shrink$pred_pairs[[1]])) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(b = 1, a = 0, col = "red") +
  coord_fixed() +
  ggtitle("Test Errors for Benchmark Model, with Shrinkage [held out fold 1]")

## ---- svd-model ----
# keep only users with lots of ratings
train <- train %>%
  group_by(User) %>%
  mutate(n = n()) %>%
  arrange(desc(n))
train$n <- NULL

svd_results <- evaluate(data_list, svd_train, list(k = 5), svd_predict)
svd_results

## ---- svd-model-plot ----
ggplot(svd_results$pred_pairs[[1]]) +
  geom_abline(aes(a = 0, b = 1), col = "red") +
  geom_point(aes(y, y_hat), size = 1) +
  coord_fixed() +
  ggtitle("SVD Predictions [Fold 1]")

## ---- svd-model-study ----
mR_test <- data.table(test_list$train[, c("User", "Track"), with = F], Rating = NA)
mR <- rbind(train_list$train[, c("User", "Track", "Rating"), with = F], mR_test)
R <- cast_ratings(mR)

# Supplied ratings matrix
R[1:10, 1:10]

# imputed ratings matrix
opts <- merge_svd_opts()
R_hat <- svd_impute(R, opts$k, opts$alpha, opts$min_val, opts$max_val)
round(R_hat[1:10, 1:10], 2)

## ---- svd-model-study-scatter ----
svd_preds_study <- data.frame(R = as.numeric(R),
                              R_hat = as.numeric(R_hat),
                              missing = as.numeric(is.na(R)))
svd_preds_study[svd_preds_study$missing == 1, "R"] <- -1
ggplot(svd_preds_study) +
  geom_point(aes(x = R, y = R_hat, col = as.factor(missing)), size = 1, alpha = 0.5) +
  coord_fixed() +
  geom_abline(b = 0, a = 1, col = "red")

## ---- svd-model-study-histo ----
ggplot(svd_preds_study) +
  geom_histogram(aes(x = R_hat), binwidth = 2) +
  facet_grid(missing ~ ., scale = "free_y")

## ---- svd-with-cov ----
opts <- merge_svd_cov_opts(list(lambdas = c(0.0001, 0.0001, .01),
                                gamma_pq = 0.001, gamma_beta = 5e-6,
                                n_iter = 20, verbose = FALSE))
svd_cov_results <- evaluate(data_list, svd_cov_train, opts, svd_cov_predict)
svd_cov_results

## ---- svd-with-cov-scatter ----
ggplot(svd_cov_results$pred_pairs[[1]]) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  coord_fixed() +
  ggtitle("Predictions from SVD with Covariates [Fold 1]")

## ---- svd-with-cov-data ----
# we can dissect the pieces of this prediction
pred_data <- prepare_pred_data(train_list, test_list)
pred_data$X[1:10, 1:10]
pred_data$Z[1:3, 1:3, 1:3]
impute_res <- svd_cov_impute(pred_data$X, pred_data$Z, opts)
RMSE(test_list$train$Rating, postprocess_preds(impute_res$X_hat, test_list))

## ---- svd-with-cov-obj ----
obj <- impute_res$res$obj
ggplot(data.frame(iter = seq_along(obj), obj = obj)) +
  geom_line(aes(x = iter, y = obj)) +
  ggtitle("Objective across Gradient Descent Iterations")

## ---- svd-with-cov-scores ----
ggplot(data.frame(P = impute_res$res$P, Z = pred_data$Z[,1,])) +
  geom_point(aes(x = P.1, y = P.2, col = Z.AGE)) +
  ggtitle("User Scores after SVD with Covariates")

## ---- svd-with-cov-beta ----
ggplot(data.frame(x = dimnames(pred_data$Z)[[3]], y = impute_res$res$beta)) +
  geom_point(aes(x = reorder(x, y, mean), y = y)) +
  theme(axis.text.x = element_text(angle = -90, size = 7)) +
  ggtitle("Coefficients from SVD with Covariates")

## ---- simulate-bern-data ----
n <- 1000
k <- 3
X_data <- generate_bern_data(k, n, d = 20)
heatmap(X_data$X)

## ---- pca ----
pca_res <- bern_exp_pca(X_data$X, 2, 5, 10, lambda = 0.1)
a_df <- data.frame(a = pca_res$A, label = X_data$copies)

## ---- exp-pca-res ----
ggplot(a_df) +
  geom_jitter(aes(x = a.1, y = a.2, col = as.factor(label)), alpha = 0.5) +
  ggtitle("Exponential Family PCA Scores")

## ---- pca-eigenvectors ----
mV <- melt(pca_res$V)
ggplot(mV) +
  geom_point(aes(x = Var1, y = value, col = as.factor(Var2))) +
  ggtitle("Estimated Subspace [basis vectors]")

## ---- normal-pca ----
normal_pc <- princomp(X_data$X)$scores[, 1:2]
ggplot(data.frame(normal_pc, label = X_data$copies)) +
  geom_point(aes(x = Comp.1, y = Comp.2, col = as.factor(label))) +
  ggtitle("Using PCA")

## ---- normal-cmdscale ----
x_dist <- dist(X_data$X, method = "jaccard")
cmd_res <- data.frame(cmdscale(x_dist), label = X_data$copies)
ggplot(cmd_res) +
  geom_point(aes(x = X1, y = X2, col = as.factor(label))) +
  ggtitle("Using MDS")
