
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
data_list <- list(train = train[1:10000, ], users = users, words = words)
caret_opts <- list(train_control = trainControl(number = 5, verbose = TRUE))
glmnet_res <- evaluate(data_list, caret_train, caret_opts, caret_predict)

## ---- elnet-preds-plot ----
trained_model <- caret_train(data_list, caret_opts)
y_hat <- caret_predict(trained_model, data_list)
training_errs <- data.frame(y = data_list$train$Rating, y_hat = y_hat)
RMSE(training_errs[, 1], training_errs[, 2])

ggplot(training_errs) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(b = 1, a = 0, col = "red") +
  coord_fixed() +
  ggtitle("Training Errors for Benchmark Model")

ggplot((glmnet_res$pred_pairs[[1]])) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(b = 1, a = 0, col = "red") +
  coord_fixed() +
  ggtitle("Test Errors for Benchmark Model [held out fold 1]")

## ---- svd-model ----


## ---- svd-with-cov ----
words_small <- words %>% filter(User %in% train$User[1:1000])
users_small <- users %>% filter(User %in% train$User[1:1000])

# need to do imputation on words, because covariates optimization assumes full Z
# matrix is available
words_num <- as.matrix(words_small[, -c(1:2), with = F])
words_hat <- SVDImpute(words_num, k = 5, num.iters = 20, verbose = FALSE)
words_small <- data.table(words_small[, 1:2, with = F], words_hat$x)

data_list <- list(train = train[1:900, ],
                  words = words_small,
                  users = users_small)
newdata <- list(train = train[901:1000, ],
                  words = words_small,
                  users = users_small)

svd_cov_model <- svd_cov_train(data_list, list(n_iter = 100))
preds <- svd_cov_predict(svd_cov_model, newdata)

# we can dissect the pieces of this prediction
pred_data <- prepare_pred_data(data_list, data_list)
dim(pred_data$X)
dim(pred_data$Z)

impute_res <- svd_cov_impute(pred_data$X, pred_data$Z, trained_model$opts)
plot(impute_res$res$obj)
plot(impute_res$res$P)
plot(impute_res$res$beta)

data.frame(newdata$train$Rating,
           y_hat = postprocess_preds(impute_res$X_hat, newdata))

# we can also cross-validate, though if we aren't even able to overfit to the
# training data, there isn't really much of a point.
svd_cov_results <- evaluate(list(train = train, words = words, users = users),
                            svd_cov_train, list(n_iter = 10), svd_cov_predict)

ggplot(svd_cov_results$pred_pairs[[1]]) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  coord_fixed()

## ---- simulate-bern-data ----
n <- 1000
k <- 3
X_data <- generate_bern_data(k, n, d = 20)
heatmap(X_data$X)

## ---- pca ----
pca_res <- invisible(bern_exp_pca(X_data$X, 2, 5, 10, lambda = 0.1))
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
  ggtitle("Using ordinary PCA")

## ---- normal-cmdscale ----
x_dist <- dist(X_data$X, method = "jaccard")
cmd_res <- data.frame(cmdscale(x_dist), label = X_data$copies)
ggplot(cmd_res) +
  geom_point(aes(x = X1, y = X2, col = as.factor(label)))
