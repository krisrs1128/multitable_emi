
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
X <- merge(users, words, by = "User", all = T)
X <- merge(train, X, by = c("User", "Artist"), all.x = T)
train_ix <- sample(seq_len(nrow(X)), .5 * nrow(X))
x_num <- as.matrix(X[, 6:ncol(X), with = F])
x_num <- scale(x_num, center = F, scale = apply(x_num, 2, max, na.rm = T))
x_hat <- SVDImpute(x_num, 10, verbose = F)
x_hat <- x_hat$x
x_fac <- expand_factors(X[, c("Track", "Time"), with = F])
x <- cbind(x_fac, x_hat)
y <- X$Rating
glmnet_model <- train(x = x[train_ix, ], y = y[train_ix], method = "glmnet", trControl = trainControl(verbose = F))
glmnet_model
glmnet_pred_df <- rbind(data.table(type = "train", y = y[train_ix], y_hat = predict(glmnet_model)),
      data.table(type = "test", y = y[-train_ix], y_hat = predict(glmnet_model, newdata = x[-train_ix, ])))
train_rmse <- RMSE(filter(glmnet_pred_df, type == "train")[, 3, with = F], filter(glmnet_pred_df, type == "train")[, 2, with = F])
test_rmse <- RMSE(filter(glmnet_pred_df, type == "test")[, 3, with = F], filter(glmnet_pred_df, type == "test")[, 2, with = F])

## ---- elnet-preds-plot ----
ggplot(glmnet_pred_df) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.01) +
  geom_abline(b = 1, a = 0, col = "red") +
  coord_fixed() +
  facet_wrap(~type) +
  ggtitle("Test and Train Errors for Benchmark Model")

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
