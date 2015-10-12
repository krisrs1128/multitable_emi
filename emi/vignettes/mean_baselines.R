
################################################################################
# Experiment using the means for artists / users / tracks times in the baseline
# predictor.
################################################################################

# ---- libraries ----
library("plyr")
library("dplyr")
library("emi")
library("data.table")
library("Matrix")
library("ggplot2")
library("caret")

# ---- get-data ----
data(train)
data(test)

# only using 1/5 of users in this experiment
subset_users <- sample(as.character(unique(train$User)), 1e3)
train <- filter(train, User %in% subset_users)
users <- filter(users, User %in% subset_users)
words <- filter(words, User %in% subset_users)
test_ix <- get_folds(n = nrow(train))

# a lot of train and test overlap
length(intersect(unique(train$User), unique(test$User)))
length(setdiff(unique(train$User), unique(test$User)))
length(setdiff(unique(test$User), unique(train$User)))

length(intersect(unique(cur_train$User), unique(cur_test$User)))
length(setdiff(unique(cur_train$User), unique(cur_test$User)))
length(setdiff(unique(cur_test$User), unique(cur_train$User)))

rmses <- matrix(nrow = length(test_ix), ncol = 6)
colnames(rmses) <- c("mean", "user", "artist", "time", "svd", "track")

for(k in seq_along(test_ix)) {
  cat(sprintf("Starting cv fold %g \n", k))
  cur_train <- train[-test_ix[[k]], ]
  cur_test <- train[test_ix[[k]], ]

  # make predictions
  y_hat_mean <- rep(mean(cur_train$Rating), nrow(cur_test))
  y_hat_user <- mean_predictor(cur_train, cur_test)
  y_hat_artist <- mean_predictor(cur_train, cur_test, "Artist")
  y_hat_time <- mean_predictor(cur_train, cur_test, "Time")
  y_hat_track <- mean_predictor(cur_train, cur_test, "Track")
  y_hat_svd <- svd_predictor(cur_train, cur_test, r = 4, alpha = 0.9)$y_hat

  # Get RMSEs
  rmses[k, "mean"] <- RMSE(y_hat_mean, cur_test$Rating)
  rmses[k, "artist"] <- RMSE(y_hat_artist, cur_test$Rating)
  rmses[k, "user"] <- RMSE(y_hat_user, cur_test$Rating)
  rmses[k, "time"] <- RMSE(y_hat_time, cur_test$Rating)
  rmses[k, "track"] <- RMSE(y_hat_track, cur_test$Rating)
  rmses[k, "svd"] <- RMSE(y_hat_svd, cur_test$Rating)
}

# We actually do better just predicting the overall mean in the training data
mrmses <- melt(rmses)
ggplot(mrmses) +
  geom_histogram(aes(x = value), binwidth = .05) +
  facet_grid(Var2 ~ .)

ggplot(mrmses) +
  geom_histogram(aes(x = value), binwidth = .05) +
  facet_grid(Var2 ~ .) +
  xlim(21, 24)

# ---- svd-ideas ----
svd_res <- svd_predictor(cur_train, cur_test, r = 10)
plot(svd_res$svd$d)
plot(svd_res$svd$u[, 1:2])

ggplot(data.frame(truth = cur_test$Rating, pred = svd_res$y_hat)) +
  geom_point(aes(x = truth, y = pred), alpha = 0.25, size = .7) +
  coord_fixed()
