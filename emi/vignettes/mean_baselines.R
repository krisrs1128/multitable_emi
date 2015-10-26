
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
data(users)
data(words)

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

rmses <- list()
rmses$track <- evaluate(list(train = train, users = users, words = words),
                        mean_model,
                        list(type = "Track"),
                        mean_pred, n_rep = 10)$errs
rmses$artist <- evaluate(list(train = train, users = users, words = words),
                        mean_model,
                        list(type = "Artist"),
                        mean_pred, n_rep = 10)$errs
rmses$user <- evaluate(list(train = train, users = users, words = words),
                        mean_model,
                        list(type = "User"),
                        mean_pred, n_rep = 10)$errs

# We actually do better just predicting the overall mean in the training data
mrmses <- melt(rmses)
ggplot(mrmses) +
  geom_histogram(aes(x = value), binwidth = .05) +
  facet_grid(L1 ~ .)
