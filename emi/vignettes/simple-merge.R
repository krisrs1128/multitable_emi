
################################################################################
# Experiment in developing new featurizations for prediction using user and
# artist characteristics. The idea is that one way to deal with multitable data
# is to turn it into one table.
################################################################################

# ---- load-libraries ----
library("caret")
library("plyr")
library("dplyr")
library("emi")

trControl <- trainControl(verbose = T)

# ---- get-data ----
data(train)
data(test)
data(users)
data(words)

# only using 1/5 of users in this experiment
subset_users <- as.character(sample(unique(train$User), 1e3))
train <- filter(train, User %in% subset_users)
users <- filter(users, User %in% subset_users)
words <- filter(words, User %in% subset_users)
test_ix <- get_folds(n = nrow(train))

# ---- summaries ----
dim(train)
dim(users)
dim(words)

summary(train)
summary(users)
summary(words)

# ---- merge-survey ----
train_user <- merge(train, users, by = c("User"), all.x = TRUE)
train_numeric <- as.matrix(get_numeric(train_user))
train_numeric <- train_numeric[, -1] # remove rating column
train_numeric <- preprocess_data(train_numeric)
train_numeric <- cbind(train_numeric, class2ind(train_user$Artist, drop2nd = T))
user_model <- caret::train(x = train_numeric[-test_ix[[1]], ],
                           y = train_user$Rating[-test_ix[[1]]],
                           method = "glmnet", trControl = trControl)
user_model
plot_lasso_coef(coef(user_model$finalModel)[-1, ])

# train and test along folds
rmses <- seq_along(test_ix)
for(k in seq_along(test_ix)) {
  cat(sprintf("fold %g \n", k))
  user_model <- caret::train(x = train_numeric[-test_ix[[k]], ],
                             y = train_user$Rating[-test_ix[[k]]],
                             method = "glmnet", trControl = trControl)
  y_hat <- predict(user_model, newdata = train_numeric[test_ix[[k]], ])
  rmses[k] <- RMSE(y_hat, train[test_ix[[k]], "Rating", with = F])
}
hist(rmses, 20)

user_model <- caret::train(x = train_numeric[-test_ix[[1]], ],
                           y = train_user$Rating[-test_ix[[1]]],
                           method = "rpart", trControl = trControl)
user_model
plot(user_model$finalModel)
text(user_model$finalModel)

user_model <- caret::train(x = train_numeric[-test_ix[[1]], ],
                           y = train_user$Rating[-test_ix[[1]]],
                           method = "rf", trControl = trControl)

# ---- merge-artists ----

# merge artist / track words if the user actually listened to them
head(words)
train_words <- merge(train, words, by = c("Artist", "User"), all.x = TRUE)
words_numeric <- get_numeric(train_words)
x <- words_numeric[-test_ix[[1]], -1]
y <- words_numeric[-test_ix[[1]], 1]
x <- preprocess_data(x)

words_model <- caret::train(x = x, y = y, method = "glmnet",
                            trControl = trControl)
words_model
plot_lasso_coef(coef(words_model$finalModel)[-1, ])

words_model <- caret::train(x = x, y = y, method = "rpart",
                            trControl = trControl)
words_model
plot(words_model$finalModel)
text(words_model$finalModel)

words_model <- caret::train(x = x, y = y, method = "rf",
                            trControl = trControl)
words_model

# ---- merge-everything ----
train_all <- merge(train_words, users, by = "User", all.x = T)
dim(train_all)

x <- expand_factors(train_all[, setdiff(colnames(train_all), "User"), with = F])
x[, "Rating":=NULL]
x <- preprocess_data(as.matrix(x))
y <- train_all$Rating
all_model <- caret::train(x = x, y = y, method = "glmnet",
                          trControl = trControl)
