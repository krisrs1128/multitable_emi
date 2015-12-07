
################################################################################
# Experiment using vanilla SVD, as described in Montanari's EE378B notes
################################################################################


## ---- libraries ----
library("data.table")
library("emi")
library("imputation")
library("plyr")
library("dplyr")
library("ggplot2")
theme_set(theme_bw())

## ---- evaluate ----
data(train)
data(words)
data(users)

words <- words %>% filter(User %in% train$User)
users <- users %>% filter(User %in% train$User)

# using very small subset of the data
data_list <- list(train = train[1:900, ],
                  words = words,
                  users = users)
newdata <- list(train = train[901:1000, ],
                words = words,
                users = users)

trained_model <- svd_train(data_list)
preds <- svd_predict(trained_model, newdata)
plot(newdata$train$Rating, preds)

# using full data
svd_model_results <- evaluate(list(train = train, words = words, users = users),
                              svd_train, list(), svd_predict)
ggplot(data.frame(svd_model_results$pred_pairs[[1]])) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  coord_fixed()
