
################################################################################
# Experiment using stochastic gradient descent with covariates
################################################################################

## ---- libraries ----
library("data.table")
library("emi")
library("imputation")
library("plyr")
library("dplyr")

## ---- evaluate ----
data(train)
data(words)
data(users)

# working on subset of data
train <- train[1:1000, ]
words <- words %>% filter(User %in% train$User)
users <- users %>% filter(User %in% train$User)

# need to do imputation on words, because covariates optimization assumes full Z
# matrix is available
words_num <- as.matrix(words[, -c(1:2), with = F])
words_hat <- SVDImpute(words_num, k = 5, num.iters = 20, verbose = FALSE)
words <- data.table(words[, 1:2, with = F], words_hat$x)

data_list <- list(train = train[1:900, ],
                  words = words,
                  users = users)
newdata <- list(train = train[901:1000, ],
                words = words,
                users = users)

trained_model <- svd_cov_train(data_list, list(n_iter = 100))
preds <- svd_cov_predict(trained_model, newdata)
plot(newdata$train$Rating, preds, asp = 1)

# we can dissect the pieces of this prediction
pred_data <- prepare_pred_data(trained_model$data_list, newdata)
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

ggplot(data.frame(svd_cov_results$pred_pairs[[1]])) +
  geom_point(aes(x = y, y = y_hat), alpha = 0.1) +
  geom_abline(aes(slope = 1, intercept = 0), col = "red") +
  coord_fixed()
