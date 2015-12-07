
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
train <- train[1:5000, ]
words <- words %>% filter(User %in% train$User)
users <- users %>% filter(User %in% train$User)

# need to do imputation on words, because covariates optimization assumes full Z
# matrix is available
words_num <- as.matrix(words[, -c(1:2), with = F])
words_hat <- SVDImpute(words_num, k = 5, num.iters = 20)
words <- data.table(words[, 1:2, with = F], words_hat$x)

data_list <- list(train = train[1:4500, ],
                  words = words,
                  users = users)
newdata <- list(train = train[4501:5000, ],
                words = words,
                users = users)

trained_model <- svd_cov_train(data_list, list(n_iter = 20))
preds <- svd_cov_predict(trained_model, newdata)
plot(newdata$train$Rating, preds, asp = 1)

# we can disect the pieces of this prediction
pred_data <- prepare_pred_data(trained_model$data_list, newdata)
dim(pred_data$X)
dim(pred_data$Z)

impute_res <- svd_cov_impute(pred_data$X, pred_data$Z, trained_model$opts)
plot(impute_res$res$obj)
plot(impute_res$res$P)
plot(impute_res$res$beta)

data.frame(newdata$train$Rating,
           y_hat = postprocess_preds(impute_res$X_hat, newdata))
