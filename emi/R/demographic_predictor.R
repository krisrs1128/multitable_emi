
################################################################################
# Naive predictors using user information
################################################################################

#' @title Build predictor matrix incorporating user information
#' @importFrom plyr catcolwise
user_info_covariates <- function(train, users) {
  train_merged <- merge(train, users, by = "User", all.x = TRUE)
  train_factor <- catcolwise(as.factor)(train_merged)
  train_merged <- cbind(train_merged[, sapply(train_merged, is.numeric), with = F],
                        train_factor)
  X <- train_merged

  # don't want to use rating, and user would be too many factors to throw in
  X[, User:=NULL]
  X[,Rating:=NULL]
  X <- expand_factors(X)
  as.matrix(X)
}

#' @title Predictor with user information
#' @param train
#' @param test
#' @param users
#' @param trControl
#' @export
user_info_predictor <- function(train, test, users, trControl = NULL) {
  if(is.null(trControl)) {
    trControl <- trainControl(verbose = TRUE, number = 10)
  }
  x_train <- user_info_covariates(train, users)
  x_train <- preprocess_data(x_train)
  trained_model <- train(x = x_train, y = train$Rating,
                         method = "rpart", trControl = trControl)

  # make predictions on the test data
  x_test <- user_info_covariates(test, users)
  x_test <- preprocess_data(x_test)
  y_hat <- predict(trained_model, x_test)
  return(list(model = trained_model, y_hat = y_hat))
}
