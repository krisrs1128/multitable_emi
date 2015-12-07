
################################################################################
# Predictions using caret
################################################################################

#' @title Default options for using generic predicton algorithms in caret
#' @param opts A partially specified list of options to use when caret models.
#' The currently supported options are
#'   $process_opts: A list of processing options to pass the feature matrix
#'    through, via the preprocess_data() function.
#'   $method: The method to use from caret. See https://topepo.github.io/caret/modelList.html
#'   $train_control: The trControl argument to caret::train().
#' @return The original opts list with defaults filled in.
#' @export
merge_caret_opts <- function(opts = list()) {
  default_opts <- list()
  default_opts$method <- "glmnet"
  default_opts$process_opts <- list(add_na_level = TRUE, impute_svd = TRUE, scale_range = TRUE)
  default_opts$train_control <- trainControl(verbose = TRUE)
  modifyList(default_opts, opts)
}

#' @title Prepare features for generic caret training
#' @param data_list A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'   train: A data.frame giving the artist, track, user, rating, and time info. \cr
#'   words: A matrix giving word indicators for artist-user pairs. \cr
#'   users: A matrix givin gsurvey results for each user. \cr
#' @param process_opts A list of processing options to apply to the feature
#' matrix, see merge_process_opts().
#' @return A lsit with the following elements, \cr
#'   $X: A feature matrix whose rows correspond to the user - track pairs
#'   $y: A response vector fo ratings associated to the rows of X
#' in the train data set.
#' @export
prepare_features <- function(data_list, process_opts) {
  X <- Reduce(merge_intersect_X, data_list)

  # keep only columns allowed in R
  y <- X$Rating
  X <- X[, Rating:=NULL]
  X <- X[, User:=NULL]

  # impute NAs, and scale to range (when using default process_opts)
  X <- X %>%
    preprocess_data(opts$process_opts) %>%
    expand_factors %>%
    as.matrix()
  list(X = X, y = y)
}

#' @title Train a generic caret model on the available data sets
#' @param data_list A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'   train: A data.frame giving the artist, track, user, rating, and time info. \cr
#'   words: A matrix giving word indicators for artist-user pairs. \cr
#'   users: A matrix givin gsurvey results for each user. \cr
#' @param opts A list of options specifyign training. See merge_caret_opts() for
#' options and their defaults.
#' @return A list with the following elements,
#'   $opts A list of options passed into training.
#'   $model The full trained caret model output.
#' @examples
#' data(train)
#' data(users)
#' data(words)
#' data_list <- list(train = train[1:1000, ], users = users, words = words)
#' caret_train(data_list)
#' @export
caret_train <- function(data_list, opts = list()) {
  opts <- merge_caret_opts(opts)
  message("Preparing data for training.")
  train_data <- prepare_features(data_list, opts$process_opts)
  message("Training model.")
  model <- train(x = train_data$X, y = train_data$y, method = opts$method,
                 trControl = opts$train_control)
  list(opts = opts, model = model)
}

#' @title Make predictions using a trained caret model
#' @param trained_model The output of caret_train.
#' @param newdata A data_list of the same form as that input to caret_train.
#' @return y_hat for the new data.
#' @export
caret_predict <- function(trained_model, newdata) {
  new_train_data <- prepare_features(newdata, trained_model$opts$process_opts)
  predict(trained_model$model, newdata = new_train_data$X)
}
