
################################################################################
# Functions for evaluating performance
################################################################################

# metrics ----------------------------------------------------------------------
#' @title Calculate the root mean squared error
#' @export
RMSE <- function(y, y_hat) {
  sqrt(mean((y - y_hat) ^ 2))
}

# get-folds --------------------------------------------------------------------
#' @title Create folds of contiguous indices
#' @param n How many indices have to be split into folds?
#' @param K How many folds should be created?
#' @export
contiguous_folds <- function(n, K) {
  v <- sort(rep(1:K, length.out = n))
  res <- vector(mode = "list", length = K)
  for(k in seq_len(K)) {
    res[[k]] <- which(v == k)
  }
  res
}

#' @title Get CV Folds for evaluation
#' @description Wrapper for caret::createFolds with some slight generalizations.
#' @param n How many indices have to be split into folds?
#' @param K How many folds should be created?
#' @param n_rep How many independent sets of cross-validation fold indices
#' should be returned?
#' @param contiguous Should the CV folds be of contiguous groups of indices?
#' @importFrom caret createFolds
#' @importFrom devtools use_data
#' @export
get_folds <- function(n = 188690, K = 10, n_rep = 1, contiguous = FALSE) {
  if(contiguous) {
    replicate(n_rep, contiguous_folds(n, K), simplify = T)
  } else {
    replicate(n_rep, createFolds(seq_len(n), k = K), simplify = T)
  }
}

# evaluation -------------------------------------------------------------------
#' @title Generic Evaluation Function
#' @param data_list A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'   train: A data.frame giving the artist, track, user, rating, and time info. \cr
#'   words: A matrix giving word indicators for artist-user pairs. \cr
#'   users: A matrix givin gsurvey results for each user. \cr
#' @param train_fun A function taking arguments data_list and train_opts, and
#' returning a trained model that can be used for prediction.
#' @param train_opts A list giving options for how to train the model in
#' train_fun.
#' @param pred_fun A function that given the output of train_fun and a new test
#' version of data_list returns predictions for each row of the test data.
#' @param K The number of cv-folds in cross-validation.
#' @param n_rep The number of cv replicates to perform.
#' @param contiguous Should the CV folds be of contiguous groups of indices?
#' @return A list with the following elements, \cr
#'    mean_rmse: The mean of the RMSEs calculated over reps and folds.
#'    err: A K x n_rep matrix, whose kj^th element gives the error when holding
#'    out the k^th fold in the j^th rep.
#'    pred_pairs: A K x n_rep matrix whose kj^th element is a list with the true
#'    and predicted ratings.
#' @export
evaluate <- function(data_list, train_fun, train_opts, pred_fun,
                     K = 10, n_rep = 1, contiguous = FALSE) {
  # extract cv folds
  folds_ix <- get_folds(nrow(data_list$train), K, n_rep, contiguous)
  errs <- matrix(NA, nrow(folds_ix), ncol(folds_ix))
  pred_pairs <- matrix(list(), nrow(folds_ix), ncol(folds_ix))

  # loop over reps and fold indices
  for(cur_rep in seq_len(n_rep)) {
    for(cur_k in seq_len(K)) {
      cat(sprintf("Starting fold %g of rep %g \n", cur_k, cur_rep))

      # get current training and testing data
      train_ix <- unlist(folds_ix[-cur_k, cur_rep])
      train_data_list <- data_list
      test_data_list <- data_list
      train_data_list$train <- train_data_list$train[train_ix, ]
      test_data_list$train <- test_data_list$train[-train_ix, ]

      # train and evaluate model
      cur_model <- train_fun(train_data_list, train_opts)
      cur_preds <- pred_fun(cur_model, test_data_list)
      truth <- test_data_list$train$Rating
      errs[cur_k, cur_rep] <- RMSE(cur_preds, truth)
      pred_pairs[[cur_k, cur_rep]] <- data.frame(y = truth, y_hat = cur_preds)
    }
  }
  return (list(mean_rmse = mean(errs), errs = errs, pred_pairs = pred_pairs))
}
