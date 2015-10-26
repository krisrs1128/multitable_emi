
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
#' @title Get CV Folds for evaluation
#' @importFrom caret createFolds
#' @importFrom devtools use_data
#' @export
get_folds <- function(n = 188690, K = 5, n_rep = 20) {
  replicate(n_rep, createFolds(seq_len(nrow(train)), k = K), simplify = T)
}

# evaluation -------------------------------------------------------------------
evaluate_model <- function(data_list, train_fun, pred_fun) {

}
