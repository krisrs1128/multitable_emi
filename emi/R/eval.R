
################################################################################
# Functions for evaluating performance
################################################################################

#' @title Calculate the root mean squared error
#' @export
RMSE <- function(y, y_hat) {
  sqrt(mean((y - y_hat) ^ 2))
}

#' @title Get CV Folds for evaluation
#' @importFrom caret createFolds
#' @importFrom devtools use_data
#' @export
get_folds <- function(data = NULL, file = NULL, n = 188690, K = 5, n_rep = 20) {
  if(is.null(file)) {
    test_ix <- replicate(n_rep, createFolds(seq_len(nrow(train)), k = K), simplify = T)
    use_data(test_ix, overwrite = TRUE)
  } else {
    suppressWarnings(data(get(file)))
  }
  return (test_ix)
}
