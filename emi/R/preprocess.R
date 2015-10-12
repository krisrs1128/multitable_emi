
################################################################################
# functions for preprocessing before prediction
################################################################################

MergePreprocessOpts <- function(opts = list()) {
  default_opts <- list()
  default_opts$impute_median <- TRUE
  modifyList(default_opts, opts)
}

#' @title Preprocessing
#' @export
preprocess_data <- function(X, opts = list()) {
  opts <- MergePreprocessOpts(opts)
  if(opts$impute_median) {
    na_cols <- apply(X, 2, function(x) any(is.na(x)))
    X[, na_cols] <- apply(X[, na_cols, drop = F], 2, function(x) {
      x[is.na(x)] <- median(x, na.rm = T)
      return (x)
    })
  }
  return (X)
}
