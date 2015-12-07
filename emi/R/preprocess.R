
################################################################################
# functions for preprocessing before prediction
################################################################################

#' @title Merge default prprocessing options
#' @export
merge_preprocess_opts <- function(opts = list()) {
  default_opts <- list()
  default_opts$add_na_level <- FALSE
  default_opts$impute_median <- FALSE
  default_opts$imupute_svd <- FALSE
  default_opts$scale_range <- FALSE
  modifyList(default_opts, opts)
}

#' @title For factor columns with NAs, convert NA to its own level
#' @export
add_na_level <- function(X) {
  factor_cols <- sapply(X, function(x) class(x) %in% c("factor", "character"))
  X[, which(factor_cols)] <- catcolwise(addNA)(X)
  X
}

#' @title Impute NAs in numeric columns of a data.frame with medians
#' @export
impute_median <- function(X) {
  numeric_cols <- sapply(X, function(x) class(x) %in% c("numeric", "integer"))
  X[, which(numeric_cols)] <- apply(X[, numeric_cols, with = F], 2,
                                    function(x) {
                                      x[is.na(x)] <- median(x, na.rm = T)
                                      x
                                    })
  X
}

#' @title Impute NAs in numeric columns of a data.frame with the SVD
#' @importFrom imputation SVDImpute
#' @export
impute_svd <- function(X, k = 10, verbose = FALSE) {
  numeric_cols <- sapply(X, function(x) class(x) %in% c("numeric", "integer"))
  impute_res <- SVDImpute(as.matrix(X[, numeric_cols, with = F]), k, verbose = verbose)
  X[, which(numeric_cols)] <- data.table(impute_res$x)
  X
}

#' @title Scale numeric columns to [0, 1] range
#' @export
scale_range <- function(X) {
  scale_fun <- function(x) {
    if(max(x, na.rm = T) > min(x, na.rm = T)) {
      return ((x-min(x, na.rm = T))/(max(x, na.rm = T)-min(x, na.rm = T)))
    } else {
      return (x)
    }
  }
  numeric_cols <- sapply(X, function(x) class(x) == "numeric")
  X[, which(numeric_cols)] <- numcolwise(scale_fun)(X)
  X
}

#' @title Preprocessing
#' @param X A data.frame or data.table to preprocess.
#' @param opts A (potentially partially specified) list of preprocessing
#' options. See merge_preprocess_opts for possible options, and their defaults.
#' @return X The processed version of X. Should have same dimensions as input.
#' @export
preprocess_data <- function(X, opts = list()) {
  opts <- merge_preprocess_opts(opts)
  if(opts$add_na_level) {
    X <- add_na_level(X)
  }
  if(opts$impute_median) {
    X <- impute_median(X)
  }
  if(opts$impute_svd) {
    X <- impute_svd(X)
  }
  if(opts$scale_range) {
    X <- scale_range(X)
  }
  X
}
