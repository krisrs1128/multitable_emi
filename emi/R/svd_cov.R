
################################################################################
#  Matrix completion using regularized SVD + covariate information.
################################################################################

# Just to make sure c++ code compiles correctly
#' @useDynLib emi
#' @importFrom Rcpp sourceCpp
NULL

#' @title Default options for SVD with covariates
#' @param opts A partially specified list of options to use when performing the
#' SVD imputation. The currently supported options are
#' @export
merge_svd_cov_opts <- function(opts = list()) {
  default_opts <- list()
  modifyList(default_opts, opts)
}

#' @title Create an array representing covariate information
#' @description All covariate information is provided in "long" form. That is,
#' different user-artist-question pairs are rows of the "words" matrix. Instead,
#' we need an array whose elements z_ijk give the response that user i gave
#' about artist j for question k.
#' @param mZ A "long" matrix that can be automatically cast by data.table.
#' So, the first two columns should be row, column, and slice ids, the fourth
#' should be the covariate value to fill in.\
#' @param x_names An optional list of row names [character vector] to match
#' against. This can be used to ensure consistency in the rows across tables.
#' If a user appears in x-names but not in mZ, a new row of all NAs included.
#' By default, the cast version of mZ is returned without any reordering of
#' users.
#' @return An array Z with the structure described above.
#' @param mZ
#' @export
cast_covariates <- function(mZ, x_names = NULL) {
  mZ <- as.data.table(mZ)
  z_vars <- colnames(mZ)
  if(is.null(x_names)) {
    x_names <- unique(mZ[, 1, with = F])
  }

  # filter
  z_names <- unlist(mZ[, z_vars[1], with = F], use.names = F)
  mZ <- mZ[z_names %in% x_names, ]

  # cast
  Z <- mZ %>%
    melt(id.vars = z_vars[1:2], variable.name = "question") %>%
    acast(formula(paste0(z_vars[1], "~", z_vars[2], "~ question")))

  # match to x_names
  Z[match(x_names, rownames(Z)),, ]
}

#' @title Wrapper for saving information in SVD with covariates model
#' @description This is mainly useful so that we can use the cv evaluation
#' functions. It just saves the data and options, actual imputation is done
#' at prediction time.
#' @param data_list A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'   train: A data.frame giving the artist, track, user, rating, and time info. \cr
#'   words: A matrix giving word indicators for artist-user pairs. \cr
#'   users: A matrix givin gsurvey results for each user. \cr
#' @param opts A list of svd with covariates imputation options. See
#' merge_svd_cov_opts.
#' @export
svd_cov_train <- function(data_list, opts = list()) {
  opts <- merge_svd_cov_opts(opts)
  setcolorder(mZ, c(2, 1, 3:ncol(mZ)))
  stop("not implemented yet")

}

# do the actual imputation
svd_cov_impute <- function(R, k, ...) {
  stop("not implemented yet")
}

#' @title Matrix completion predictions using the SVD
#' @param trained_model The output of svd_train()
#' @param newdata A new data_list of the same form as data_list in the input to
#' svd_train()
#' @return y_hat Predictions y_hat for every new user-track pair in the newdata.
#' @export
svd_cov_predict <- function(trained_model, newdata) {
  stop("not implemented yet")
}
