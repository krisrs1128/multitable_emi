
################################################################################
# Traditional matrix completion using SVD
################################################################################

#' @title Default options for SVD predictions
#' @param opts A partially specified list of options to use when performing the
#' SVD imputation. The currently supported options are
#'   $alpha The rescaling parameter to apply after taking the SVD [to account
#'    for the many 0's in the matrix on which we take the SVD]
#'   $k The rank of the imputation. Defaults to 10.
#'   $keep_cols The columns of the "tall" training data that will be "cast"
#'   $max_val The top value to truncate predictions at.
#'   $min_val THe bottom value to truncate predictions at.
#' @export
merge_svd_opts <- function(opts = list()) {
  default_opts <- list()
  default_opts$alpha <- NULL
  default_opts$k <- 10
  default_opts$keep_cols <- c("User", "Track")
  default_opts$max_val <- 100
  default_opts$min_val <- 0
  modifyList(default_opts, opts)
}

#' @title Cast a ratings matrix from tall to wide
#' @param mR A "tall" version of the usual ratings matrix. Each observed
#' user-movie pair is a row of this matrix. The user and movie ids are the first
#' two columns, the third is the rating associated with each observed pair.
#' @return R The "wide", standard form of the ratings matrix.
#' @importFrom data.table dcast.data.table as.data.table
#' @export
cast_ratings <- function(mR) {
  cast_fmla <- formula(paste0(colnames(mR)[1], "~", colnames(mR)[2]))
  R <- dcast.data.table(cast_fmla, data = as.data.table(mR))
  user_names <- as.character(R[[1]])
  movie_names <- as.character(colnames(R)[-1])
  R <- as.matrix(R[, 1:=NULL])
  dimnames(R) <- list(user_names, movie_names)
  R
}

#' @title Wrapper saving information in SVD model
#' @description This is mainly useful so that we can use the cv evaluation
#' functions. It just saves the data and options, actual imputation is done
#' at prediction time.
#' @param data_list A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'   train: A data.frame giving the artist, track, user, rating, and time info. \cr
#'   words: A matrix giving word indicators for artist-user pairs. \cr
#'   users: A matrix givin gsurvey results for each user. \cr
#' @param opts A list of svd imputation options. See merge_svd_opts.
#' @export
svd_train <- function(data_list, opts = list()) {
  opts <- merge_svd_opts(opts)
  mR <- data_list$train[, c(opts$keep_cols, "Rating"), with = F]
  list(mR = mR, opts = opts)
}

#' @title Perform core SVD imputation
#' @param R A users x ratings rating matrix, with NAs whenever a rating is
#' unknown.
#' @param k The rank of the SVD approximation to use.
#' @param alpha The rescaling factor after performing imputation, to account for
#' the many 0's in the originally SVD'd matrix.
#' @param min_val The minimum value to truncate predictions at.
#' @param max_val The maximum value to truncate predictions at.
#' @return pR_hat The version of R with imputations filled in.
#' @importFrom irlba irlba
#' @importFrom Matrix sparseMatrix
#' @export
svd_impute <- function(R, k, alpha, min_val, max_val) {
  # calculate deviations from user means
  user_means <- rowMeans(R, na.rm = T)
  user_means[user_means == 0] <- mean(R, na.rm = T) # when a user has no ratings, don't want mean to be 0
  user_means_mat <- matrix(user_means) %*% rep(1, ncol(R))
  R <- R - user_means_mat

  # automatic scaling factor, if not provided
  if(is.null(alpha)) {
    alpha <- prod(dim(R)) / sum(!is.na(R))
  }

  # perform svd approximation
  R[is.na(R)] <- 0
  svdR <- irlba(as(R, "sparseMatrix"), nu = 5, nv = 5)
  pR <- alpha * svdR$u %*% diag(svdR$d) %*% t(svdR$v)
  pR_hat <- pR + user_means_mat

  # truncate to possible range
  pR_hat[pR_hat < min_val] <- min_val
  pR_hat[pR_hat > max_val] <- max_val
  dimnames(pR_hat) <- dimnames(R)
  pR_hat
}

#' @title Matrix completion predictions using the SVD
#' @param trained_model The output of svd_train()
#' @param newdata A new data_list of the same form as data_list in the input to
#' svd_train()
#' @return y_hat Predictions y_hat for every new user-track pair in the newdata.
#' @importFrom data.table as.data.table melt.data.table setnames
#' @export
svd_predict <- function(trained_model, newdata) {
  # merge train and test, and reshape data to wide
  opts <- trained_model$opts
  mR_test <- data.table(newdata$train[, opts$keep_cols, with = F], Rating = 0)
  mR <- rbind(trained_model$mR, mR_test)
  R <- cast_ratings(mR)

  # get imputation results
  R_hat <- svd_impute(R, opts$k, opts$alpha, opts$min_val, opts$max_val)

  # filter down to the predictions on the test cases
  postprocess_preds(R_hat, newdata)
}
