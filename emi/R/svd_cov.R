
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

#' @title Merge in user specific covariates
#' @param We would like to include users' descriptions of their personal tastes
#' in this regression problem. However, our regression array has the form
#' user x artist x question, so to merge it in, we need to copy the same
#' responses for every artist.
#' @param Z An array with dimensions user x artist x question response.
#' @param users A matrix whose first column is a user id, and whose remaining
#' columns give descriptions of musical preference.
#' @return Z_users The matrix Z with the questions dimension increased to
#' account for the additional covariates from the users matrix. If a user in Z
#' did not appear in the users matrix, the new question is filled in with all
#' NAs.
#' @importFrom abind abind
#' @export
merge_users_covariates <- function(Z, users) {
  # filter to the users in Z, and convert to matrix
  user_names <- unlist(users[, 1, with = F])
  question_names <- colnames(users)[-1]
  users <- as.matrix(users[match(rownames(Z), user_names), -1, with = F])

  # bind in covariates to the Z matrix
  users <- array(users, dim = c(dim(users), 1))
  colnames(users) <- question_names
  users <- aperm(users, c(1, 3, 2)) # make into users x artist x covariate
  users <- users[,rep(1, ncol(Z)), ]
  abind(Z, users, along = 3)
}

#' @title Copy covariates associated with artists to match tracks
#' @description Unfortunately, the covariates in the EMI data are only
#' associated with individual artists, not tracks, which is the level at which
#' we are doing matrix completion. So, we need to expand the covariates matrix Z
#' from the artist level to the track level, using the artist_track_map.
#' @param Z A user x artist x question covariates matrix.
#' @param track_names The columns of the ratings matrix on which we are doing
#' matrix completion. After applying this function, the column names of Z will
#' be this vector.
#' @param artist_track_map A two column data.table mapping artists (first col)
#' to associated tracks (second col).
#' @return Z The same matrix Z as before, but with responses for individual
#' artists repeated to correspond with the track level labels.
#' @export
expand_by_track <- function(Z, track_names, artist_track_map) {
  artist_track_vec <- setNames(unlist(artist_track_map[, 1, with = F]),
                               unlist(artist_track_map[, 2, with = F]))
  artist_track_vec <- artist_track_vec[track_names]
  Z <- Z[, artist_track_vec, ]
  colnames(Z) <- track_names
  Z
}

#' @title Wrapper for saving information in SVD with covariates model
#' @description This is mainly useful so that we can use the cv evaluation
#' functions. It just saves the data and options, actual imputation is done
#' at prediction time.
#' @param data_list A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'  $train: A data.frame giving the artist, track, user, rating, and time info. \cr
#'  $words: A matrix giving word indicators for artist-user pairs. \cr
#'  $users: A matrix givin gsurvey results for each user. \cr
#' @param opts A list of svd with covariates imputation options. See
#' merge_svd_cov_opts.
#' @return A list that can be used for imputation with covariates. It contains
#' the following elements,
#'  $X A users x tracks matrix of ratings.
#'  $Z A users x tracks x question covariates matrix.
#'  $opts A list of options to use in prediction.
#' @examples
#' data(train)
#' data(users)
#' data(words)
#' data_list <- list(train = train[1:1000, ],
#'                   words = words,
#'                   users = users)
#' svd_cov_train(data_list)
#' @export
svd_cov_train <- function(data_list, opts = list()) {
  opts <- merge_svd_cov_opts(opts)
  mZ_words <- data_list$words %>%
    setcolorder(c(2, 1, 3:ncol(mZ_words)))
  artist_track_map <- unique(data_list$train[, c("Artist", "Track"), with = F])

  X <- cast_ratings(data_list$train[, c("User", "Track", "Rating"), with = F])
  Z <- cast_covariates(mZ_words, rownames(X)) %>%
    merge_users_covariates(data_list$users) %>%
    expand_by_track(colnames(X), artist_track_map)

  list(X = X, Z = Z, opts = opts)
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
