
################################################################################
#  Matrix completion using regularized SVD + covariate information.
################################################################################

# utils ------------------------------------------------------------------------
#' @title Get shared column names
#' @export
intersect_names <- function(X, Y) {
  intersect(colnames(X), colnames(Y))
}

#' @title Wrapper for merging by all shared columns
#' @export
merge_intersect <- function(X, Y) {
  merge(X, Y, by = intersect_names(X, Y), all = TRUE, sort = FALSE)
}

#' @title Wrapper for merging by all shared columns, but only keeping X
#' @export
merge_intersect_X <- function(X, Y) {
  merge(X, Y, by = intersect_names(X, Y), all.x = TRUE, sort = FALSE)
}

# compilation ------------------------------------------------------------------
# Just to make sure c++ code compiles correctly
#' @useDynLib emi
#' @importFrom Rcpp sourceCpp
NULL

# prepare-covariates -----------------------------------------------------------
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
#' @importFrom magrittr %>%
#' @importFrom data.table melt
#' @importFrom reshape2 acast
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
#' @description We would like to include users' descriptions of their personal tastes
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
  Z <- Z[, as.character(artist_track_vec), ]
  colnames(Z) <- track_names
  Z
}

#' @title Wrapper to do all the covariates preparation.
#' @param users A matrix whose first column is a user id, and whose remaining
#' columns give descriptions of musical preference.
#' @param words The words data.table in directly from the EMI challenge.
#' @param x_names An optional list of row names [character vector] to match
#' against. This can be used to ensure consistency in the rows across tables.
#' If a user appears in x-names but not in mZ, a new row of all NAs included.
#' By default, the cast version of mZ is returned without any reordering of
#' users.
#' @param track_names The columns of the ratings matrix on which we are doing
#' matrix completion. After applying this function, the column names of Z will
#' be this vector.
#' @param artist_track_map A two column data.table mapping artists (first col)
#' to associated tracks (second col).
#' @return A covariates matrix with dimensions user x track x question. If no
#' response was recorded, the array element is filled with an NA.
#' @importFrom data.table setcolorder
#' @importFrom magrittr %>%
#' @export
prepare_covariates <- function(users, words, x_names, track_names,
                               artist_track_map) {
  mZ_words <- words %>%
    setcolorder(c(2, 1, 3:ncol(words)))
  cast_covariates(mZ_words, x_names) %>%
    merge_users_covariates(users) %>%
    expand_by_track(track_names, artist_track_map)
}

# prediction-data-manipulation -------------------------------------------------
#' @title Merge train and test data, for imputation
#' @param train_list A data_list of the form
#'  $train: A data.frame giving the artist, track, user, rating, and time info. \cr
#'  $words: A matrix giving word indicators for artist-user pairs. \cr
#'  $users: A matrix givin gsurvey results for each user. \cr
#' @param newdata A test data_list, of the same form as train_list.
#' @return A list with the following elements, \cr
#'  $X: A user x track rating matrix
#'  $Z: A user x track x question covariates matrix
#' @importFrom imputation SVDImpute
#' @export
prepare_pred_data <- function(train_list, newdata) {
  # merge training with new data (with ratings as NAs)
  newdata$train$Rating <- NA
  train <- merge_intersect(train_list$train, newdata$train)
  users <- merge_intersect(train_list$users, newdata$users)
  words <- merge_intersect(train_list$words, newdata$words)

  # prepare the data
  artist_track_map <- unique(train[, c("Artist", "Track"), with = F])
  X <- cast_ratings(train[, c("User", "Track", "Rating"), with = F])
  Z <- prepare_covariates(users, words, rownames(X), colnames(X),
                          artist_track_map)

  # preprocess Z, to remove NAs [which aren't allowed in the optimization]
  for(j in seq_len(ncol(Z))) {
    Z[, j, ] <- SVDImpute(Z[, j, ], k = 3, num.iters = 3, verbose = F)$x %>% scale_range()
  }
  list(X = X, Z = Z)
}

#' @title Extract predictions from X_hat matrix
#' @param X_hat A user x track matrix with predictions.
#' @param newdata A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'  $train: A data.frame giving the artist, track, user, and time info. \cr
#'  $words: A matrix giving word indicators for artist-user pairs. \cr
#'  $users: A matrix givin gsurvey results for each user. \cr
#' @return A vector of ratings corresponding to the user x track pairs in $train
#' from the newdata.
#' @importFrom data.table melt
#' @importFrom magrittr %>%
#' @importFrom data.table setnames
#' @export
postprocess_preds <- function(X_hat, newdata) {
  mX_hat <- melt(X_hat) %>%
    setnames(c("User", "Track", "Rating"))
  newdata$train <- newdata$train[, setdiff(colnames(newdata$train), "Rating"), with = F]
  newdata$train$User <- as.integer(as.character(newdata$train$User))
  newdata$train$Track <- as.integer(as.character(newdata$train$Track))
  mX_hat <- merge_intersect_X(newdata$train, mX_hat)
  mX_hat$Rating
}

# model-fitting ----------------------------------------------------------------
#' @title Default options for SVD with covariates
#' @param opts A partially specified list of options to use when performing the
#' SVD imputation. The currently supported options are
#'  $batch_samples: The proportion of samples to use when evaluating gradients
#'   for each user's / track's latent factors. Defaults to 1, so it is usual
#'   gradient rather than stochastic gradient descent.
#'  $batch_factors: The proportion of factors to update during each iteration.
#'   Defaults to 1, so it is usual  gradient rather than stochastic gradient
#'   descent.
#'  $gamma_pq: The learning rate for the users and tracks factors.
#'  $gamma_beta: The learning rate for the regression covariates.
#'  $k_factors: How many latent factors should we use for the users and tracks?
#'  $lambdas: Regularization parameters for user factors, track factors, and
#'   covariates regression coefficients, respectively.
#'  $max_val The top value to truncate predictions at.
#'  $min_val THe bottom value to truncate predictions at.
#'  $n_iter: Number of iterations to run the (stochastic) gradient descent)
#' @export
merge_svd_cov_opts <- function(opts = list()) {
  default_opts <- list()
  default_opts$batch_samples <- 1
  default_opts$batch_factors <- 1
  default_opts$gamma_pq <- 1e-5
  default_opts$gamma_beta <- 1e-8
  default_opts$k_factors <- 5
  default_opts$lambdas <- c(10, 10, 10)
  default_opts$max_val <- 100
  default_opts$min_val <- 0
  default_opts$n_iter <- 5
  default_opts$verbose <- FALSE
  modifyList(default_opts, opts)
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
  list(data_list = data_list, opts = opts)
}

#' @title Perform SVD with Covariates imputation
#' @param X A users x tracks matrix of ratings.
#' @param Z A users x tracks x question covariates matrix.
#' @param opts A list of options to use in prediction.
#' @return A list with the following elements: \cr
#'  $X_hat A users x tracks matrix of predicted ratings. \cr
#'  $res The full output of the SVD imputation with covariates, svd_cov, in the
#'   src directory. /\cr
#' @export
svd_cov_impute <- function(X, Z, opts) {
  # call underlying C++ routine, after row centering
  X_means <- rowMeans(X, na.rm = TRUE)
  X_means[is.na(X_means)] <- mean(X, na.rm = T) # some users never appear in training
  X0 <- sweep(X, 1, X_means, "-")
  res <- svd_cov(X0, Z, opts$k_factors, opts$lambdas, opts$n_iter,
                 opts$batch_samples, opts$batch_factors, opts$gamma_pq,
                 opts$gamma_beta, opts$verbose)

  # get the X_hat matrix, and add back user means
  Zbeta <- apply(Z, 2, function(x) x %*% res$beta)
  X_hat <- res$P %*% t(res$Q) + Zbeta
  X_hat <- sweep(X_hat, 1, X_means, "+")
  X_hat <- pmin(pmax(X_hat, opts$min_val), opts$max_val)
  dimnames(X_hat) <- dimnames(X)
  list(X_hat = X_hat, res = res)
}

#' @title Matrix completion predictions using the SVD
#' @param trained_model The output of svd_train()
#' @param newdata A new data_list of the same form as data_list in the input to
#' svd_cov_train() [except it doesn't need to have a "Rating" column"]
#' @return y_hat Predictions y_hat for every new user-track pair in the newdata.
#' @examples
#' data(train)
#' data(users)
#' data(words)
#' data_list <- list(train = train[1:100, ], words = words, users = users)
#' newdata <- list(train = train[101:200, -5, with = F], words = words, users = users)
#' trained_model <- svd_cov_train(data_list, list(n_iter = 5))
#' svd_cov_predict(trained_model, newdata) # all na's, because we need to do imputation on Z
#' @export
svd_cov_predict <- function(trained_model, newdata) {
  message("Preparing data for imputation.")
  pred_data <- prepare_pred_data(trained_model$data_list, newdata)

  message("Training model.")
  impute_res <- svd_cov_impute(pred_data$X, pred_data$Z, trained_model$opts)

  message("Extracting predictions from imputed matrix.")
  postprocess_preds(impute_res$X_hat, newdata)
}
