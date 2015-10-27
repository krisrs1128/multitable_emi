
################################################################################
# Latent factor approach, fitted with SGD, described in online writeup,
# https://speakerd.s3.amazonaws.com/presentations/501d641214507500020491a3/MusicHackathonShanda.pdf
################################################################################

#' @title Merge default options for the latent factor model trained with SGD
#' @param opts A partially specified list of options, with the following
#' possible elements, \cr
#'   lambdas: A vector of regularization parameters. \cr
#'   eta: The learning rate for the stochastic gradient descent. \cr
#'   iter__max: The number of iterations to run the SGD. \cr
#'   d: The dimension of the latent factors. \cr
#' @return The opts list with defaults filled in.
#' @export
merge_lf_sgd_opts <- function(opts = list()) {
  default_opts <- list()
  default_opts$iter_max <- 1e6
  default_opts$eta <- 0.005
  default_opts$lambdas <- rep(5, 4)
  default_opts$d <- 5
  modifyList(default_opts, opts)
}

#' @title Train a latent factor SGD model
#' @param data_list A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'   train: A data.table giving the artist, track, user, rating, and time info. \cr
#'   words: A data.table giving word indicators for artist-user pairs. \cr
#'   users: A data.table giving survey results for each user. \cr
#' We don't ever actually use words or users in this model. This convention is
#' enforced for simplicity only.
#' @param train_opts A list with possible training options as specified in
#' merge_lf_sgd_opts().
#' @return A latent factor model, with elements
#'    mu: The global mean of R. \cr
#'    bu: Bias terms for each user. \cr
#'    bi: Bias terms for each item. \cr
#'    P: A user x d latent factor matrix for users \cr.
#'    Q: A item x d latent factor matrix for items.
#' @export
lf_sgd_train <- function(data_list, train_opts = list()) {
  train_opts <- merge_lf_sgd_opts(train_opts)
  return (list(train_opts = train_opts, train = data_list$train))
}

#' @title Make predictions from the lf-sgd model
#' @param model The output of a call to lf_sgd_train().
#' @param data_list The new test samples to make predictions over.
#' @return preds A vector of predictions on the new samples.
#' @importFrom data.table dcast
#' @export
lf_sgd_predict <- function(model, data_list) {
  # not allowed to look at current rating info, if it were available
  data_list$train$Rating <- NA
  all_data <- rbind(data_list$train, model$train)

  # shape ratings data
  R <- dcast(all_data, User ~ Track, value.var = "Rating")
  users <- unlist(R[, 1, with = F])
  R <- as.matrix(R[, -1, with = F])
  rownames(R) <- users

  # train model, with NAs in the positions to predict
  mu <- mean(R, na.rm = T)
  obs_ix <- which(!is.na(R), arr.ind = T)
  sgd_res <- lf_sgd(R = R - mu, obs_ix = obs_ix,
                    lambdas = model$train_opts$lambdas,
                    eta = model$train_opts$eta,
                    iter_max = model$train_opts$iter_max,
                    d = model$train_opts$d)

  # make predictions
  R_hat <- mu + sgd_res$bu %*% matrix(1, 1, ncol(R)) +
    matrix(1, nrow(R), 1) %*% t(sgd_res$bi) +
    sgd_res$P %*% t(sgd_res$Q)

  dimnames(R_hat) <- dimnames(R)
  preds <- R_hat[cbind(as.character(data_list$train$User),
                       as.character(data_list$train$Track))]
  return (preds)
}
