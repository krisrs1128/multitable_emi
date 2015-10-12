
################################################################################
# Traditional matrix completion usign SVD
################################################################################

#' @importFrom data.table dcast.data.table
#' @importFrom rARPACK svds
#' @export
svd_predictor <- function(train, test, r = 5, alpha = 0.7) {
  # reshape to be user x artist matrix
  train_mat <- dcast.data.table(train, User ~ Artist,
                                value.var = "Rating", fun.aggregate = mean,
                                drop = FALSE)
  rownames(train_mat) <- train_mat$User
  train_mat <- train_mat[, User:=NULL]

  # compute deviations from user means
  Mt <- scale(t(train_mat), scale = FALSE)
  M <- t(Mt)
  user_means <- attr(Mt, "scaled:center")
  M[is.na(M)] <- 0
  rownames(M) <- rownames(train_mat)

  # build predictions for deviations [using ARPACK, faster than usual SVD]
  M <- as(M, "dgCMatrix")
  svdM <- svds(M, k = r)

  scores <- svdM$u[, 1:r] %*% diag(svdM$d[1:r])
  v <- svdM$v[, 1:r]
  M_hat <- scores %*% t(v)
  dimnames(M_hat) <- dimnames(M)

  # many users in the training data are not in the test for these users, just
  # use the global mean.
  new_users <- setdiff(as.character(unique(test$User)), rownames(M_hat))
  M_new_users <- matrix(0, nrow = length(new_users), ncol = ncol(M_hat),
                        dimnames = list(new_users, colnames(M_hat)))
  M_hat <- rbind(M_hat, M_new_users)
  new_means <- rep(mean(train$Rating), length(new_users))
  user_means <- c(user_means, new_means)
  names(user_means) <- c(rownames(train_mat), new_users)

  # make predictions using means for the users + deviations coming from SVD
  y_hat <- M_hat[as.matrix(test[, c("User", "Artist"), with = F])] +
    (1 - alpha) * user_means[as.character(test$User)] + alpha * mean(user_means)
  return (list(y_hat = y_hat, svd = svdM, M_hat = M_hat,
               user_means = user_means))
}
