
################################################################################
# Latent factors + covariates, learned through stochastic gradient descent
################################################################################

#' @title Regular gradient descent for latent factor model with covariates
#' @export
lf_gd_cov <- function(X, Z, k_factors = 3, lambas = c(1, 1, 1), n_iter = 100,
                      gamma = 0.1) {
  m <- nrow(X)
  n <- ncol(X)
  l <- dim(Z)[3]

  # initialize results
  P <- matrix(0, m, k_factors)
  Q <- matrix(0, n, k_factors)
  beta <- vector(0, length = l)

  for(cur_iter in seq_len(n_iter)) {
    # update user factors, movie factors, the regression coefficients
    for(i in seq_len(m)) {
      P[i, ] <- update_p(X[i, ], Z[i,, ], P[i, ], Q, beta, lambdas[1], gamma)
    }
    for(j in seq_len(n)) {
      Q[j, ] <- update_q(X[, j], Z[, j,], P, Q[j, ], beta, lambdas[2], gamma)
    }
    beta <- update_beta(X, P, Q, beta, lambdas[3])
  }

  return (list(P = P, Q = Q, beta = beta))
}

update_factor <- function(x, z, update_factor, other_factor, lambda, gamma) {
  obs_ix <- !is.na(x)
  other_factor_obs <- other_factor[obs_ix, ]
  resid <- x[obs_ix] - t(other_factor_obs) %*% udpate_factor - z[other_factor_obs, ] %*% beta
  (1 - lambda) * update_factor + lambda * gamma * t(other_factor_obs) %*% resid
}

update_q <- function(x_j, z_j, P, q_j, beta, lambda, gamma) {
  update_factor(x_j, z_j, q_j, P, beta, lambda, gamma)
}

udpate_p <- function(x_i, z_i, p_i, Q, beta, lambda, gamma) {
  update_factor(x_i, z_j, p_i, Q, beta, lambda, gamma)
}


update_beta <- function(X, P, Q, beta, lambda, gamma) {
  obs_ix <- is.na(X)
  resid <- X - P %*% t(Q) - Z %*% beta
  beta_grad <- vector(length = length(beta))
  for(l in seq_len(beta_grad)) {
    beta_grad[l] <- sum(resid[obs_ix] * Z[,, l][obs_ix])
  }
  (1 - lambda) * beta + lambda * gamma * beta_grad
}
