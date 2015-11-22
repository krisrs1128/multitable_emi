
################################################################################
# Latent factors + covariates, learned through stochastic gradient descent
################################################################################

#' @title Regular gradient descent for latent factor model with covariates
#' @export
lf_gd_cov <- function(X, Z, k_factors = 3, lambas = c(1, 1, 1), n_iter = 100) {
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
      P[i, ] <- update_p(X[i, ], Z[i,, ], P[i, ], Q, beta, lambdas[1])
    }
    for(j in seq_len(n)) {
      Q[j, ] <- update_q(X[, j], Z[, j,], P, Q[j, ], beta, lambdas[2])
    }
    beta <- update_beta(X, P, Q, beta, lambdas[3])
  }

  return (list(P = P, Q = Q, beta = beta)
}
