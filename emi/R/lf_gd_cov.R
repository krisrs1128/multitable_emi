
################################################################################
# Latent factors + covariates, learned through stochastic gradient descent
################################################################################

#' @title Regular gradient descent for latent factor model with covariates
#' @param X The ratings matrix. Unobserved entries must be marked NA. Users
#' must be along rows, and tracks must be along columns.
#' @param Z The covariates associated with each pair. This must be shaped as an
#' array with users along rows, tracks along columns, and features along
#' slices.
#' @param k_factors The number of latent factors for the problem.
#' @param lambdas The regularization parameters for P, Q, and beta, respectively.
#' @param n_iter The number of gradient descent iterations to run.
#' @param gamma The step-size in the gradient descent.
#' @return A list with the following elements, \cr
#'   $P The learned user latent factors. \cr
#'   $Q The learned track latent factors.
#'   $beta The learned regression coefficients.
#' @export
lf_gd_cov <- function(X, Z, k_factors = 3, lambdas = rep(1, 3), n_iter = 5,
                      gamma_pq = 0.1, gamma_beta = 0.0001) {
  m <- nrow(X)
  n <- ncol(X)
  l <- dim(Z)[3]

  # initialize results
  P <- matrix(rnorm(m * k_factors), m, k_factors)
  Q <- matrix(rnorm(n * k_factors), n, k_factors)
  #beta <- rnorm(l)
  beta <- rep(0, l)
  objs <- vector(length = n_iter)

  for(cur_iter in seq_len(n_iter)) {
    cat(sprintf("iteration %g \n", cur_iter))

   # update user factors, movie factors, the regression coefficients
    for(i in seq_len(m)) {
      P[i, ] <- update_factor(X[i, ], Z[i,, ], P[i, ], Q, beta, lambdas[1], gamma_pq)
    }
    for(j in seq_len(n)) {
      Q[j, ] <- update_factor(X[, j], Z[, j, ], Q[j, ], P, beta, lambdas[2], gamma_pq)
    }
    beta <- update_beta(X, Z, P, Q, beta, lambdas[3], gamma_beta)
    objs[cur_iter] <- objective_fun(X, Z, P, Q, beta, lambdas)
    cat(sprintf("Objective: %g \n", objs[cur_iter]))
  }

  return (list(P = P, Q = Q, beta = beta, objs = objs))
}

#' @title Gradient step for Latent Factors
#' @param x Either a single row or single column of X, the ratings matrix.
#' @param z A single matrix from the covariates array, associated with the
#' either a single row or column of X.
#' @param p_i The latent factors associated with a single user or track.
#' @param Q The latent factors associated with either all tracks or all users,
#' depending on the type of latent factor in p_i.
#' @param beta The learned regression coefficients.
#' @param lambda The regularization parameter for the current latent factor.
#' @param gamma The step-size in the gradient descent.
#' @return The update latent factor for the current user / track.
#' @export
update_factor <- function(x, z, p_i, Q, beta, lambda, gamma) {
  obs_ix <- !is.na(x)
  Q_obs <- Q[obs_ix, , drop = F]
  resid <- as.numeric(x[obs_ix] - Q_obs %*% p_i)
  p_i_grad <- 2 * as.numeric(lambda * p_i - t(Q_obs) %*% resid)
  p_i - gamma * p_i_grad
}

#' @title Gradient step for regression coefficient
#' @param X The ratings matrix. Unobserved entries must be marked NA. Users
#' must be along rows, and tracks must be along columns.
#' @param P The learned user latent factors.
#' @param Q The learned track latent factors.
#' @param beta The learned regression coefficients.
#' @param lambda The regularization parameter for beta.
#' @param gamma The step-size in the gradient descent.
#' @return The update regression coefficients.
#' @export
update_beta <- function(X, Z, P, Q, beta, lambda, gamma) {
  obs_ix <- !is.na(X)
  Zbeta <- apply(Z, 2, function(x) x %*% beta)
  resid <- X - P %*% t(Q) - Zbeta
  beta_grad <- vector(length = length(beta))
  for(l in seq_along(beta_grad)) {
    beta_grad[l] <- sum(resid[obs_ix] * Z[,, l][obs_ix])
  }
  beta_grad <- 2 * lambda * beta - 2 * beta_grad
  beta - gamma * beta_grad
}

#' @title Objective function to minimize
#' @description This is the sum of squared error at observed entries,
#' between the ratings matrix and its approximation by latent factors and
#' covariates.
#' @param X The ratings matrix. Unobserved entries must be marked NA. Users
#' must be along rows, and tracks must be along columns.
#' @param Z The covariates associated with each pair. This must be shaped as an
#' array with users along rows, tracks along columns, and features along
#' slices.
#' @param P The learned user latent factors.
#' @param Q The learned track latent factors.
#' @param beta The learned regression coefficients.
#' @param lambda  The regularization parameters for P, Q, and beta, respectively.
#' @return The value of the objective function given the current parameter
#' values.
#' @export
objective_fun <- function(X, Z, P, Q, beta, lambdas) {
  obs_ix <- !is.na(X)
  Zbeta <- apply(Z, 2, function(x) x %*% beta)
  resid <- X - P %*% t(Q) - Zbeta
  sum(resid[obs_ix] ^ 2) +  lambdas %*%  c(sum(P ^ 2), sum(Q ^ 2), sum(beta ^ 2))
}
