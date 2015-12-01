
////////////////////////////////////////////////////////////////////////////////
// Latent factor modeling, using gradient descent with covariates
////////////////////////////////////////////////////////////////////////////////

#include <RcppArmadillo.h>
#include <math.h>
#include <stdio.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

// utils -----------------------------------------------------------------------
// @title Multiply a arma::cube by a vector, to get a matrix
arma::mat cube_multiply(arma::cube Z, arma::vec beta) {
  arma::mat Zbeta = arma::zeros(Z.n_rows, Z.n_cols);
  for(int j = 0; j <  Z.n_cols; j++) {
    arma::mat zj = Z(arma::span(), arma::span(j), arma::span());
    Zbeta.col(j) = zj * beta;
  }
  return Zbeta;
}

// gradients -------------------------------------------------------------------
//' @title Gradient step for Latent Factors
//' @param x Either a single row or single column of X, the ratings matrix.
//' @param z A single matrix from the covariates array, associated with the
//' either a single row or column of X.
//' @param p_i The latent factors associated with a single user or track.
//' @param Q The latent factors associated with either all tracks or all users,
//' depending on the type of latent factor in p_i.
//' @param beta The learned regression coefficients.
//' @param lambda The regularization parameter for the current latent factor.
//' @param gamma The step-size in the gradient descent.
//' @return The update latent factor for the current user / track.
//' @export
// [[Rcpp::export]]
arma::vec update_factor(arma::vec x, arma::mat z, arma::vec p_i, arma::mat Q,
			arma::vec beta, double lambda, double gamma) {
  arma::uvec obs_ix = arma::conv_to<arma::uvec>::from(arma::find_finite(x));
  arma::mat Q_obs = Q.rows(obs_ix);
  arma::vec resid = x(obs_ix) - Q_obs * p_i;
  arma::vec p_i_grad = 2 * (lambda * p_i - Q_obs.t() * resid);
  return  p_i - gamma * p_i_grad;
}


// @title Gradient step for regression coefficient
// @param X The ratings matrix. Unobserved entries must be marked NA. Users
// must be along rows, and tracks must be along columns.
// @param P The learned user latent factors.
// @param Q The learned track latent factors.
//@param beta The learned regression coefficients.
// @param lambda The regularization parameter for beta.
// @param gamma The step-size in the gradient descent.
// @return The update regression coefficients.
arma::vec update_beta(arma::mat X, arma::cube Z, arma::mat P, arma::mat Q, arma::vec beta,
		      double lambda, double gamma) {
  arma::uvec obs_ix = arma::conv_to<arma::uvec>::from(arma::find_finite(X));
  arma::mat resid = X - P * Q.t() - cube_multiply(Z, beta);
  arma::vec beta_grad = arma::zeros(beta.size());
  for(int l = 0; l < beta.size(); l++) {
    beta_grad[l] = accu(resid(obs_ix) % Z.slice(l)(obs_ix));
  }
  beta_grad = 2 * (lambda * beta - beta_grad);
  return beta - gamma * beta_grad;
}

// objective -------------------------------------------------------------------
// @title Objective function to minimize
// @description This is the sum of squared error at observed entries,
// between the ratings matrix and its approximation by latent factors and
// covariates.
// @param X The ratings matrix. Unobserved entries must be marked NA. Users
// must be along rows, and tracks must be along columns.
// @param Z The covariates associated with each pair. This must be shaped as an
// array with users along rows, tracks along columns, and features along
// slices.
// @param P The learned user latent factors.
// @param Q The learned track latent factors.
// @param beta The learned regression coefficients.
// @param lambda  The regularization parameters for P, Q, and beta, respectively.
// @return The value of the objective function given the current parameter
// values.
double objective_fun(arma::mat X, arma::cube Z, arma::mat P, arma::mat Q, arma::vec beta,
		     Rcpp::NumericVector lambdas) {
  arma::uvec obs_ix = arma::conv_to<arma::uvec>::from(arma::find_finite(X));
  arma::mat resid = X - P * Q.t() - cube_multiply(Z, beta);
  return arma::sum(arma::pow(resid(obs_ix), 2)) +
    arma::as_scalar(lambdas[0] * arma::accu(arma::pow(P, 2))) +
    arma::as_scalar(lambdas[1] * arma::accu(arma::pow(Q, 2))) +
    arma::as_scalar(lambdas[2] * arma::accu(arma::pow(beta, 2)));
}

// gradient-descent ------------------------------------------------------------
//' @title Regular gradient descent for latent factor model with covariates
//' @param X The ratings matrix. Unobserved entries must be marked NA. Users
//' must be along rows, and tracks must be along columns.
//' @param Z The covariates associated with each pair. This must be shaped as an
//' array with users along rows, tracks along columns, and features along
//' slices.
//' @param k_factors The number of latent factors for the problem.
//' @param lambdas The regularization parameters for P, Q, and beta, respectively.
//' @param n_iter The number of gradient descent iterations to run.
//' @param gamma The step-size in the gradient descent.
//' @return A list with the following elements, \cr
//'   $P The learned user latent factors. \cr
//'   $Q The learned track latent factors.
//'   $beta The learned regression coefficients.
//' @export
// [[Rcpp::export]]
Rcpp::List lf_gd_cov(Rcpp::NumericMatrix X, Rcpp::NumericVector Z_vec,
		     int k_factors, Rcpp::NumericVector lambdas, int n_iter,
		     double gamma_pq, double gamma_beta) {

  // convert to arma
  Rcpp::IntegerVector Z_dim = Z_vec.attr("dim");
  arma::cube Z(Z_vec.begin(), Z_dim[0], Z_dim[1], Z_dim[2], false);

  // initialize results
  int m = X.nrow();
  int n = X.ncol();
  int l = Z_dim[2];

  arma::mat P = 1.0 / sqrt(m) * arma::randn(m, k_factors);
  arma::mat Q = 1.0 / sqrt(n) * arma::randn(n, k_factors);
  arma::vec beta = 1.0 / sqrt(l) * arma::randn(l);
  arma::vec objs = arma::zeros(n_iter);

  // perform gradient descent steps
  for(int cur_iter = 0; cur_iter < n_iter; cur_iter++) {
    printf("iteration %d \n", cur_iter);

    // update user factors
    for(int i = 0; i < m; i++) {
      P.row(i) = update_factor(X.row(i), Z(arma::span(i), arma::span(), arma::span()),
			       arma::conv_to<arma::vec>::from(P.row(i)), Q, beta,
			       lambdas[0], gamma_pq).t();
    }

    // update track factors
    for(int j = 0; j < n; j++) {
      Q.row(j) = update_factor(X.column(j), Z(arma::span(), arma::span(j), arma::span()),
			       arma::conv_to<arma::vec>::from(Q.row(j)), P, beta,
			       lambdas[1], gamma_pq).t();
    }

    // update regression coefficients
    beta = update_beta(Rcpp::as<arma::mat>(X), Z, P, Q, beta, lambdas[2], gamma_beta);
    objs[cur_iter] = objective_fun(Rcpp::as<arma::mat>(X), Z, P, Q, beta, lambdas);
    printf("Objective: %g \n", objs[cur_iter]);
  }

  return Rcpp::List::create(Rcpp::Named("P") = P,
			    Rcpp::Named("Q") = Q,
			    Rcpp::Named("beta") = beta,
			    Rcpp::Named("objs") = objs);
}
