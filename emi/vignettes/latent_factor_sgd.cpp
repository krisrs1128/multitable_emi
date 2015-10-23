
////////////////////////////////////////////////////////////////////////////////
// Latent factor modeling, using stochastic gradient descent, as described in
// https://speakerd.s3.amazonaws.com/presentations/501d641214507500020491a3/MusicHackathonShanda.pdf
////////////////////////////////////////////////////////////////////////////////


#include <RcppArmadillo.h>

// [[Rcpp::depends(RcppArmadillo)]]
using namespace Rcpp;

//' @title Latent Factor modeling using Stochastic Gradient Descent
//' @description Decompose a matrix R to minimize
//'     ||R - b_u * 1 + 1 * b_i' - PQ||_{F}^{2} + lambda_1 * ||b_{u}||_{2}^{2} +
//'        lambda_2 * ||b_i||_{2}^{2} + lambda_3 * ||P||_{F}^{2} +
//'        lambda_4 * ||Q||_{F}^{2}
//' @param R A user x item matrix of ratings.
//' @param obs_ix A n_obs x 2 matrix whose rows give indices at which ratings
//' were observed.
//' @param lambdas A vector of regularization parameters, as in the objective
//' function given above.
//' @param eta The learning rate for the stochastic gradient descent.
//' @param iter_max The number of iterations to run the SGD.
//' @param d The dimension of the latent factors.
//' @return The fitted parameters from the objective function below, \cr
//'    bu: Bias terms for each user. \cr
//'    bi: Bias terms for each item. \cr
//'    P: A user x d latent factor matrix for users \cr.
//'    Q: A item x d latent factor matrix for items.
//' @export
// [[Rcpp::export]]
List lf_sgd(NumericMatrix R, IntegerMatrix obs_ix, NumericVector lambdas,
	    double eta = 0.05, int iter_max = 1000, int d = 5) {
  // get dimensions
  int n_u = R.nrow();
  int n_i = R.ncol();
  int n_obs = obs_ix.nrow();

  // initialize bias and latent factor terms
  arma::vec bu = arma::randn(n_u);
  arma::vec bi = arma::randn(n_i);
  arma::mat P = arma::randn(n_u, d);
  arma::mat Q = arma::randn(n_i, d);

  for(int iter = 0; iter < iter_max; iter++) {
    // draw a random user x item pair
    int cur_ix = floor(runif(1) * n_obs)[0];
    int cur_user = obs_ix(cur_ix, 0) - 1.0;
    int cur_item = obs_ix(cur_ix, 1) - 1.0;

    // get current error
    int r_ui_hat = bu(cur_user) + bi(cur_item) + arma::dot(P.row(cur_user), Q.row(cur_item));
    int e_ui = R(cur_user, cur_item) - r_ui_hat;

    // update parameters
    bu(cur_user) += eta * (e_ui - lambdas(0) * bu(cur_user));
    bi(cur_item) += eta * (e_ui - lambdas(1) * bi(cur_item));
    P.row(cur_user) += eta * (e_ui * Q.row(cur_item) - lambdas(2) * P.row(cur_user));
    Q.row(cur_item) += eta * (e_ui * P.row(cur_user) - lambdas(3) * Q.row(cur_item));
  }

  return Rcpp::List::create(Rcpp::Named("bu") = bu,
			    Rcpp::Named("bi") = bi,
			    Rcpp::Named("P") = P,
			    Rcpp::Named("Q") = Q);
}
