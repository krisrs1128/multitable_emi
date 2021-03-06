% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/svd_cov.R
\name{prepare_pred_data}
\alias{prepare_pred_data}
\title{Merge train and test data, for imputation}
\usage{
prepare_pred_data(train_list, newdata)
}
\arguments{
\item{train_list}{A data_list of the form
$train: A data.frame giving the artist, track, user, rating, and time info. \cr
$words: A matrix giving word indicators for artist-user pairs. \cr
$users: A matrix givin gsurvey results for each user. \cr}

\item{newdata}{A test data_list, of the same form as train_list.}
}
\value{
A list with the following elements, \cr
 $X: A user x track rating matrix
 $Z: A user x track x question covariates matrix
}
\description{
Merge train and test data, for imputation
}

