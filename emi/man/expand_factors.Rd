% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/expand_factors.R
\name{expand_factors}
\alias{expand_factors}
\title{Construct Dummy Varibles for Factor Column}
\usage{
expand_factors(Z)
}
\arguments{
\item{Z}{A matrix for which we need to construct dummy variables for every
factor column}
}
\value{
Z_expand The matrix Z with all numeric columns shifted to the front
and all factor columns expanded into dummy variables at the back.
}
\description{
Construct Dummy Varibles for Factor Column
}

