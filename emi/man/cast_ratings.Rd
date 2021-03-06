% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/svd_predict.R
\name{cast_ratings}
\alias{cast_ratings}
\title{Cast a ratings matrix from tall to wide}
\usage{
cast_ratings(mR)
}
\arguments{
\item{mR}{A "tall" version of the usual ratings matrix. Each observed
user-movie pair is a row of this matrix. The user and movie ids are the first
two columns, the third is the rating associated with each observed pair.}
}
\value{
R The "wide", standard form of the ratings matrix.
}
\description{
Cast a ratings matrix from tall to wide
}

