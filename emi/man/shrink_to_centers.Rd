% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/postprocess.R
\name{shrink_to_centers}
\alias{shrink_to_centers}
\title{Shrink a vector towards closest points in a vector}
\usage{
shrink_to_centers(y, y_star, alpha = 0.2)
}
\arguments{
\item{y}{A vector that we are postprocessing.}

\item{y_star}{A vector of points to shrink y towards}

\item{alpha}{The strength of the shrinkage}
}
\value{
The vector y, but with each element shrink towards the closest
element among the y_stars.
}
\description{
Shrink a vector towards closest points in a vector
}

