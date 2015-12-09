
################################################################################
# Functions to postprocess predictions
################################################################################

#' @title Shrink a vector towards closest points in a vector
#' @param y A vector that we are postprocessing.
#' @param y_star A vector of points to shrink y towards
#' @param alpha The strength of the shrinkage
#' @return The vector y, but with each element shrink towards the closest
#' element among the y_stars.
#' @export
shrink_to_centers <- function(y, y_star, alpha = 0.2) {
  closest_y <- sapply(y, function(x) y_star[which.min(abs(x - y_star))])
  (1 - alpha) * y + alpha * closest_y
}
