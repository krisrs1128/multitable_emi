
################################################################################
# Making dummay variables for factors
################################################################################

# expand -----------------------------------------------------------------------
#' @title Construct Dummy Varibles for Factor Column
#' @param Z A matrix for which we need to construct dummy variables for every
#' factor column
#' @return Z_expand The matrix Z with all numeric columns shifted to the front
#' and all factor columns expanded into dummy variables at the back.
#' @importFrom caret class2ind
#' @importFrom data.table as.data.table
#' @importFrom magrittr %>%
#' @export
expand_factors <- function(Z) {
  Z <- as.data.table(Z)
  factor_ix <- which(sapply(Z, is.factor))
  if(length(factor_ix) < ncol(Z)) {
    Z_expand <- Z[, -factor_ix, with = F]
  } else {
    Z_expand <- matrix(nrow = nrow(Z), ncol = 0)
  }
  for(i in seq_along(factor_ix)) {
    cur_name <- names(factor_ix)[i]
    cur_factor <- unlist(Z[, factor_ix[i], with = F])
    if(any(is.na(cur_factor))) {
      cur_factor <- addNA(cur_factor)
    }
    new_mat <- class2ind(cur_factor)
    colnames(new_mat) <- paste(cur_name, colnames(new_mat), sep = ".")
    Z_expand <- cbind(Z_expand, new_mat)
  }
  return (Z_expand)
}
