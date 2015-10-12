
################################################################################
# Tools for reshaping and subsetting the EMI data to make it more amenable to
# statistical analysis.
################################################################################

# get-matrix -------------------------------------------------------------------
#' @title Get matrix type
#' @description Check whether the input matrix is the users or words matrix
#' @param X The matrix to check.
#' @return Either "users" or "words" depending on the matrix type.
#' @export
get_data_type <- function(X) {
  ifelse("I.love.technology" %in% colnames(X), "users", "words")
}

# get-numeric ------------------------------------------------------------------
#' @title Get Quantitative Features
#' @param X The matrix to get numeric columns for.
#' @return The matrix of quantitative features.
#' @importFrom data.table data.table
#' @export
get_numeric <- function(X) {
  X <- as.data.table(X)
  numeric_ix <- sapply(X, is.numeric)
  as.matrix(X[, numeric_ix, with = F])
}

# get-keys ---------------------------------------------------------------------
#' @title Get the keys for the matrix
#' @param X The matrix to get keys for.
#' @description Get the keys associated with the users and words matrices.
#' @return Either the RESPID or RESPID + artists IDs for the users or words
#' matrices.
#' @importFrom data.table as.data.table
#' @export
get_keys <- function(X) {
  type <- get_data_type(X)
  X <- as.data.table(X)
  if(type == "users") {
    Y <- X$RESPID
  } else {
    Y <- X[, c("Artist", "User"), with = F]
  }
  return (Y)
}

# get-descriptions -------------------------------------------------------------
#' @title Get Descriptions
#' @description This will return columns that are not the numeric question
#' variables and not the keys. E.g., this returns gender, age, working, etc.
#' @return Y A data.table with the descriptions of each data type.
#' @importFrom data.table as.data.table
#' @export
get_desc <- function(X) {
  type <- get_data_type(X)
  X <- as.data.table(X)
  if(type == "users") {
    Y <- X[, 2:8, with = F]
  } else {
    Y <- X$V3
  }
  return (Y)
}
