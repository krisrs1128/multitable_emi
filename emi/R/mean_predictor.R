
################################################################################
# Simple predictors based on the mean
################################################################################

# mean-predictor ---------------------------------------------------------------
#' @title Predict using the mean of a group
#' @param train The training data, with a column for "type" and a column for
#' true ratings.
#' @param test The test data, with a column for "type", whose ratings we want to
#' predict.
#' @param type The column to take averages over. Defaults to user.
#' @return pred_means The predicted ratings for the test data.
#' @importFrom magrittr %>%
#' @importFrom dplyr group_by_ summarise
#' @export
mean_predictor <- function(train, test, type = "User") {
  cur_summary <- train %>%
    group_by_(type) %>%
    summarise(mean = mean(Rating))
  cur_means <- setNames(cur_summary$mean, unlist(cur_summary[, type]))
  pred_means <- cur_means[test[, type]]
  pred_means[is.na(pred_means)] <- mean(train$Rating)
  return (pred_means)
}
