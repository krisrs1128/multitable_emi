
################################################################################
# Simple predictors based on the mean
################################################################################

# mean-model -------------------------------------------------------------------
#' @title "Train" a model that predicts the means within groups.
#' @param data_list A list containing the following elements, paralleling the
#' input provided by Kaggle,\cr
#'   train: A data.table giving the artist, track, user, rating, and time info. \cr
#'   words: A data.table giving word indicators for artist-user pairs. \cr
#'   users: A data.table givin gsurvey results for each user. \cr
#' We don't ever actually use words or users in this model. This convention is
#' enforced for simplicity only.
#' @param train_opts A list with an element called type, specifying the group to
#' average over.
#' @return A list with the following elements \cr
#'    cur_means: The means within groups for this model.
#'    type: The type of group over which we calculated means.
#'    global_mean: The mean over all ratings.
#' @importFrom dplyr group_by_ summarise
#' @export
mean_model <- function(data_list, train_opts = list(type = "User")) {
  type <- train_opts$type
  cur_summary <- data_list$train %>%
    group_by_(type) %>%
    summarise(mean = mean(Rating))
  cur_means <- setNames(cur_summary$mean, unlist(cur_summary[, type, with = F]))
  global_mean <- mean(data_list$train$Rating, na.rm = T)
  return (list(cur_means = cur_means, type = type, global_mean = global_mean))
}

#' @title "Predict" using means within groups
#' @param mean_model The output of a call to mean_model.
#' @param data_list A list in the same form as in the input to mean_model, but
#' with only the test cases.
#' @return Predictions on the test cases.
#' @export
mean_pred <- function(model, data_list) {
  pred_means <- model$cur_means[unlist(data_list$train[, model$type, with = F])]
  pred_means[is.na(pred_means)] <- model$global_mean
  return (pred_means)
}
