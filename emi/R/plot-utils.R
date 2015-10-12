
################################################################################
# Utilities to make nice plots
################################################################################

# plot-glmnet -------------------------------------------------------------
#' @title Alternative visualization of glmnet coefficients
#' @description Visualize significant coefficients using a heatmap,
#' instead of a trace of the lasso paths.
#' @param beta The coefficient matrix of a glmnet object.
#' @return res_plot A heatmap of the lasso paths.
#' @importFrom reshape2 melt
#' @importFrom ggplot2 ggplot geom_tile aes scale_fill_gradient2 element_text
#' element_blank geom_vline scale_color_manual
#' theme
#' @export
plot_lasso_coef <- function(beta) {
  # reshape betas
  beta <- as.matrix(beta)
  beta <- beta[rev(rownames(beta)), ]
  mbeta <- melt(beta)
  colnames(mbeta) <- c("feature", "lambda", "value")
  mbeta <- mbeta[order(mbeta$lambda), ]

  ggplot(mbeta) +
    geom_tile(aes(x = lambda, y = feature, fill = value), col = "grey") +
    scale_fill_gradient2(midpoint = 0) +
    theme(axis.text.x = element_text(angle = -90, size = 5, hjust = 0, vjust = 0),
          axis.text.y = element_text(size = 10),
          axis.title = element_blank())
}
