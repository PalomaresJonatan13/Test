#' Polychoric Correlation
#'
#' Calculates the polychoric correlation between ordinal variables and displays the results
#' with a heatmap.
#'
#' @param data Dataset to which the variables belong.
#' @return Heatmap where the correlation between the categorical variables is shown.
#' @examples
#' data <- data.frame(
#'   taste = sample(1:100, 30, replace = TRUE),  # Scale from 1 to 5
#'   aroma = sample(1:100, 30, replace = TRUE),
#'   color = sample(1:100, 30, replace = TRUE),
#'   texture = sample(1:100, 30, replace = TRUE)
#'  )
#' example_data <- data.frame(lapply(example_data, as.factor))
#' poly_cor(example_data)
#' @export
poly_cor <- function(data){
  if (!is.data.frame(data)){
    stop("Error: The input data is not a dataframe.")
  }

  pcor <- hetcor(data)$correlations

  #Correlation Matrix
  corrplot(pcor, method = "color", type = "full",
           tl.col = "black", tl.srt = 45, diag = TRUE,
           col = colorRampPalette(c("darkblue", "white", "firebrick4"))(200),
           addCoef.col = "black", number.cex = 0.8)
}


