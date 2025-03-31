#' Phi Correlation
#'
#' Calculates the phi correlation between dichotomous variables and displays the results
#' with a heatmap.
#'
#' @param data Dataset to which the variables belong.
#' @param asig A list that can be defined by the user if they wish to specify a value for 0 and 1.
#' @return Heatmap where the correlation between the categorical variables is shown.
#' @examples
#' data <- data.frame(
#' smoke = c("Smokes", "Smokes", "Smokes", "Does not smoke", "Smokes"),
#' diabetes = c("No", "No", "Yes", "Yes", "No"),
#' hypertension = c("Yes", "No", "Yes", "No", "Yes")
#' )
#' asig <- list(smoke = c("Does not smoke", "Smokes"), diabetes = c("No", "Yes"))
#' phi_cor(data, asig)
#' @export
phi_cor <- function(data, asig = list()) {
  #Detect dichotomous variables
  bin_var <- sapply(data, function(x) {
    if (is.numeric(x)) {
      length(unique(x)) == 2
    } else if (is.factor(x) || is.character(x)) {
      length(unique(x)) == 2
    } else {
      FALSE
    }
  })

  if (!any(bin_var)) {
    stop("Error: No dichotomous variables were found in the dataset.")
  }

  #No dichotomous variables were found in the dataset.
  bin_data <- data.frame(lapply(names(data), function(var) {
    if (bin_var[var]) {
      if (var %in% names(asig)) {
        #User-defined order
        levels_order <- asig[[var]]
      } else {
        #Default alphabetical order
        levels_order <- sort(unique(data[[var]]))
      }
      as.numeric(factor(data[[var]], levels = levels_order)) - 1
    } else {
      data[[var]]
    }
  }))

  colnames(binary_data) <- names(data)

  #Phi correlation matrix
  n <- sum(bin_var)
  phi_matrix <- matrix(NA, n, n)
  colnames(phi_matrix) <- rownames(phi_matrix) <- names(binary_data)[bin_var]

  #Phi value
  for (i in 1:n) {
    for (j in 1:n) {
      phi_matrix[i, j] <- phi(table(bin_data[, i], bin_data[, j]))
    }
  }

  corrplot(phi_matrix, method = "color", type = "full",
           tl.col = "black", tl.srt = 45, diag = TRUE,
           col = colorRampPalette(c("darkblue", "white", "darkred"))(200),
           addCoef.col = "black", number.cex = 0.8,
           cl.cex = 0.8,
           cl.ratio = 0.2,
           cl.length = 5)
}
