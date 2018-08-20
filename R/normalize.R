#' Normalize
#'
#' This function allows you normalise a variable. The function takes one variable as an input, and then subtracts the mean and divides by the standard deviation of the variable.
#' @param x A variable
#' @keywords Normalisation, Standardisation, Centering
#' @export 
#' @examples 
#' normalize()

normalize <- function(x) {
  y <- (x - mean(x, na.rm = TRUE))/
    sd(x, na.rm = TRUE)
  return(y)
}