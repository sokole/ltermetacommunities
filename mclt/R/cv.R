#' @title Coefficient of variation
#'
#'
#' @author LTER metacommunities group
#' @author Eric R. Sokol \email{sokole@gmail.com}
#'
#' @description Calculates the coefficient of variation (CV) for a vector of numbers
#'
#'
#' @importFrom stats sd
#'
#'
#' @param x A vector of numeric values
#'
#'
#' @return Returns a numeric value
#'
#'
#' @examples
#' cv(c(23, 54, 34, 10, 5))
#'
#'
#' @export
#'
cv <- function(x){
  x <- as.numeric(x)
  stats::sd(x, na.rm = TRUE)/mean(x, na.rm = TRUE)
}
