#' @title Summary of Correlation for Object Class 'ryx'
#'
#' @description Given an object of class 'ryx', the \code{summary} function would print out the
#' description of the 'ryx' object class. The description includes the variables that are compared to each other,
#' the median absolute correlation value, the range of correlation values, and amount of variables with
#' p values less than 0.05.
#'
#' @param x An object of 'ryx' class
#' @param digits Digits for rounding of the values, Default value is at 3
#'
#' @return A summary statement of the median correlation value, range of correlation values, and amount
#' of variables with p-value < 0.05
#' @export
#'
#' @examples
#' \dontrun{
#' #import MASS package for example data
#' library(MASS)
#'
#' #create an 'ryx' class object
#' x <- ryx(Boston, y = "medv")
#'
#' #summary of the 'ryx' class object
#' summaryR <- summary(x)
#' }
#'

summary.ryx <- function(x, digits=3){
  if(!inherits(x,"ryx")){
    stop("This function requires an object of type 'ryx'.")
  }
  df <- x[["df"]]
  p_val <- which(df$p < 0.05)
  cat("Correlating",x[["y"]],
      "with", x[["x"]],"\n",
      "The median absolute correlation was", round(median(abs(df$r)),digits), "with a range from",
      round(min(df$r),digits), "to",round(max(df$r),digits), "\n",
      length(p_val), "out of", length(df$p), "variables where significant at the p < 0.05 level."
  )
}
