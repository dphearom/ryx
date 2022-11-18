#' @title Print of Correlation for Object Class 'ryx'
#'
#' @description Given an object of class 'ryx', the \code{print} function would print table with
#' column names "variable", "r", "p", "sigif". The table also excludes the rownames.
#'
#' @param x An object of 'ryx' class
#' @param digits Rounding digits of the values, default value of 3
#'
#' @return A printing of the correlation table
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
#' #print the 'ryx' class object
#' printR <- print(x)
#' }
#'

print.ryx <- function(x, digits = 3){
  if(!inherits(x,"ryx")){
    stop("This function requires an object of type 'ryx'.")
  }
  df <- x[["df"]]
  df$r <- round(df$r,digits)
  df$p <- format.pval(df$p,digits)
  cat("Correlation of ", x[["y"]], " with\n")
  print(df,row.names=FALSE)
}
