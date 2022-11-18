#' @title Correlation Table of class 'ryx'
#'
#' @description Given a dataframe, a y variable, and a vector x of variables, the \code{ryx} function would
#' return a list of variable y, vector of variables x, and a dataframe of correlation, p-value, and stars rating of the
#' p-value.
#'
#' @param data a dataframe
#' @param y a variable
#' @param x a vector of variables to create a correlation table, if NULL, return a correlation table of all of the variables
#' excluding y itself
#'
#' @return A plot with x-axis as the absolute correlation values and y-axis as the variables.
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
#' }
#'
#'
ryx <- function(data, y, x){
  if(missing(x)){
    x <- names(data)[sapply(data, class)=="numeric"]
    x <- setdiff(x, y)
  }
  df <- data.frame()
  for (var in x){
    res <- cor.test(data[[y]], data[[var]])
    df_temp <- data.frame(variable = var,
                          r = res$estimate,
                          p = res$p.value)
    df <- rbind(df, df_temp)
    df <- df[order(-abs(df$r)),]
  }

  df$sigif <- ifelse(df$p < .001, "***",
                     ifelse(df$p < .01, "**",
                            ifelse(df$p < .05, "*", " ")))
  results <- list(y=y, x=x, df=df)
  class(results) <- "ryx"
  return(results)
}
