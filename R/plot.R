#' @title Plot of Correlation for Object Class 'ryx'
#'
#' @description Given an object of class 'ryx', the \code{plot} function would return a ggplot showing the
#' absolute correlation values for each variables in the object.
#'
#' @param x An object of 'ryx' class
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
#' #plot the 'ryx' class object
#' plotR <- plot(x)
#' }
#'
plot.ryx <- function(x){
  if(!inherits(x,"ryx")){
    stop("This function requires an object of type 'ryx'.")
  }
  library(ggplot2)
  ggplot(data = x$df,
         aes(x = reorder(x$df$variable,abs(x$df$r)),
             y = abs(x$df$r))) +
    geom_point(aes(color=ifelse(x$df$r < 0, "negative","positive"))) +
    scale_color_manual(name="Direction",
                       values = c("negative" = "red",
                                  "positive" = "blue"))+
    geom_segment(aes(x=x$df$variable,xend=x$df$variable,y=0,yend=abs(x$df$r))) +
    coord_flip() +
    labs(
      title = paste("Correlation with", x$y),
      x = "Variables",
      y = "Correlation (absolute value)") +
    scale_y_continuous(limits = c(0,1),breaks = seq(0.0,1.0, by=0.1))+
    theme_minimal() +
    theme(panel.grid.minor.x = element_blank(),
          panel.grid.major.x = element_line(colour="grey", linetype="dashed"),
          panel.grid.minor.y = element_blank(),
          panel.grid.major.y = element_blank(),
          panel.border = element_rect(colour = "black", fill = NA, size = 0.5))

}
