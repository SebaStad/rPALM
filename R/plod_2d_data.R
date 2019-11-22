#' Title
#'
#' @param matrix A 2D Matrix
#'
#' @return Returns a ggplot
#'
#' @examples
#' x <- matrix(runif(100), ncol = 10, nrow =10 )
#' plot_2d_data(x)
plot_2d_data <- function(matrix){
  require(reshape2)
  require(ggplot2)
  meltvec  <- reshape2::melt(matrix)


  ggplot2::ggplot(meltvec, ggplot2::aes(x = Var1, y = Var2, fill = value)) +
    geom_tile() +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11),
                       legend.text=element_text(size=7))
}
