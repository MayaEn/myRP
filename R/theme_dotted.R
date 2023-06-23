#' Theme Dotted - a custom ggplot2 theme
#'
#' @return A theme function
#' @export
#'
#' @examples
#' library(ggplot2)
#'
#' p <- ggplot(mtcars, aes(mpg, hp)) + geom_point()
#' p + theme_dotted()
theme_dotted <- function() {
  ggplot2::theme_minimal() +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_line(color = "grey", size = 0.1, linetype = "dotted"),
      panel.background = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(colour = "grey"),
      legend.title = ggplot2::element_text(size = 12),
      text = ggplot2::element_text(face = "plain", family = "sans"),
      legend.key = ggplot2::element_blank()
    )
}
