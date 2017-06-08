#' walt.violinplot
#' 
#' Function for drawing the composite violin plot.
#' 
#' @param df A dataframe what needs a violin plot.
#' @param column The name of the column to plot.
#' @return A ggplot.
walt.violinplot <- function(df, column) {
  thisPlot <- ggplot2::ggplot(df, ggplot2::aes_string(y=column, x=1)) + 
    ggplot2::geom_violin(fill = c("#a8ddb5"), color = c("#a8ddb5"), linetype = "blank", alpha=I(0.1), na.rm=TRUE ) +
    ggplot2::scale_color_manual(values=c("#a8ddb5")) +
    ggplot2::geom_rug() + 
    ggthemes::theme_tufte(ticks = F) +
    ggplot2::theme(axis.text.x=ggplot2::element_blank(), axis.title.x=ggplot2::element_blank(), text=ggplot2::element_text(size=10)) +
    ggplot2::geom_boxplot(width = 0.1, na.rm=TRUE)
  return(thisPlot)
}