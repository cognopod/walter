#' walt.histogram
#' 
#' A function for drawing the composite histogram.
#' 
#' @param df A dataframe what needs a violin plot.
#' @param column The name of the column to plot.
#' @return A ggplot.
walt.histogram <- function(df, column) {
  g.box = ggplot2::ggplot(df, ggplot2::aes_string(column)) +
    ggplot2::geom_boxplot(ggplot2::aes_string(y = column, group = 0), width = 0.1, na.rm=TRUE) +
    ggthemes::theme_tufte(ticks = F) +
    ggplot2::theme(axis.ticks=ggplot2::element_blank(),
                   axis.line=ggplot2::element_blank(),
                   axis.text.x=ggplot2::element_blank(),
                   axis.title.x=ggplot2::element_blank(),
                   axis.text.y=ggplot2::element_blank(),
                   axis.title.y=ggplot2::element_blank(),
                   text=ggplot2::element_text(size=8))
  #g.box
  
  g.box.grob <- ggplot2::ggplotGrob(g.box)
  
  g.histo = ggplot2::ggplot(df, ggplot2::aes_string(x = column)) + 
    ggthemes::theme_tufte(ticks = F) +
    ggplot2::geom_histogram( ggplot2::aes_string(x = column, y = "..density.."), binwidth = 1, fill = c("#a8ddb5"), color = c("#a8ddb5"), linetype = "blank", alpha=I(0.1), na.rm=TRUE ) + 
    ggplot2::geom_histogram( ggplot2::aes_string(x = column, y = "-..density.."), binwidth = 1, fill = c("#a8ddb5"), color = c("#a8ddb5"), linetype = "blank", alpha=I(0.1), na.rm=TRUE ) +
    ggplot2::coord_flip() +
    ggplot2::theme(axis.text.x=ggplot2::element_blank(), axis.title.x=ggplot2::element_blank(), text=ggplot2::element_text(size=10))
  #g.histo
  
  g.histo.range <- max(ggplot2::ggplot_build(g.histo)[[1]][[1]]$y)
  #return(ggplot_build(g.histo))
  thisPlot <- g.histo + ggplot2::annotation_custom(grob=g.box.grob, xmin=-Inf, xmax=Inf, ymin=-g.histo.range/4.5,ymax=g.histo.range/4.5)
  return(thisPlot)
}