#' riverbank
#'
#' A function to create edges and node for riverplot.
#'
#' @param factorlist A list of factors from \code{watershed()}
#' @return A list of components
#' @export
riverbank <- function(factorlist) {
  palette = paste0(RColorBrewer::brewer.pal(ncol(factorlist), "Set1"), "90")
  for (this.col in 2:ncol(factorlist)){
    if (this.col == 2) {
      EDGES <- as.data.frame(table(factorlist[,(this.col-1)], factorlist[,this.col]))
    } else {
      EDGES <- rbind(EDGES, as.data.frame(table(factorlist[,(this.col-1)], factorlist[,this.col])))
    }
  }
  colnames(EDGES) <- c("N1", "N2", "Value")
  for (this.col in 1:ncol(factorlist)){
    if (this.col == 1) {
      NODES <- data.frame(ID = unique(factorlist[,this.col]), x = length(na.omit(unique(factorlist[,this.col]))))
    } else {
      NODES <- rbind(NODES, data.frame(ID = unique(factorlist[,this.col]), x = this.col))
    }
  }
  colnames(NODES) <- c("ID", "x")
  NODES <- na.omit(NODES)
  NODES <- NODES[order(NODES$ID),]
  NODES$col <- palette[NODES$x]
  #NODES$x <- as.numeric(levels(NODES$x))[NODES$x]
  return(list("NODES" = NODES, "EDGES" = EDGES))
}
