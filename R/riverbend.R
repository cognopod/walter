#' riverbend
#'
#' A function for automatically generating factor analysis riverplots wiht a sequence of solutions.
#'
#' @param data Data frame containing only variables to test.
#' @param maxlatent Maximum number of latent variables to test.
#' @param minlatent Minimum number of latent variables to test.
#' @param cutoff Cutoff value for leaving a variable out of that factor analysis.
#' @return Draws a riverplot
#' @export
riverbend <- function(data, maxlatent = NA, minlatent = NA, cutoff = 0.4) {
  FACTORS <- watershed(data, maxlatent = maxlatent, minlatent = minlatent, cutoff = cutoff)
  RIVER <- riverbank(dplyr::select(FACTORS, -ITEM))
  FACTOR.COUNT <- data.frame(table(tidyr::gather(dplyr::select(FACTORS, -ITEM), key = 'FACTOR', value = 'NODE')$NODE))
  r <- riverplot::makeRiver(RIVER$NODES, RIVER$EDGES, node_labels = as.character(FACTOR.COUNT$Freq))
  uselessinfo <- capture.output(riverplot::riverplot(r, gravity = "center", direction = "rl"))
}
