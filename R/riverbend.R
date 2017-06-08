riverbend <- function(data, maxlatent = NA, minlatent = NA, cutoff = 0.4) {
  FACTORS <- watershed(data, maxlatent = maxlatent, minlatent = minlatent, cutoff = cutoff)
  RIVER <- riverbank(dplyr::select(FACTORS, -ITEM))
  FACTOR.COUNT <- data.frame(table(tidyr::gather(dplyr::select(FACTORS, -ITEM), key = 'FACTOR', value = 'NODE')$NODE))
  r <- riverplot::makeRiver(RIVER$NODES, RIVER$EDGES, node_labels = as.character(FACTOR.COUNT$Freq))
  uselessinfo <- capture.output(riverplot::riverplot(r, gravity = "center", direction = "rl"))
}
