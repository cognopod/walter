#' efa
#'
#' A wrapper function for running exploratory factor analysis.
#'
#' @param data A data frame with the data to be analyzed. May contain missing data (which will be deleted listwise).
#' @param nfactors The number of factors to return.
#' @param items A vector of column numbers to include in the analysis.
#' @param cutoff The cutoff for loading display in the factor analysis. 0.3 by default.
#' @return Prints the output of efa using principal axis/oblimin + recommendations for item reduction.
#' @export
efa <- function(data, nfactors = 2, items = c(1:ncol(data)), cutoff = 0.3){
  # data file
  SUBEFA <- subset(data, complete.cases(data), items)
  #
  # factor analysis
  data.pa.oblimin <- psych::fa(SUBEFA, nfactors=nfactors, SMC=TRUE, fm="pa", rotate="oblimin", max.iter=100)
  print(data.pa.oblimin, digits=2, cut=cutoff, sort=TRUE)
  # which item has the lowest communality
  cat("------------------------------------\n")
  cat("\nITEM WITH LOWEST COMMUNALITY: ")
  cat(which.min(data.pa.oblimin$communality), "\n")
  cat("LOWEST COMMUNALITY: ")
  cat(data.pa.oblimin$communality[which.min(data.pa.oblimin$communality)[1]], "\n")
  # which item has the lowest loading
  k = 1
  k.nom = NULL
  z = length(data.pa.oblimin$communality)
  for (i in 1:z) {
    j = max(abs(data.pa.oblimin$loadings[c(seq(from = i, to = (nfactors-1)*z + i, by = z))]))
    if (j < k) {k = j; k.nom = i}
  }
  cat("\nMINIMUM MAX LOADING:")
  cat("\nLOADING: ")
  cat(k, "\n")
  cat("VAR NUMBER: ")
  cat(which(colnames(data) == row.names(data.pa.oblimin$loadings)[k.nom], arr.ind=TRUE), "\n")
  cat("VAR NAME: ")
  cat(row.names(data.pa.oblimin$loadings)[k.nom], "\n")
  cat("THIS COMMUNALITY: ")
  cat(data.pa.oblimin$communality[k.nom])
}
