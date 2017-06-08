#' heuristics
#'
#' Summary output of heuristics before and after factor analysis.
#'
#' @param data A data frame with the data to be analyzed. May contain missing data (which will be deleted listwise).
#' @param items A vector of column numbers to include in the analysis.
#' @return Prints the results of 3 heuristics + eigenvalues > 1 (for purposes of mockery) + scree plot to graphics device.
#' @export
heuristics <- function (data, items = c(1:ncol(data))) {
  # data file
  data <- subset(data, complete.cases(data), items)
  cat("Eigenvalues >= 1: ")
  EGON <- sum(sapply(eigen(cor(data, use="pairwise.complete.obs"))$values, function (x) {if (x > 1) 1 else 0}))
  cat(EGON, "\n")

  psych::VSS.scree(data)
  sink(tempfile())
  pdf(NULL)
  PARA <- suppressWarnings(psych::fa.parallel(data, fa="both"))
  MAP <- suppressWarnings(psych::vss(data, n = 10, rotate = "oblimin", diagonal = FALSE, SMC=FALSE, fm = "pa", n.obs=nrows(EFA),plot=FALSE,title="Very Simple Structure"))
  ICLUST <- suppressWarnings(psych::ICLUST(data, beta.min=0.7, beta.size=2, n.iterations=2, output=1))
  dev.off()
  sink()
  cat("Parallel Analysis: ", PARA$nfact, "\n")
  cat("Velicer's MAP: ", which.min(MAP$map), "\n")
  CLUSTER.SIZES <- apply(ICLUST$clusters, 2, function(x) {sum(abs(x))})
  ICLUST.BIG <- sum(sapply(CLUSTER.SIZES, function (x) {if (x > 3) 1 else 0}))
  ICLUST.SMALL <- sum(sapply(CLUSTER.SIZES, function (x) {if (x > 3) 0 else 1}))
  cat("Big ICLUST cluster (>3): ", ICLUST.BIG, "\n")
  cat("Small ICLUST cluster (<=3): ", ICLUST.SMALL, "\n")
  cat("ICLUST cluster size summary:\n")
  table(CLUSTER.SIZES)
}



