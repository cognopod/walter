#' waltKMO
#'
#' waltKMO is a function for calculating Kaiser-Meyer using the Kaiser & Rice (1974) algorithm.
#'
#' Kaiser and Rice (1974) categorized KMO values using the following subjective appraisals.
#'
#' \tabular{rl}{
#'    in the 0.90s \tab marvelous\cr
#'    in the 0.80s \tab meritorious\cr
#'    in the 0.70s \tab middling\cr
#'    in the 0.60s \tab mediocre\cr
#'    in the 0.50s \tab miserable\cr
#'    below 0.50 \tab unacceptable
#' }
#'
#' @param x a dataframe containing all of the columns to be included in an analysis.
#' @return A list of components.
#'    \item{KMO}{The overall measure of sampling adequacy.}
#'    \item{MSA}{Measures of sampling adequacy for each item.}
#' @export
waltKMO <- function(x)
{
  # Omit missing values
  x <- subset(x, complete.cases(x))
  # correlation components
  # Squared correlation matrix
  squared.cor <- (cor(x))^2
  # squared partial correlation components
  inv.matrix <- solve(cor(x))
  diag.inv <- diag(inv.matrix)
  sqpartial.cor <- (-inv.matrix/sqrt(outer(diag.inv, diag.inv)))^2
  # delete diagonals (correlations with self) to examine relationships between variables
  diag(squared.cor) <- diag(sqpartial.cor) <- 0
  # proportion of correlations due to partial correlations
  KMO <- sum(squared.cor)/(sum(squared.cor)+sum(sqpartial.cor))
  # for each item
  MSA <- colSums(squared.cor)/(colSums(squared.cor)+colSums(sqpartial.cor))
  return(list(KMO=KMO, MSA=MSA))
}
