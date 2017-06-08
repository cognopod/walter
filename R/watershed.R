#' watershed
#'
#' A function for constructing a summary data frame for a series of exploratory factor analyses.
#'
#' @param data Data frame containing only variables to test.
#' @param maxlatent Maximum number of latent variables to test.
#' @param minlatent Minimum number of latent variables to test.
#' @param cutoff Cutoff value for leaving a variable out of that factor analysis.
#' @return A data frame with maximum factor loading identified for each variable.

watershed <- function(data, maxlatent = NA, minlatent = NA, cutoff = 0.4) {
  FACTORTEST <- data.frame(matrix(rep(NA,(maxlatent-(minlatent-1)) * (ncol(data))), ncol = maxlatent-(minlatent-1), nrow = ncol(data)))
  ITEM = c(LETTERS, c(t(outer(LETTERS, LETTERS, paste, sep = ""))))[1:ncol(data)]
  FACTORTEST <- cbind(ITEM, FACTORTEST)
  for (i in (TEST$nlatent+2):1) {
    data.pa.oblimin <- psych::fa(data, nfactors=i, SMC=TRUE, fm="pa", rotate="oblimin", max.iter=100)
    FACTORTEST[,i+1] <- as.vector(apply(data.pa.oblimin$Structure,1,function(x) {if(max(abs(x)) >= cutoff) paste0(LETTERS[i], which.max(abs(x))) else NA}))
  }
  return(FACTORTEST)
}
