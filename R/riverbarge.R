#' riverbarge
#'
#' A function for reporting on the quality of factors from a series of factor analyses
#'
#' @param data Data frame containing only variables to test.
#' @param maxlatent Maximum number of latent variables to test.
#' @param minlatent Minimum number of latent variables to test.
#' @param hicutoff Loading necessary to be considered as part of a strong factor.
#' @param cutoff Cutoff value for leaving a variable out of that factor analysis.
#' @param crosscutoff Cutoff value for unacceptable crossloading.
#' @return A data frame witha summary of the series of factor analyses: number of factors, number of crossloadings, number of strong, marginal, and weak factors, number of items in each factor.
#' @export
riverbarge <- function(data, maxlatent = ncol(data), minlatent = 1, hicutoff = 0.5, cutoff = 0.4, crosscutoff = 0.3) {
  COLUMNS <- 5
  FACTORTEST <- data.frame(matrix(rep(NA,(maxlatent-minlatent+1) * (COLUMNS + maxlatent)), ncol = (COLUMNS + maxlatent), nrow = maxlatent-minlatent+1 ))
  colnames(FACTORTEST) <- c("NFACTORS", "XLOAD", "STRONG", "MARGNL", "WEAK", outer("F", 1:maxlatent, paste, sep = ""))
  for (i in (maxlatent-minlatent):0) {
    FACTORTEST[(i+1), 1] <- minlatent + i
    data.pa.oblimin <- psych::fa(data, nfactors=minlatent+i, SMC=TRUE, fm="pa", rotate="oblimin", max.iter=100)
    HIITEMS <- as.vector(apply(unclass(data.pa.oblimin$loadings),2,function(x) {sum(abs(x) >= hicutoff)}))
    TOTALITEMS <- as.vector(apply(unclass(data.pa.oblimin$loadings),2,function(x) {sum(abs(x) >= cutoff)}))
    XLOAD <- as.vector(apply(unclass(data.pa.oblimin$loadings),1,function(x) {sum(abs(x) >= crosscutoff)}))
    FACTORTEST[(i+1), 2] <- sum(XLOAD > 1)
    # HI
    FACTORTEST[(i+1), 3] <- sum(HIITEMS >= 5)
    # T
    FACTORTEST[(i+1), 4] <- sum((TOTALITEMS >= 5 & HIITEMS >= 3 & HIITEMS < 5) | (HIITEMS >= 4 & HIITEMS < 5))
    FACTORTEST[(i+1), 5] <- FACTORTEST[(i+1), 1] - (FACTORTEST[(i+1), 3] + FACTORTEST[(i+1), 4])
    FACTORTEST[(i+1), (COLUMNS+1):((COLUMNS)+length(TOTALITEMS))] <- TOTALITEMS
  }
  return(FACTORTEST)
}
