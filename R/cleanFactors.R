#' cleanFactors
#'
#' Make factor analysis practice data.
#'
#' @param nsubject Number of rows of data to simulate.
#' @param nlatent Number of latent factors. By default generates a random number of factors between 2 and 6.
#' @param nvars Number of measured variables (items) for each latent factor. By default generated a random number of variables for each factor. Assigning a value generates the same number of variables for each factor.
#' @param ndistract Number of variable (items) not loading on any factor.
#' @param noiselevel An integer to add noise to the structure matrix used to create the factor structure.
#' @param garbage A boolean argument used to generate uniform data with some level of noise.
#' @return A list of components.
#'    \item{sample}{The sample dataset.}
#'    \item{nlatent}{The number of latent factors used to generate the dataset.}
#'    \item{nvars}{A vector of the number of variables in each factor.}
#'    \item{ndistract}{The number of variables not in the factor structure.}
#'    \item{nsubjects}{The number of subjects in the dataset.}
#'    \item{noiselevel}{The amount of noise added to the factor structure.}
#' @export
cleanFactors <- function (nsubjects = 300, nlatent = NA, nvars = NA, ndistract = NA, noiselevel = 0, garbage = FALSE) {
  # number of latent factors
  # generates a random discrete value between 2 and 6
  if (is.na(nlatent)) {
    nlatent <- e1071::rdiscrete(1,c(.1,.1,.1,.1,.1))+1
  }
    # number of variables in each latent factor
  # generates a random discrete value between 4 and 8 for each latent factor
  if (is.na(nvars)) {
    nvars <- e1071::rdiscrete(nlatent,c(.1,.1,.1,.1,.1))+3
  } else {
    nvars <- rep(nvars, nlatent)
  }
  # counter for loop in next step
  countVars <- 1
  # number of non-loading variables
  # generates a random discrete value between 2 and 6
  if (is.na(ndistract)) {
    ndistract <- e1071::rdiscrete(1,c(rep(.1,10)))+5
  }
  ###############
  # generate fx #
  ###############
  # generate factor loadings
  loadings <- apply(as.matrix(runif(nvars[1], 0.6-(0.05*noiselevel), 0.8)), 1, function(x) x * sample(c(1,-1),1,prob=c(0.7,0.3)))
  # thisx <- rep(0, sum(nvars) + ndistract)
  thisx <- runif(sum(nvars) + ndistract,-0.05*noiselevel,0.05*noiselevel)
  thisx[countVars:(countVars+nvars[1]-1)] <- loadings
  countVars <- countVars + nvars[1]
  fx <- as.matrix(thisx,ncol=1)
  for (j in 2:nlatent) {
    loadings <- apply(as.matrix(runif(nvars[j], 0.6-(0.05*noiselevel), 0.8)), 1, function(x) x * sample(c(1,-1),1,prob=c(0.7,0.3)))
    # thisx <- rep(0, sum(nvars) + ndistract)
    thisx <- runif(sum(nvars) + ndistract,-0.05*noiselevel,0.05*noiselevel)
    thisx[countVars:(countVars+nvars[j]-1)] <- loadings
    countVars <- countVars + nvars[j]
    thisx <- as.matrix(thisx,ncol=1)
    fx <- cbind(fx, thisx)
  }
  if (garbage == TRUE) {
    fx <- matrix(runif((sum(nvars) + ndistract)*nlatent,-0.05*noiselevel,0.05*noiselevel), nrow = sum(nvars) + ndistract, ncol = nlatent)
  }
  ######################################
  # generate factor correlation matrix #
  ######################################
  gload <- matrix(c(1))
  fload <- matrix(runif(nlatent,-0.9,0.9),ncol=1,byrow=TRUE)
  Phi <- psych::sim.hierarchical(gload,fload)
  #################
  # generate data #
  #################
  sample <- data.frame(psych::sim.structure(fx, Phi, n=nsubjects, raw=TRUE)$observed)
  sample <- unlist(sample)
  fakedata <- as.integer(round(scales::rescale(sample, to=c(1,7))))
  fakedata <- data.frame(matrix(fakedata, nrow = nsubjects, ncol = sum(nvars) + ndistract))
  return(list(sample = fakedata, nlatent = nlatent, nvars = nvars, ndistract = ndistract, nsubject = nsubjects, noiselevel = noiselevel, popstructure = fx, popfacforcorr = Phi))
}
