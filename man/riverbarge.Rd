% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/riverbarge.R
\name{riverbarge}
\alias{riverbarge}
\title{riverbarge}
\usage{
riverbarge(data, maxlatent = ncol(data), minlatent = 1, hicutoff = 0.5,
  cutoff = 0.4, crosscutoff = 0.3)
}
\arguments{
\item{data}{Data frame containing only variables to test.}

\item{maxlatent}{Maximum number of latent variables to test.}

\item{minlatent}{Minimum number of latent variables to test.}

\item{hicutoff}{Loading necessary to be considered as part of a strong factor.}

\item{cutoff}{Cutoff value for leaving a variable out of that factor analysis.}

\item{crosscutoff}{Cutoff value for unacceptable crossloading.}
}
\value{
A data frame witha summary of the series of factor analyses: number of factors, number of crossloadings, number of strong, marginal, and weak factors, number of items in each factor.
}
\description{
A function for reporting on the quality of factors from a series of factor analyses
}
