% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/autorunJagsAndCaptureOutput.R
\name{.autorunJagsAndCaptureOutput}
\alias{.autorunJagsAndCaptureOutput}
\title{Fit an MCMC model to a dataset, capture and log JAGS messages}
\usage{
.autorunJagsAndCaptureOutput(
  data,
  modelString,
  monitor,
  inits = NULL,
  returnClass = "binomialModel",
  max.time = 600,
  ...
)
}
\arguments{
\item{data}{the JAGS data list}

\item{modelString}{The JAGS model string}

\item{monitor}{a vector of names of parameters to monitor}

\item{inits}{the inits used by the JAGS model}

\item{returnClass}{the class to prepend to the class of the tab element of
of the returned value}

\item{max.time}{the maximum time allowed for the model fitting,}

\item{...}{other parameters passed to autorun.jags}
}
\description{
Fit an MCMC model to a dataset, capture and log JAGS messages
}
