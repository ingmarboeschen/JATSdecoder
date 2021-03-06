% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/character_get.power.R
\name{get.power}
\alias{get.power}
\title{get.power}
\usage{
get.power(x)
}
\arguments{
\item{x}{text to process}
}
\description{
Extract a priori power, empirial power values and 1-betaerror
}
\examples{
x<-"We used G*Power 3 to calculate the needed sample with 
beta error rate set to 12\% and alpha error to .05."
get.power(x)
}
