% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/character_get.assumption.R
\name{get.assumptions}
\alias{get.assumptions}
\title{get.assumptions}
\usage{
get.assumptions(x, hits_only = TRUE)
}
\arguments{
\item{x}{text to process}

\item{hits_only}{Logical. If TRUE returns the detected assumtions only, else a hit matrix with all potential assumptions}
}
\description{
Extract mentioned assumptions in text out of list with 22 statistical assumptions
}
\examples{
x<-"Sphericity assumption and gaus-marcov was violated."
get.assumptions(x)
}
