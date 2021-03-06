% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/character_get.sig.adjectives.R
\name{get.sig.adjectives}
\alias{get.sig.adjectives}
\title{get.sig.adjectives}
\usage{
get.sig.adjectives(x, unique_only = FALSE)
}
\arguments{
\item{x}{text to process}

\item{unique_only}{Logical. If TRUE returns unique hits only.}
}
\description{
Extract adjectives used for in/significance out of list with 37 potential adjectives
}
\examples{
get.sig.adjectives(
 x<-"We found very highly significance for type 1 effect"
)
}
