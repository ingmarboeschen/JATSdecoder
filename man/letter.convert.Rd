% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_letter.convert.R
\name{letter.convert}
\alias{letter.convert}
\title{letter.convert}
\usage{
letter.convert(x, cermine = FALSE, greek2text = FALSE, warning = TRUE)
}
\arguments{
\item{x}{text to process}

\item{cermine}{Logical. If TRUE CERMINE specific error handling and letter conversion will be applied}

\item{greek2text}{Logical. If TRUE some greek letters and special characters will be unified to textual representation. (important to extract stats)}

\item{warning}{Logical. If TRUE prints warning massage if CERMINE specific letter conversion was performed}
}
\description{
Convert and unify most hex and some html coded letters in text to unicode characters and correct CERMINE specific errors in captured statistical results.
}
\examples{
x<-c("five &#x0003c; ten","five &lt; ten")
letter.convert(x)
}
