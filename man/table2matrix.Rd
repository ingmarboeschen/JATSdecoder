% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helper_table2matrix.R
\name{table2matrix}
\alias{table2matrix}
\title{table2matrix}
\usage{
table2matrix(
  x,
  letter.convert = TRUE,
  greek2text = FALSE,
  replicate = FALSE,
  rm.duplicated = TRUE,
  rm.html = FALSE,
  rm.empty.rows = TRUE,
  collapseHeader = TRUE,
  header2colnames = FALSE,
  unifyMatrix = FALSE
)
}
\arguments{
\item{x}{HTML file or text, HTML tables as vector}

\item{letter.convert}{Logical. If TRUE hex codes will be unified and converted to utf-8 with JATSdecoder::letter.convert()}

\item{greek2text}{Logical. If TRUE and 'letter.convert=TRUE' converts and unifies various Greek letters to a text based form (e.g. 'alpha', 'beta')}

\item{replicate}{Logical. If TRUE the content of cells with row/col span is replicated in all connected cells, if FALSE disconnected cells will be empty,}

\item{rm.duplicated}{Logical. If TRUE duplicated rows are removed from output}

\item{rm.html}{Logical. If TRUE all HTML tags are removed, except <sub> and <sup> , </break> is converted to space}

\item{rm.empty.rows}{Logical. If TRUE empty rows are removed from output}

\item{collapseHeader}{Logical. If TRUE header cells are collapsed for each column if header has 2 or more lines}

\item{header2colnames}{Logical. If TRUE and 'collapseHeader=TRUE' first table row is used for column names and removed from table}

\item{unifyMatrix}{Logical. If TRUE matrix cells are further unified for better post processing.}
}
\value{
List with detected HTML tables as matrices.
}
\description{
Extracts and converts HTML tables to a list of R character matrices
}
\examples{
x<-readLines("https://en.wikipedia.org/wiki/R_(programming_language)",warn=FALSE)
tabs<-table2matrix(x)
}
