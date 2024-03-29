% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study.character.R
\name{study.character}
\alias{study.character}
\title{study.character}
\source{
An interactive web application for selecting and analyzing extracted article metadata and study characteristics for articles linked to PubMed Central is hosted at: \href{https://www.scianalyzer.com}{https://www.scianalyzer.com/}

The XML version of PubMed Central database articles can be downloaded in bulk from: \cr\href{https://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/}{https://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/}
}
\usage{
study.character(
  x,
  stats.mode = "all",
  recalculate.p = TRUE,
  alternative = "auto",
  estimateZ = FALSE,
  T2t = FALSE,
  R2r = FALSE,
  selectStandardStats = NULL,
  checkP = TRUE,
  criticalDif = 0.02,
  alpha = 0.05,
  p2alpha = TRUE,
  alpha_output = "list",
  captions = TRUE,
  text.mode = 1,
  update.package.list = FALSE,
  add.software = NULL,
  quantileDF = 0.9,
  N.max.only = FALSE,
  output = "all",
  rm.na.col = TRUE
)
}
\arguments{
\item{x}{NISO-JATS coded XML file.}

\item{stats.mode}{Character. Select subset of standard stats. One of: c("all", "checkable", "computable").}

\item{recalculate.p}{Logical. If TRUE recalculates p values (for 2 sided test) if possible.}

\item{alternative}{Character. Select sidedness of recomputed p-values for t-, r- and Z-values. One of c("auto", "undirected", "directed"). If set to "auto" 'alternative' will be be set to 'directed' if get.test.direction() detects one-directional hypotheses/tests in text. If no directional hypotheses/tests are dtected only "undirected" recomputed p-values will be returned.}

\item{estimateZ}{Logical. If TRUE detected beta-/d-value is divided by reported standard error "SE" to estimate Z-value ("Zest") for observed beta/d and recompute p-value. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.}

\item{T2t}{Logical. If TRUE capital letter T is treated as t-statistic when extracting statistics with get.stats().}

\item{R2r}{Logical. If TRUE capital letter R is treated as correlation when extracting statistics with get.stats().}

\item{selectStandardStats}{Select specific standard statistics only (e.g.: c("t", "F", "Chi2")).}

\item{checkP}{Logical. If TRUE observed and recalculated p-values are checked for consistency.}

\item{criticalDif}{Numeric. Sets the absolute maximum difference in reported and recalculated p-values for error detection.}

\item{alpha}{Numeric. Defines the alpha level to be used for error assignment of detected incosistencies.}

\item{p2alpha}{Logical. If TRUE detects and extracts alpha errors denoted with critical p-value (what may lead to some false positive detections).}

\item{alpha_output}{One of c("list", "vector"). If alpha_output = "list" a list with elements: alpha_error, corrected_alpha, alpha_from_CI, alpha_max, alpha_min is returned. If alpha_output = "vector" unique alpha errors without a distinction of types is returned.}

\item{captions}{Logical. If TRUE captions text will be scanned for statistical results.}

\item{text.mode}{Numeric. Defines text parts to extract statistical results from (text.mode=1: abstract and full text, text.mode=2: method and result section, text.mode=3: result section only).}

\item{update.package.list}{Logical. If TRUE updates available R packages with utils::available.packages() function.}

\item{add.software}{additional software names to detect as vector.}

\item{quantileDF}{quantile of (df1+1)+(df2+1) to extract for estimating sample size.}

\item{N.max.only}{return only maximum of estimated sample sizes.}

\item{output}{output selection of specific results c("doi", "title", "year", "Nstudies", \cr
"methods", "alpha_error", "power", "multi_comparison_correction", \cr
"assumptions", "OutlierRemovalInSD", "InteractionModeratorMediatorEffect", \cr
"test_direction", "sig_adjectives", "software", "Rpackage", "stats", \cr
"standardStats", "estimated_sample_size").}

\item{rm.na.col}{Logical. If TRUE removes all columns with only NA in extracted standard statistics.}
}
\value{
List with extracted study characteristics.
}
\description{
Extracts study characteristics out of a NISO-JATS coded XML file. Use \href{https://github.com/CeON/CERMINE}{CERMINE} to convert PDF to CERMXML files.
}
\note{
A short tutorial on how to work with JATSdecoder and the generated outputs can be found at: \href{https://github.com/ingmarboeschen/JATSdecoder}{https://github.com/ingmarboeschen/JATSdecoder}
}
\examples{
# download example XML file via URL
x<-"https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
# file name
file<-paste0(tempdir(),"/file.xml")
# download URL as "file.xml" in tempdir() if a connection is possible
tryCatch({
readLines(x,n=1)
download.file(x,file)
},
warning = function(w) message(
  "Something went wrong. Check your internet connection and the link address."),
error = function(e) message(
  "Something went wrong. Check your internet connection and the link address."))
# convert full article to list with study characteristics
if(file.exists(file)) study.character(file)
}
\references{
Böschen (2023). "Evaluation of the extraction of methodological study characteristics with JATSdecoder.” \emph{Scientific Reports.} doi: \href{https://www.nature.com/articles/s41598-022-27085-y}{10.1038/s41598-022-27085-y}.

Böschen (2021). "Evaluation of JATSdecoder as an automated text extraction tool for statistical results in scientific reports.” \emph{Scientific Reports.} doi: \href{https://www.nature.com/articles/s41598-021-98782-3}{10.1038/s41598-021-98782-3}.
}
\seealso{
\code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.

\code{\link[JATSdecoder]{get.stats}} for extracting statistical results from textual input and different file formats.
}
