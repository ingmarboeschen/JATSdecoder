% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/study.character.R
\name{study.character}
\alias{study.character}
\title{study.character}
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
  p2alpha = FALSE,
  alpha_output = "list",
  captions = TRUE,
  text.mode = 1,
  update.package.list = FALSE,
  add.software = NULL,
  quantileDF = 0.75,
  N.max.only = FALSE,
  output = "all",
  rm.na.col = TRUE
)
}
\arguments{
\item{x}{JATS coded XML file or JATSdecoder result}

\item{stats.mode}{Character. Select subset of standard stats. One of: "all", "checkable", "computable"}

\item{recalculate.p}{Logical. If TRUE recalculates p values (for 2 sided test) if possible}

\item{alternative}{Character. Select sidedness of recomputed p-values for t-, r- and Z-values. One of c("auto","undirected","directed","both"). If set to "auto" 'alternative' will be be set to 'both' if get.test.direction() detects one-directional hypotheses/tests in text. If no directional hypotheses/tests are dtected only "undirected" recomputed p-values will be returned}

\item{estimateZ}{Logical. If TRUE detected beta-/d-value is divided by reported standard error "SE" to estimate Z-value ("Zest") for observed beta/d and recompute p-value. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.}

\item{T2t}{Logical. If TRUE capital letter T is treated as t-statistic when extracting statistics with get.stats()}

\item{R2r}{Logical. If TRUE capital letter R is treated as correlation when extracting statistics with get.stats()}

\item{selectStandardStats}{Select specific standard statistics only (e.g.: c("t","F","Chi2"))}

\item{p2alpha}{Logical. If TRUE detects and extracts alpha errors denoted with critical p-value (what may lead to some false positive detections)}

\item{alpha_output}{One of "list" (list with elements: alpha_error, corrected_alpha, alpha_from_CI, alpha_max, alpha_min), vector with unique alpha errors but no distinction of types}

\item{captions}{Logical. If TRUE captions text will be scanned for statistical results}

\item{text.mode}{text parts to extract statistical results from (text.mode=1: abstract and full text, text.mode=2: method and result section, text.mode=3: result section only)}

\item{update.package.list}{if TRUE updates available R packages with available.packages() function}

\item{add.software}{additional software names to detect as vector}

\item{quantileDF}{quantile of (df1+1)+(df2+1) to extract for estimating sample size}

\item{N.max.only}{return only maximum of estimated sample sizes}

\item{output}{output selection of specific results c("all", "doi", "title", "year", "n.studies", "methods", "alpha.error", "power", "multi.comparison.correction", "assumptions", "OutlierRemovalInSD", "InteractionModeratorMediatorEffect", "test.direction", "sig.adjectives", "software", "Rpackage", "stats", "standardStats", "estimated.sample.size")}

\item{rm.na.col}{Logical. If TRUE removes all columns with only NA in extracted standard statistics}
}
\description{
extracts study characteristics out of a JATS coded XML file or JATSdecoder result
}
