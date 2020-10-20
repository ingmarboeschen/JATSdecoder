#' get.stats
#'
#' Extract statistical results from text or NISO-JATS coded XML file as vector, extract standardStats and recalculate p-value if possible
#' @param x text or JATScoded XML file to extract statistical results from
#' @param output Select the desired output. One of c("both","allStats","standardStats")
#' @param stats.mode Select subset of standardStats. One of: "all", "checkable", "computable"
#' @param recalculate.p Logical. If TRUE recalculates p-values of standardStats if possible
#' @param alternative Character. Select sidedness of recomputed p-values from t-, r- and beta-values. One of c("undirected","directed","both")
#' @param estimateZ Logical. If TRUE detected beta-/d-value is divided by reported standard error "SE" to estimate Z-value ("Zest") for observed beta/d and recompute p-value. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE capital letter T is treated as small letter t
#' @param rm.na.col Logical. If TRUE removes all columns with only NA from standardStats
#' @param cermine Logical. If TRUE CERMINE specific letter conversion will be peformed on allStats results
#' @export
#' @examples
#' x<-c("The mean difference of scale A was significant (beta=12.9, t(18)=2.5, p<.05).",
#' "The ANOVA yielded significant results on 
#'  faktor A (F(2,18)=6, p<.05, eta(g)2<-.22)",
#' "the correlation of x and y was r=.37.")
#' get.stats(x)

get.stats<-function(x,output="both",stats.mode="all",recalculate.p=TRUE,alternative="undirected",estimateZ=FALSE,T2t=FALSE,rm.na.col=TRUE,cermine=FALSE){
# get text and abstract if x is file
  if(!is.list(x)) if(file.exists(x[1])){
   x<-unname(unlist(c(unlist(lapply(JATSdecoder(x,output="abstract"),function(x) text2sentences(x))),unlist(lapply(JATSdecoder(x,output="text")$text,text2sentences)))))
  }
# get text and abstract if x is JATSdecoder result
  if(is.list(x)){
    x<-c(unlist(lapply(x$abstract,text2sentences)),unlist(lapply(x$text,text2sentences)))
  }

# extract stats and standardStats
stats<-allStats(x)
if(output=="standardStats"|output=="both"){
if(cermine==TRUE) stats<-letter.convert(stats,cermine=cermine)
sStats<-standardStats(stats,stats.mode=stats.mode,recalculate.p=recalculate.p,alternative=alternative,T2t=T2t,rm.na.col=rm.na.col,estimateZ=estimateZ)
}
# output
if(output=="both") return(list(stats=stats,standardStats=sStats))
if(output=="stats")   return(stats)
if(output=="standardStats") return(sStats)
}
