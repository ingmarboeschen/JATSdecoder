#' get.stats
#'
#' Extracts statistical results from text string, XML, CERMXML, HTML or DOCX files. The result is a list with a vector containing all identified sticked results and a matrix containing the reported standard statistics and recalculated p-values if computation is possible.
#' @param x NISO-JATS coded XML or DOCX file path or plain textual content.
#' @param stats.mode Select a subset of test results by p-value checkability for output. One of: c("all", "checkable", "computable", "uncomputable").
#' @param recalculate.p Logical. If TRUE recalculates p-values of test results if possible.
#' @param alternative Character. Select sidedness of recomputed p-values from t-, r- and beta-values. One of c("undirected", "directed", "both").
#' @param estimateZ Logical. If TRUE detected beta-/d-value is divided by reported standard error "SE" to estimate Z-value ("Zest") for observed beta/d and recompute p-value. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE capital letter T is treated as t-statistic.
#' @param R2r Logical. If TRUE capital letter R is treated as correlation.
#' @param output Select the desired output. One of c("both", "allStats", "standardStats").
#' @param select Select specific standard statistics only (e.g.: c("t", "F", "Chi2")).
#' @param rm.na.col Logical. If TRUE removes all columns with only NA from standardStats.
#' @param cermine Logical. If TRUE CERMINE specific letter conversion will be peformed on allStats results.
#' @return If output="all": list with two elements. E1: vector of extracted results by \code{\link[JATSdecoder]{allStats}} and E2: matrix of standard results by \code{\link[JATSdecoder]{standardStats}}.\cr If output="allStats": vector of extracted results by \code{\link[JATSdecoder]{allStats}}.\cr If output="standardStats": matrix of standard results by \code{\link[JATSdecoder]{standardStats}}.
#' @source A minimal web application that extracts statistical results from single documents with \code{\link[JATSdecoder]{get.stats}} is hosted at: \href{https://www.get-stats.app}{https://www.get-stats.app/}
#' @source Statistical results extracted with \code{\link[JATSdecoder]{get.stats}} can be analyzed and used to identify articles stored in the PubMed Central library at: \href{https://www.scianalyzer.com}{https://www.scianalyzer.com/}. 
#' @references Böschen (2021). "Evaluation of JATSdecoder as an automated text extraction tool for statistical results in scientific reports.” \emph{Scientific Reports.} doi: \href{https://www.nature.com/articles/s41598-021-98782-3}{10.1038/s41598-021-98782-3}.
#' @seealso \code{\link[JATSdecoder]{study.character}} for extracting different study characteristics at once.
#' @export
#' @examples
#' ## Extract results from plain text input
#' x<-c("The mean difference of scale A was significant (beta=12.9, t(18)=2.5, p<.05).",
#' "The ANOVA yielded significant results on 
#'  faktor A (F(2,18)=6, p<.05, eta(g)2<-.22)",
#' "the correlation of x and y was r=.37.")
#' get.stats(x)
#' 
#' ## Extract results from native NISO-JATS XML file
#' # download example XML file via URL
#' x<-"https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
#' # file name
#' file<-paste0(tempdir(),"/file.xml")
#' # download URL as "file.xml" in tempdir()
#' download.file(x,file)
#' # apply get.stats() to file
#' get.stats(file)

get.stats<-function(x,output="both",
                    stats.mode="all",
                    recalculate.p=TRUE,
                    alternative="undirected",
                    estimateZ=FALSE,T2t=FALSE,R2r=FALSE,
                    select=NULL,
                    rm.na.col=TRUE,
                    cermine=FALSE){
# prechecks: is.null(), is.na()
  if(is.null(x)) return(list(stats=character(0),standardStats=character(0)))
  if(length(x)==1) if(is.na(x)) return(list(stats=character(0),standardStats=character(0)))
  
# get text and abstract if x is file
  if(!is.list(x)) if(file.exists(x[1])){
   #  if is website
  if(length(grep("xml$|html$|htm$",tolower(x)))==1) x<-html2text(x)
    #  if is docx
  if(length(grep("\\.docx$",tolower(x)))==1) x<-docx2text(x)
 #  x<-unname(c(unlist(lapply(JATSdecoder(x,output=c("abstract","text","captions")),function(x) text2sentences(x)))))
  }
# get text and abstract if x is JATSdecoder result
  if(is.list(x)){
    x<-c(unlist(lapply(x$abstract,text2sentences)),unlist(lapply(x$text,text2sentences)),unlist(lapply(x$captions,text2sentences)))
  }

# extract stats and standardStats
stats<-allStats(x)
if(output=="standardStats"|output=="both"){
if(cermine==TRUE) stats<-letter.convert(stats,cermine=cermine)
sStats<-standardStats(stats,stats.mode=stats.mode,recalculate.p=recalculate.p,alternative=alternative,T2t=T2t,R2r=R2r,rm.na.col=rm.na.col,estimateZ=estimateZ,select=select)
}
# output
if(output=="both") return(list(stats=stats,standardStats=sStats))
if(output=="stats")   return(stats)
if(output=="standardStats") return(sStats)
}


## import functions
docx2text<-function(file){
  # read DOCX
  if(length(grep("\\.docx$",tolower(file[1])))==1){
    a<-utils::unzip(file,"word/document.xml")
    x<-readLines(a,warn=FALSE,encoding="UTF-8")
    y<-invisible(x)
    # remove HTML
    y<-gsub("</[-\\=a-zA-Z0-9 ,;_\\:\\\\/\\'\"]*>","",y)
    y<-gsub("<[-\\=a-zA-Z,;_\\:\\\\/\\'\"]*>","",y)
    y<-gsub("<[a-z][-\\=a-zA-Z0-9 #\\.,;_\\:\\\\/\\'\"]*>","",y)
    
    # clean up white spaces
    y<-gsub("^ *|(?<= ) | *$", "", y, perl = TRUE)
    # remove empty lines
    y<-y[nchar(y)>0]
    y<-letter.convert(y)
    y<-paste(y,collapse=" ")
    y<-gsub("^ *|(?<= ) | *$", "", y, perl = TRUE)
    # clean up coma and point use
    y<-gsub(" \\. ", ". ", y)
    y<-gsub(" , ", ", ", y)
    y<-gsub(" ; ", "; ", y)
  }else stop("file is not in docx format")
  return(y)
}


# read HTML or cermxml
html2text<-function(file
#                    warn=TRUE # logical. If TRUE prints warnig to use study.character() when processing NISO-JATS documents
){
  w<-NULL
  if(length(grep("xml$|html$|htm$",tolower(file[1])))==1){
    # read HTML
    x<-readLines(file,warn=FALSE,encoding="UTF-8")
    y<-invisible(x)
    # is JATS coded document?
    if(length(grep("NLM//DTD",x[1:6]))>0){
      y<-JATSdecoder(file[1],output=c("abstract","sections","text","captions"))
      y<-unlist(lapply(y,text2sentences))
      }else{
    #  w<-1
    # remove HTML
    y<-gsub("<sub>"," ",y)
    y<-gsub("</[-\\=a-zA-Z0-9 ,;_\\:\\\\/\\'\"]*>","",y)
    y<-gsub("<[-\\=a-zA-Z,;_\\:\\\\/\\'\"]*>","",y)
    y<-gsub("<[a-z][-\\=a-zA-Z0-9 #\\.,;_\\:\\\\/\\'\"]*>","",y)
    # clean up white spaces
    y<-gsub("^ *|(?<= ) | *$", "", y, perl = TRUE)
    # remove empty lines
    y<-y[nchar(y)>0]
    y<-letter.convert(y)
    y<-paste(y,collapse=" ")
    y<-gsub("^ *|(?<= ) | *$", "", y, perl = TRUE)
    # clean up coma and point use
    y<-gsub(" \\. ", ". ", y)
    y<-gsub(" , ", ", ", y)
    y<-gsub(" ; ", "; ", y)
}
      }else stop("file is not in html format")
  #if(w==1&warn==TRUE) warning("The file is NISO-JATS coded.\n You might want to use study.character() to extract text and have further options.")
  return(y)
}

