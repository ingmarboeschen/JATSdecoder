#' get.stats
#'
#' Extracts statistical results from text string, XML, CERMXML, HTML or DOCX files. The result is a list with a vector containing all identified sticked results and a matrix containing the reported standard statistics and recalculated p-values if computation is possible.
#' @param x NISO-JATS coded XML or DOCX file path or plain textual content.
#' @param stats.mode Select a subset of test results by p-value checkability for output. One of: c("all", "checkable", "computable", "uncomputable").
#' @param recalculate.p Logical. If TRUE recalculates p-values of test results if possible.
#' @param checkP Logical. If TRUE observed and recalculated p-values are checked for consistency.
#' @param alpha Numeric. Defines the alpha level to be used for error assignment.
#' @param criticalDif Numeric. Sets the absolute maximum difference in reported and recalculated p-values for error detection.
#' @param alternative Character. Select test sidedness for recomputation of p-values from t-, r- and beta-values. One of c("undirected", "directed"). If "directed" is specified, p-values for directed null-hypothesis are added to the table but still require a manual inspection on consistency of the direction.
#' @param estimateZ Logical. If TRUE detected beta-/d-value is divided by reported standard error "SE" to estimate Z-value ("Zest") for observed beta/d and recompute p-value. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE capital letter T is treated as t-statistic.
#' @param R2r Logical. If TRUE capital letter R is treated as correlation.
#' @param output Select the desired output. One of c("both", "allStats", "standardStats").
#' @param select Select specific standard statistics only (e.g.: c("t", "F", "Chi2")).
#' @param rm.na.col Logical. If TRUE removes all columns with only NA from standardStats.
#' @param cermine Logical. If TRUE CERMINE specific letter conversion will be peformed on allStats results.
#' @param warnings Logical. If FALSE warning messages are omitted. 
#' @return If output="all": list with two elements. E1: vector of extracted results by \code{\link[JATSdecoder]{allStats}} and E2: matrix of standard results by \code{\link[JATSdecoder]{standardStats}}.\cr If output="allStats": vector of extracted results by \code{\link[JATSdecoder]{allStats}}.\cr If output="standardStats": matrix of standard results by \code{\link[JATSdecoder]{standardStats}}.
#' @note In order to extract statistical test results reported in simple/unpublished PDF documents with get.stats(), the function \emph{pdf_text()} from the \href{https://CRAN.R-project.org/package=pdftools/}{pdftools} package may help to extract textual content from pdf files (be aware that tabled content may cause corrupt text).
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
#' # download example XML file via URL if a connection is possible
#' x<-"https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
#' # file name
#' file<-paste0(tempdir(),"/file.xml")
#' # download URL as "file.xml" in tempdir() if a connection is possible
#' tryCatch({
#'   readLines(x,n=1)
#'   download.file(x,file)
#'   },
#'   warning = function(w) message(
#'   "Something went wrong. Check your internet connection and the link address."),
#'   error = function(e) message(
#'   "Something went wrong. Check your internet connection and the link address.")
#' )
#' # apply get.stats() to file
#' if(file.exists(file)) get.stats(file)

get.stats<-function(x,output="both",
                    stats.mode="all",
                    recalculate.p=TRUE,
                    checkP=FALSE,
                    alpha=.05,
                    criticalDif=.02,
                    alternative="undirected",
                    estimateZ=FALSE,T2t=FALSE,R2r=FALSE,
                    select=NULL,
                    rm.na.col=TRUE,
                    cermine=FALSE,
                    warnings=TRUE){
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
sStats<-standardStats(stats,stats.mode=stats.mode,recalculate.p=recalculate.p,alternative=alternative,T2t=T2t,R2r=R2r,rm.na.col=rm.na.col,estimateZ=estimateZ,select=select,warnings=warnings)
}

## check p-values on consistency with recalculated p-values 
if(checkP==TRUE&recalculate.p==TRUE&output!="allStats") sStats<-pCheck(sStats,alpha=alpha,criticalDif=criticalDif,warnings=warnings)

# output
if(output=="both") return(list(stats=stats,standardStats=sStats))
if(output=="allStats")   return(stats)
if(output=="standardStats") return(sStats)
}


## import functions
docx2text<-function(file){
  # read DOCX
  if(length(grep("\\.docx$",tolower(file[1])))==1){
    a<-utils::unzip(file,"word/document.xml")
    x<-readLines(a,warn=FALSE,encoding="UTF-8")
    y<-invisible(x)
    
    # remove first technical line
    if(grepl("<\\?xml",y[1])&nchar(y[1])<60)
      y<-y[-1]
    if(length(y)==0) return(NULL)
    
    # remove tables
    y<-paste0(gsub("<w:tbl>.*</w:tbl>","",
         unlist(strsplit2(y,"<w:tbl>","before"))),collapse=" ")
    # end of paragraph to space
    y<-gsub("</w:p>"," ",y)
    # remove HTML
    y<-gsub("</[-\\=a-zA-Z0-9 ,;_\\:\\\\/\\'\"]*>","",y)
    y<-gsub("<[-\\=a-zA-Z,;_\\:\\\\/\\'\"]*>","",y)
    y<-gsub("<[a-z][-\\=a-zA-Z0-9 #\\.,;_\\:\\\\/\\'\"]*>","",y)
    
    # clean up white spaces
    y<-gsub("^ *|( ) *| *$", "\\1", y, perl = TRUE)
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
    # is JATS coded document?
    if(length(grep("NLM//DTD",x[1:6]))>0){
      y<-invisible(x)
      y<-JATSdecoder(file[1],output=c("abstract","sections","text","captions"))
      y<-unlist(lapply(y,text2sentences))
      }else{
        y<-invisible(x)
        y<-paste(y,collapse=" ")
        # remove tables
        y<-paste0(gsub("<table.*>.*</table>","",
                       unlist(strsplit2(y,"<table[- >]","before"))),collapse=" ")
        # remove scripts/button/style/link
        y<-paste0(gsub("<scrip[^>]*>.*</script>","",
                       unlist(strsplit2(y,"<script[- >]","before"))),collapse=" ")
        y<-paste0(gsub("<style[^>]*>.*</style>","",
                       unlist(strsplit2(y,"<style[- >]","before"))),collapse=" ")
        y<-paste0(gsub("<butto[^>]*>.*</button>","",
                       unlist(strsplit2(y,"<button[- >]","before"))),collapse=" ")
        y<-paste0(gsub("<link[^>]*>.*</link>|<link [^>]*>","",
                       unlist(strsplit2(y,"<link[- >]","before"))),collapse=" ")
        #remove styles
        y<-gsub(">[\\.@][a-z][^\\{]*\\{[a-z].*\\}\\}",">",y)
        y<-gsub(">[\\.@][a-z][^\\{]*\\{[a-z][^\\{]*\\}",">",y)
        
        #unlist(strsplit(y,"fetc"))[1]

        # replace specific tags
        y<-gsub("<sub>","_",y)
        y<-gsub("<sup>","^",y)
        y<-gsub("</*br/*>"," ",y)
    
        # remove all other HTML tags
        y<-gsub("</*-*[A-z!][^>]*>","",y)

        y<-gsub("</[-\\=a-zA-Z0-9 ,;_\\:\\\\/\\'\"]*>","",y)
        y<-gsub("<[-\\=a-zA-Z,;_\\:\\\\/\\'\"]*>","",y)
        y<-gsub("<[a-z][-\\=a-zA-Z0-9 #\\.,;_\\:\\\\/\\'\"]*>","",y)
        # clean up white spaces
        y<-gsub("  *", " ", y)
        y<-gsub("^ *| *$", "", y, perl = TRUE)
        # remove empty lines
        y<-y[nchar(y)>0]
        y<-letter.convert(y)
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


