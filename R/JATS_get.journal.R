#' get.journal
#'
#' Extracts journal tag from NISO-JATS coded XML file or text.
#' @param x a NISO-JATS coded XML file or text.
#' @return Character string with the extracted journal name.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @export
#' @examples
#' x<-"Some text <journal-title>PLoS One</journal-title> some text"
#' get.journal(x)

get.journal<-function(x){
  # run prechecks or readLines(x) if x is file
  x<-preCheck(x)
  
if(length(grep("</journal-title",x))>0&length(grep("<journal-title",x))>0) {
  journal<-gsub(".*>","",gsub("<.*?>","",letter.convert(paste(gsub(".*<journal-title","",gsub("</journal-title>.*","",
         x[grep("<journal-title",x)[1]:grep("</journal-title",x)[1]]
         )),collapse=""))))
}else journal<-NA
if(nchar(journal)==0&!is.na(journal)) journal<-NA
journal
}

