#' get.keywords
#'
#' Extracts keyword tag/s from NISO-JATS coded XML file or text as vector of keywords.
#' @param x a NISO-JATS coded XML file or text.
#' @param paste if paste!="" keyword list is collapsed to one cell with seperator specified (e.g. paste=";").
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode.
#' @param include.max a maximum number of keywords to extract.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character vector with extracted keyword/s.
#' @export
#' @examples
#' x<-"Some text <kwd>Keyword 1</kwd>, <kwd>Keyword 2</kwd> some text"
#' get.keywords(x)
#' get.keywords(x,paste(", "))

get.keywords<-function(x,paste="",letter.convert=TRUE,include.max=length(keyword)){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")

if(length(grep("</kwd",x))>0) {
 keyword<-paste(x,collapse=" ")
 if(!is.null(unlist(strsplit(keyword,"</kwd>")))){
    # split and clean up
    keyword<-gsub("</kwd.*","",gsub(".*<kwd","",unlist(strsplit(keyword,"</kwd>"))))
    # select keywords only
    keyword<-keyword[-length(keyword)]
    # clean up
    keyword<-gsub("<.*?.*>","",keyword)
    keyword<-gsub(".*>","",keyword)
    keyword<-unique(keyword)
    if(length(keyword)>include.max) keyword<-keyword[1:include.max]
    if(paste!="") keyword<-paste(keyword,collapse=paste)
    if(letter.convert==T) keyword<-letter.convert(keyword)
# if starts with coma and/or space delete it
#    keyword<-gsub("^, |^ |^,","",keyword)

    }else keyword<-NA
  }else keyword<-NA
return(keyword)
}

#get.keywords(x,,12)
