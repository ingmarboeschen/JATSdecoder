#' get.keywords
#'
#' Extract keyword tag/s from NISO-JATS coded XML file or text as vector of keywords
#' @param x a NISO-JATS coded XML file or text
#' @param paste if paste!="" author vector is collapsed to one cell
#' @param letter.convert Logical. If TRUE converts hex and html coded characters to unicode
#' @param include.max a maximum number of keywords to extract
#' @export
#' @examples
#' x<-"Some text <kwd>Keyword 1</kwd>, <kwd>Keyword 2</kwd> some text"
#' get.keywords(x)
#' get.keywords(x,paste(", "))

get.keywords<-function(x,paste="",letter.convert=TRUE,include.max=length(keyword)){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE)

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
