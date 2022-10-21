#' get.type
#'
#' Extracts article type from NISO-JATS coded XML file or text.
#' @param x a NISO-JATS coded XML file or text.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character string with extracted article type.
#' @export

## get.type
get.type<-function(x){
  # run prechecks or readLines(x) if x is file
  x<-preCheck(x)
  
if(length(grep("article-type=",x))>0){
  type<-tolower(gsub("^ ","",gsub(" $","",gsub("\\\".*","",gsub("\">.*","",gsub(".*article-type=\"","",grep("article-type=",x,value=TRUE)[1]))))))
 }else type<-NA
return(type)
}
