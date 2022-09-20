#' get.doi
#'
#' Extracts articles doi from NISO-JATS coded XML file or text.
#' @param x a NISO-JATS coded XML file or text.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character string with the extracted doi.
#' @export

## get.link
get.doi<-function(x){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")
# with doi
if(length(grep("<article-id pub-id-type=\"doi\">",x,value=TRUE))>0){
  temp<-paste("https://doi.org/",sub(".*>","",sub("</article-id>.*","",sub(".*<article-id pub-id-type=\"doi\">","",grep("<article-id pub-id-type=\"doi\">",x,value=TRUE)[1]))),sep="")
}else
# from pmc
if(length(grep("pub-id-type=\"pmc\">",x,value=TRUE))>0){
  temp<-paste("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC",gsub("<.*","",gsub(".*pub-id-type=\"pmc\">","",grep("pub-id-type=\"pmc\">",x,value=TRUE)[1])),sep="")
# else NA
}else temp<-NA

# correct some errors
temp<-gsub("/$|;$","",temp)

# set empty link to NA
if(!is.na(temp)&nchar(temp)<5) temp<-NA
return(temp)
}
