#' get.title
#'
#' Extracts article title from NISO-JATS coded XML file or text.
#' @param x a NISO-JATS coded XML file or text.
#' @return Character string with extracted article title.
#' @export
get.title<-function(x){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")
# extract title
if(length(grep("</article-title",x))>0&length(grep("<article-title",x))>0) {
  title<-gsub("<.*?>","",letter.convert(paste(gsub(".*<article-title","",gsub("</article-title>.*","",
         x[grep("<article-title",x)[1]:grep("</article-title",x)[1]]
         )),collapse=" ")))
         # remove ">" at start or remove till ">"
         ifelse(substr(title,1,1)==">", title<-gsub("^>","",title),title<-paste(unlist(strsplit2(title,">","after"))[-1],collapse=""))
  title<-gsub("\\\"|\\n","",title)
  }else title<-NA
if(nchar(title)==0&!is.na(title)) title<-NA
return(title)
}

