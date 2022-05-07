#' get.type
#'
#' Extract article-type tag from NISO-JATS coded XML file or text
#' @param x a NISO-JATS coded XML file or text
#' @export

## get.type
get.type<-function(x){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")

if(length(grep("article-type=",x))>0){
  type<-tolower(gsub("^ ","",gsub(" $","",gsub("\\\".*","",gsub("\">.*","",gsub(".*article-type=\"","",grep("article-type=",x,value=TRUE)[1]))))))
 }else type<-NA
return(type)
}
