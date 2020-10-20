#' get.category
#'
#' Extract category tag/s from NISO-JATS coded XML file or text as vector of categories
#' @param x a NISO-JATS coded XML file or text
#' @export
#' @examples
#' x<-"Some text <article-categories>Some category</article-categories> some text"
#' get.category(x)

get.category<-function(x){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE)

if(length(grep("<article-categories>",x,value=TRUE))>0){
 # collapse to one row
 temp<-paste(x,collapse=" ")
 # split at "<article-categories>" and remove first line
 temp<-unlist(strsplit(temp,"<article-categories>"))[-1]
 # remove after "</article-categories.*" and html tags
 temp<-gsub("<.*?>",", ",gsub("</article-categories.*","",temp))
 # clean up comas
 while(length(grep(", , ",temp))>0) temp<-gsub(", , ",", ",temp)
 temp<-gsub("/",", ",sub(", $","",sub("^, ","",temp)))
# else NA
} else temp<-NA
return(temp)
}

