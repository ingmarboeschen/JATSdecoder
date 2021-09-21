#' get.tables
#'
#' extracts HTML tables as vector of tables
#' @param x HTML file or html text
#' @export

get.tables<-function(x){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE)
tables<-character(0)
if(sum(grep("</table>",x))>0){
# split lines with table
tables<-paste(x,collapse=" ")
tables<-unlist(strsplit2(tables,"<table>|<table frame","before"))
tables<-unlist(strsplit2(tables,"</table>","after"))
# select lines with </caption>
tables<-grep("<table>|<table frame|</table>",tables,value=TRUE)
} 
return(unlist(tables))
}
