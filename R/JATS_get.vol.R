#' get.vol
#'
#' Extract volume, first and last page from NISO-JATS coded XML file or text
#' @param x a NISO-JATS XML coded file or text
#' @export

get.vol<-function(x){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE)

x<-paste(x,collapse=" ")
x<-gsub("<abstract.*|<body.*","",x)
vol<-gsub("</.*","",unlist(strsplit(x,"<volume>"))[2])
fpage<-gsub("</.*","",unlist(strsplit(x,"<fpage>"))[2])
lpage<-gsub("</.*","",unlist(strsplit(x,"<lpage>"))[2])
return(c(vol=vol,fpage=fpage,lpage=lpage))
}
