#' get.doi
#'
#' Extract articles doi from NISO-JATS coded XML file or text
#' @param x a NISO-JATS coded XML file or text
#' @export

## get.link
get.doi<-function(x){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE)
# with doi
if(length(grep("<article-id pub-id-type=\"doi\">",x,value=TRUE))>0){
  temp<-paste("https://doi.org/",sub(".*>","",sub("</article-id>.*","",sub(".*<article-id pub-id-type=\"doi\">","",grep("<article-id pub-id-type=\"doi\">",x,value=TRUE)[1]))),sep="")
}else
# from pmc
if(length(grep("pub-id-type=\"pmc\">",x,value=TRUE))>0){
  temp<-paste("https://www.ncbi.nlm.nih.gov/pmc/articles/PMC",gsub("<.*","",gsub(".*pub-id-type=\"pmc\">","",grep("pub-id-type=\"pmc\">",x,value=TRUE)[1])),sep="")
# else NA
}else temp<-NA

# set empty link to NA
if(!is.na(temp)&nchar(temp)<5) temp<-NA
temp
}
