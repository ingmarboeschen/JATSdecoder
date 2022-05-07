#' get.references
#'
#' Extract reference list from NISO-JATS coded XML file or text as vector of references
#' @param x a NISO-JATS coded XML file or text
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode
#' @param remove.html Logical. If TRUE removes all HTML tags
#' @param extract part of refernces to extract (one of "full" or "title")
#' @export
get.references<-function(x,letter.convert=FALSE, remove.html=FALSE,extract="full"){ 
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")

if(length(grep("<ref-list",x))>0){
  temp<-paste(x,collapse=" ")
# extract content within ref-list tag
  temp<-gsub(".*<ref-list","",gsub("</ref-list.*","",temp))
# split at end of each reference
  temp<-gsub("^>","",unlist(strsplit(temp,"<ref id")))
# remove empty lines (nchar<50)
  temp<-temp[nchar(temp)>50]
# extract title
if(extract=="title"){
    # with title tag
    if(length(grep("title>",temp))>0) temp[grep("title>",temp)]<-
     gsub("</.*"," ",gsub(".*?title>(.+)","\\1",temp[grep("title>",temp)]))
    # without title tag
    if(length(grep("source>",temp))>0) temp[grep("source>",temp)]<-gsub("</.*"," ",gsub(".*?source>(.+)","\\1",temp[grep("source>",temp)]))
    # clean up
    temp<-gsub(" $|^ ","",gsub("<ext.*|</mixed.*","",gsub(".*other\\\">","",temp)))
  }


if(remove.html==T){
# replace </name> and </surname>
  temp<-gsub("</name>",", ",gsub("</surname>",", ",temp))
# remove html
  temp<-gsub(".*\">","",gsub("<.*?.*>"," ",temp))
# remove white spaces
  temp<-gsub("^ *|(?<= ) | *$", "", temp,perl = TRUE)
# correct brackets, comma and dot
  temp<-gsub(" \\.","\\.",gsub(" ,",",",gsub("\\( ","\\(",gsub(" \\)","\\)",temp))))
  temp<-gsub(",,",",",gsub(", \\& |, \\&amp; ","; ",temp))
  temp<-gsub("[\\.],",".;",temp)
  }
  
# remove ^Revision recieved
temp<-grep("^Revision received",temp,invert=TRUE,value=TRUE)  
# remove ^Recieved|^Accepted if is last line
if(length(grep("^Received |^Accepted ",temp[length(temp)]))>0) temp<-temp[-1*length(temp)]
if(letter.convert==T) temp<-letter.convert(temp)
}else temp<-NA
return(temp)
}
  
