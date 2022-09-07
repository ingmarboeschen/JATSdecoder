#' get.subject
#'
#' Extracts subject tag/s from NISO-JATS coded XML file or text as vector of subjects.
#' @param x a NISO-JATS coded XML file or text.
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode.
#' @param paste if paste!="" subject list is collapsed to one cell with seperator specified (e.g. paste=";").
#' @return Character vector with extracted subject/s.
#' @export
#' @examples
#' x<-"Some text <subject>Some subject</subject> some text"
#' get.subject(x)
#' x<-"Some text <subject>Some subject</subject> TEXT ...
#' <subject>Some other subject</subject> Some text "
#' get.subject(x)
#' get.subject(x,paste=", ")

get.subject<-function(x,letter.convert=TRUE,paste=""){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")

if(length(grep("<subject",x,value=TRUE))>0){
 # collapse to one row
 temp<-paste(x,collapse=" ")
 # split at "<subject>" and remove first line
 temp<-unlist(strsplit(temp,"<subject>"))[-1]
 # remove after "</subject.*" and html tags
 temp<-gsub("<.*?>",", ",gsub("</subject.*","",temp))
 # remove until ">" 
 temp<-gsub(".*>","",temp)
 # collapse with "," as seperator
 if(paste!="") temp<-paste(temp,collapse=", ")
 # clean up and split at comas
 while(length(grep(", , ",temp))>0) temp<-gsub(", , ",", ",temp)
 temp<-gsub("/",", ",sub(", $","",sub("^, ","",temp)))
 temp<-unlist(strsplit(temp," , |, "))
 if(letter.convert==T) temp<-letter.convert(temp)
# else NA
} else temp<-NA
return(unique(temp))
}


