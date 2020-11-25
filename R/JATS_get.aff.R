#' get.aff
#'
#' Extract affiliation tag/s from NISO-JATS coded XML file or text as vector of affiliations
#' @param x a NISO-JATS coded XML file or text
#' @param remove.html Logical. If TRUE removes all html tags
#' @param letter.convert Logical. If TRUE converts hex and html coded characters to unicode
#' @export
#' @examples
#' x<-"Some text <aff>Some affiliation</aff> some text"
#' get.aff(x)
#' x<-"Some text <aff>Some affiliation</aff> TEXT <aff>Some other affiliation</aff> Some text "
#' get.aff(x)

get.aff<-function(x,remove.html=FALSE,letter.convert=TRUE){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE)

if(length(grep("<aff",x))>0){
  x<-paste(x,collapse=" ")
  aff<-gsub("</aff.*","",unlist(strsplit(x,"<aff"))[-1])
  aff<-gsub("^ | $","",aff)
# remove <tags>
if(remove.html==T) aff<-gsub("^ | $","",gsub(".*>","",gsub(" ,",",",gsub("  "," ",gsub("<.*?.*>"," ",gsub("</.*?.*>",", ",aff))))))
# remove doubled spaces
  aff<-gsub("^ *|(?<= ) | *$", "", aff, perl = TRUE)  
  aff<-gsub(" ,",",",aff)
  aff<-gsub(", $|,$","",aff)
  aff<-gsub("^>","",aff)
if(letter.convert==TRUE) aff<-letter.convert(aff)
}else aff<-character(0)
return(aff)  
}

