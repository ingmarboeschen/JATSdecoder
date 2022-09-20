#' get.aff
#'
#' Extracts the affiliation tag information from NISO-JATS coded XML file or text as a vector of affiliations.
#' @param x a NISO-JATS coded XML file or text.
#' @param remove.html Logical. If TRUE removes all html tags.
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character vector with the extracted affiliation name/s.
#' @export
#' @examples
#' x<-"Some text <aff>Some affiliation</aff> some text"
#' get.aff(x)
#' x<-"TEXT <aff>Some affiliation</aff> TEXT <aff>Some other affiliation</aff> TEXT"
#' get.aff(x)

get.aff<-function(x,remove.html=FALSE,letter.convert=TRUE){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")

if(length(grep("<aff",x))>0){
  x<-paste(x,collapse=" ")
  aff<-gsub("</aff.*","",unlist(strsplit(x,"<aff"))[-1])
  aff<-gsub("^ | $","",aff)
# remove label<tag>
if(remove.html==T) aff<-gsub("<label>.*</label>",", ",aff)
# remove <tags>
if(remove.html==T) aff<-gsub("^ | $","",gsub(".*>","",gsub(" ,|, [;,]",",",gsub("  "," ",gsub("<.*?.*>"," ",gsub("</*?.*>",", ",aff))))))
# remove doubled spaces
  aff<-gsub("^ *|(?<= ) | *$", "", aff, perl = TRUE)  
  if(letter.convert==TRUE) aff<-letter.convert(aff)
  # coma clean up 
  aff<-gsub(",[ ,]*",", ",aff)
  aff<-gsub(", $|,$|^, ","",aff)
aff<-gsub("^>|^ ","",aff)
}else aff<-character(0)
return(aff)  
}

