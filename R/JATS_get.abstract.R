#' get.abstract
#'
#' Extracts abstract tag from NISO-JATS coded XML file or text as vector of abstracts.
#' @param x a NISO-JATS coded XML file or text.
#' @param sentences Logical. If TRUE abstract is returned as vector of sentences.
#' @param remove.title Logical. If TRUE removes section titles in abstract.
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode.
#' @param cermine Logical. If TRUE and if 'letter.convert=TRUE' CERMINE specific letter correction is carried out (e.g. inserting of missing operators to statistical results).
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character. The abstract/s text as floating text or vector of sentences.
#' @export
#' @examples
#' x<-"Some text <abstract>Some abstract</abstract> some text"
#' get.abstract(x)
#' x<-"Some text <abstract>Some abstract</abstract> TEXT <abstract with subsettings>
#' Some other abstract</abstract> Some text "
#' get.abstract(x)

get.abstract<-function(x,sentences=FALSE,remove.title=TRUE,letter.convert=TRUE,cermine=FALSE){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")
# case 1
if(length(grep("<abstract",x,value=TRUE))>0) {
temp<-paste(x,collapse=" ")
temp<-unlist(strsplit(temp,"<abstract"))[-1]
# remove behind end of abstract
temp<-gsub("</abstract.*","",temp)
# remove start with ">" for <abtract> and till first \"> for abstracts with type and id definition
ifelse(substr(temp,1,1)==">",temp<-gsub("^>","",temp),temp<-paste(unlist(strsplit2(temp,">","after"))[-1],collapse=""))
# remove <p> tag temp
temp<-gsub("</p>","",gsub("<p>","",temp))
# remove <italic> tag 
temp<-gsub("</italic>","",gsub("<italic>","",temp))
# if has no </abstract
 if(length(grep("</abstract",x))==0) temp<-gsub("</sec></sec>.*","",temp)
# remove abstract section titles
if(remove.title==T){
  if(length(grep("<title>",temp))>0) temp<-gsub("<title>.*?.*</title>","",temp)
  }
# split abstract sections to vektor (if possible)
if(remove.title==F){
  if(length(grep("<title>",temp))>0) 
  temp<-gsub("</title>",": ",unlist(strsplit(temp,"<title>")))[-1]
  }  
# remove everything between <..>
temp<-gsub("<[/a-z].*?.*[\"a-z]>"," ",temp)
# remove declaration of interest
temp<-gsub("None. $|None.$","",temp)
# remove double white spaces and white space in front and end
temp<-gsub("^ *|(?<= ) | *$", "", temp, perl = TRUE)
if(temp[1]==""&!is.na(temp[1])) temp<-NA
# clean up spaces in front of signs
temp<-gsub(" \\.",".",gsub(" ,",",",temp))
temp<-gsub(" [)]",")",gsub("[(] ","(",temp))
#convert letters
if(letter.convert==TRUE) temp<-letter.convert(temp,cermine=cermine)
# clean up
# Convert figure only to NA
if(length(grep("^fig. [0-9]|^fig [0-9]|^fig. i|^figs. [0-9]|^figure [0-9]|^figure i|^figures [0-9]|^p\\[|^p[0-9]^plate [0-9]|^plate I",tolower(temp)))>0) temp<-NA
if(length(grep("^Supplemental Digital Content",temp))>0) temp<-NA
# collapse to one cell
temp<-paste(temp,collapse=" ")
# if no <abstract> tag
}else{
   temp<-NA}
if(!exists("temp")) temp<-NA
if(is.na(temp)|temp=="NA") temp<-NA
if(sentences==TRUE&!is.na(temp)) temp<-text2sentences(temp)
return(temp)
}

