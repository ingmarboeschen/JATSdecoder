#' get.journal
#'
#' Extracts journal tag from NISO-JATS coded XML file or text.
#' @param x a NISO-JATS coded XML file or text.
#' @return Character string with the extracted journal name.
#' @export
#' @examples
#' x<-"Some text <journal-title>PLoS One</journal-title> some text"
#' get.journal(x)

get.journal<-function(x){
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")

if(length(grep("</journal-title",x))>0&length(grep("<journal-title",x))>0) {
  journal<-gsub(".*>","",gsub("<.*?>","",letter.convert(paste(gsub(".*<journal-title","",gsub("</journal-title>.*","",
         x[grep("<journal-title",x)[1]:grep("</journal-title",x)[1]]
         )),collapse=""))))
}else journal<-NA
if(nchar(journal)==0&!is.na(journal)) journal<-NA
journal
}

