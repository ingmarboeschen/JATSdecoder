#' get.tables
#'
#' Extracts HTML tables as vector of tables.
#' @param x HTML file or text with html tags.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character vector with extracted HTML tables.
#' @export

get.tables<-function(x){
  # run prechecks or readLines(x) if x is file
  x<-JATSdecoder::preCheck(x)
  # remove newline sign
  x<-gsub("\\n"," ",x)
  
  tables<-character(0)
  if(sum(grep("<table[-> ]",x))>0){
    # without table wrap
    if(sum(grep("</table-wrap[> ]",x))==0){
      # split collapsed lines at <table
      tables<-paste(x,collapse=" ")
      tables<-unlist(JATSdecoder::strsplit2(tables,"<table>|<table [a-z]","before"))
      tables<-unlist(JATSdecoder::strsplit2(tables,"</table>","after"))
      # select lines with <table>
      tables<-grep("<table>|<table [a-z]|</table>",tables,value=TRUE)
    }
    
    # with table wrap
    if(sum(grep("<table-wrap[> ]",x))>0){
      # split collapsed lines at <table-wrap
      tables<-paste(x,collapse=" ")
      tables<-unlist(JATSdecoder::strsplit2(tables,"<table-wrap[> ]","before"))
      tables<-unlist(JATSdecoder::strsplit2(tables,"</table-wrap>","after"))
      # select lines with table wrap
      tables<-grep("<table-wrap[> ]",tables,value=TRUE)
    }
    
  } 
  return(unlist(tables))
}

