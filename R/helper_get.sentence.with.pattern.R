#' get.sentence.with.pattern
#'
#' Return lines with search term patterns
#' @param x text to process
#' @param patterns search terms
#' @param tolower Logical. If TRUE converts search terms and text to lower case
#' @export
#' @examples
#' text<-c("This demo demonstrates how get.sentence.with.pattern works.","The is a simple 0, 1.")
#' get.sentence.with.pattern(text,c("Demo","example","work"))
#' get.sentence.with.pattern(text,c("Demo","example","work"),tolower=TRUE)

get.sentence.with.pattern<-function(x,patterns=c(""),tolower=TRUE){
pat<-paste(patterns,collapse="|")
x<-as.character(x)
if(tolower==T) temp<-x[grep(tolower(pat),tolower(x))]
if(tolower!=T) temp<-x[grep(pat,x)]
return(temp)
}
