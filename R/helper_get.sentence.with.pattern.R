#' get.sentence.with.pattern
#'
#' Returns lines with search term patterns.
#' @param x sentence vector to process.
#' @param patterns search terms.
#' @param tolower Logical. If TRUE converts search terms and text to lower case.
#' @return Character. Vector with sentences, that contain search pattern.
#' @keywords internal
#' @export
#' @examples
#' text<-c("This demo", "demonstrates how", "get.sentence.with.pattern works.")
#' get.sentence.with.pattern(text,c("Demo","example","work"))
#' get.sentence.with.pattern(text,c("Demo","example","work"),tolower=FALSE)

get.sentence.with.pattern<-function(x,patterns=c(""),tolower=TRUE){
pat<-paste(patterns,collapse="|")
x<-as.character(x)
if(tolower==T) temp<-x[grep(tolower(pat),tolower(x))]
if(tolower!=T) temp<-x[grep(pat,x)]
return(temp)
}
