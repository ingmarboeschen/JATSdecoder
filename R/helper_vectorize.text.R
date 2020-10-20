#' vectorize.text
#'
#' Converts text to vector of words
#' @param x text to vectorize
#' @export
#' @examples
#' text<-"This demo demonstrates how vectorize.text() works."
#' vectorize.text(text)

vectorize.text<-function(x){
x<-unlist(x)
x<-strsplit(x," ")
#lapply(x,function(x) paste(x,collapse=" "))
return(x)
}
