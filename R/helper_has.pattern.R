#' has.pattern
#'
#' Return search term hit vector for all search patterns
#' @param x text to process
#' @param patterns search terms
#' @param tolower Logical. If TRUE converts search terms and text to lower case
#' @export
#' @examples
#' text<-c("This demo demonstrates how has.pattern() works.",
#'         "The result is a simple 0, 1 coded vector for all search patterns.")
#' has.pattern(text,c("Demo","example","work"))
#' has.pattern(text,c("Demo","example","work"),tolower=TRUE)


has.pattern<-function(x,patterns=c(""),tolower=TRUE){
res<-NULL
for(i in 1:length(patterns)){
if(tolower==T) res[i]<-ifelse(length(grep(patterns[i],tolower(x)))>0,1,0)
if(tolower!=T) res[i]<-ifelse(length(grep(patterns[i],x))>0,1,0)
}
names(res)<-patterns
res
}

