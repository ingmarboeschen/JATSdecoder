#' which.term
#'
#' Returns search element/s from vector that is/are present in text or returns search term hit vector for all terms
#' @param x text to process
#' @param terms search terms
#' @param tolower Logical. If TRUE converts search terms and text to lower case
#' @param hits_only Logical. If TRUE returns search pattern/s, that were found in text and not a search term hit vector
#' @export
#' @examples
#' text<-c("This demo demonstrates how which.term works.",
#'        "The result is a simple 0, 1 coded vector for all search patterns or 
#'         a vector including the identified patterns only.")
#' which.term(text,c("Demo","example","work"))
#' which.term(text,c("Demo","example","work"),tolower=TRUE,hits_only=TRUE)

which.term<-function(x,terms,tolower=TRUE,hits_only=FALSE){
res<-NULL
if(tolower==TRUE){terms<-tolower(terms); x<-tolower(x)}
for(i in 1:length(terms)) res[i]<-ifelse(length(grep(terms[i],x))>0,1,0)
names(res)<-terms
if(hits_only==T) res<-names(res[res==1])
return(res)
}
