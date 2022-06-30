#' vectorize.text
#'
#' Converts vector of text to a list of vectors with words within each cell. Note: punctuation will be removed.
#' @param x text string to vectorize.
#' @export
#' @examples
#' text<-"One hundred twenty-eight students participated in our Study, 
#' that was administred in thirteen clinics."
#' vectorize.text(text)

# Function to convert text to vector of words
vectorize.text<-function(x){
helper<-function(x){
x<-unlist(x)
# split at spaces
x<-unlist(strsplit(x," "))
# clean up front and back
x<-gsub("[.,;(]([^0-9])","\\1",x)
x<-gsub("[.,;)!?]*$","",x)
# select lines with letters or numbers
x<-grep("[a-zA-Z0-9]",x,value=T)
x<-x[which(nchar(x)>0)]
}
x<-lapply(x,helper)
return(x)
}

