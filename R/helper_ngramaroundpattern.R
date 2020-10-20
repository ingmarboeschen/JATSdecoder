#' ngramaroundpattern
#'
#' Extract an ngram word vector around a pattern word match
#' @param x text to process
#' @param pattern a search string pattern to build the ngram
#' @param ngram a vector of length=2 that defines the number of gram on left and right side of pattern word match
#' @param tolower Logical. If TRUE converts text and pattern to lower case
#' @export
#' @examples
#' text<-"One hundred twenty-eight students participated in our Study, 
#' that was administred in thirteen clinics."
#' ngramaroundpattern(text,pattern="study",ngram=c(-1,2))

ngramaroundpattern<-function(x,pattern,ngram=c(-3,3),tolower=TRUE){
temp<-NA
if(length(x)>0){
text<-x
if(tolower==TRUE){ pattern<-tolower(pattern); text<-tolower(text)}
if(length(grep(pattern,text))>0){
   text<-unlist(text)
   if(sum(!is.na(text))>0){
    # select lines
    text<-text[grep(pattern,text)]
    # vectorize lines and extract ngram
    temp<-vectorize.text(text)
    # get positions of pattern in lines
    ind<-lapply(lapply(temp,function(y) grep(pattern,y)),max,warn=F)
    # get index of ngram in lines
    # For several pattern matchess in one line, take max of index
    ind<-lapply(ind,function(y) y+(ngram[1]:ngram[2]))
    len<-lapply(temp,length)
    # remove bad indices
    for(i in 1:length(ind)){
      ind[[i]]<-unlist(ind[i])[unlist(ind[i])>0&unlist(ind[i])<=length(unlist(temp[i]))]
      temp[[i]]<-temp[[i]][unlist(ind[i])]
      temp[[i]]<-paste(unlist(temp[[i]]),collapse=" ")
    }
  }
 }
}
return(unlist(temp))
}
