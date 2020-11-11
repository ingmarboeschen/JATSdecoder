#' study.type
#'
#' function to identify type of study by list of study types
#' @param title articles title text
#' @param text main text to process
#' @export
#' @examples 
#' study.type("We performed a randomized treatment control trail with waiting group")
 
study.type<-function(title=NULL,text=NULL){
# out of title
if(length(title)>0){
types<-c("meta[ -]analys"," review|^review ","cohort study","study protocol")
title<-which.term(title,types,hits_only=T)
}else title<-NULL
# out of text
if(length(text)>0){
# experimental designs
rand<-grep("random|experimental",text,value=T)
pat<-c("placebo","treatment control|control treatment","treatment as usual","[^a-zA-z]rct[^a-z]|[^a-zA-z]rtc[^a-z]","wait list|waiting list","active group")
has.pattern(rand,pat)
}else text<-NULL
return(c(title,text))
}


