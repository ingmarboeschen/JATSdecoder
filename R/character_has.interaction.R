#' has.interaction
#'
#' Identify interaction/moderator/mediator effect in text
#' @param x text to process
#' @export
 
has.interaction<-function(x){
if(length(x)>0){
if(sum(!is.na(x))>0){
# search term definition
int<-"interaction[- ]effect|interactive[- ]effect|interactional[- ]effect|the interaction|[an][no] interaction|and interaction|significant.*interaction|interaction.*signif|[\\*].*? interaction|[^a-z]time[s ].*? interaction|[^a-z]way .*interaction| x .*? interaction|predicted interaction|proposed interaction|interaction.*[^a-zA-Z]F *\\([1-9][0-9]*, *[1-9][0-9]*\\)|[^a-zA-Z]F *\\([1-9][0-9]*, *[1-9][0-9]*\\).*interaction"
mod<-"moderating[- ]effect|moderation[- ]effect|moderator[- ]effect|moderator[- ]variable|moderated[- ]effect|moderational[- ]effect|moderator and mediator|significant moderator|moderator analysis|moderator model|moderation model|proposed moderator|moderated by .*?[\\.0-9][0-9]"
med<-"mediating[- ]effect|mediation[- ]effect|mediator[- ]effect|mediator[- ]variable|mediated[- ]effect|mediational[- ]effect|moderator and mediator|significant mediator|mediator analysis|mediator model|mediation model|proposed mediator|mediated by"
hit<-ifelse(has.pattern(x,c(int,mod,med),tolower=T)==1,TRUE,FALSE)
inter<-c("interaction","moderator","mediator")[hit]
if(length(inter)==0) inter<-character(0)
}}else inter<-character(0)
return(inter)
}


