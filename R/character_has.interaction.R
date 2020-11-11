#' has.interaction
#'
#' Identify interaction/moderator/mediator effect in text
#' @param x text to process
#' @export
 
has.interaction<-function(x){
if(length(x)>0){
if(sum(!is.na(x))>0){
# search term definition
int<-"interaction[- ]effect|interactive[- ]effect|interactional[- ]effect|the interaction|[an][no] interaction|and interaction|significant interaction|interaction was not signif|interaction was signi|[\\*].*? interaction| time[s ].*? interaction| x .*? interaction|predicted interaction|proposed interaction|interaction of"
mod<-"moderating[- ]effect|moderation[- ]effect|moderator[- ]effect|moderator[- ]variable|moderated[- ]effect|moderational[- ]effect|moderator and mediator|significant moderator|moderator analysis|moderator model|moderation model|proposed moderator|moderated by .*?[\\.0-9][0-9]"
med<-"mediating[- ]effect|mediation[- ]effect|mediator[- ]effect|mediator[- ]variable|mediated[- ]effect|mediational[- ]effect|moderator and mediator|significant mediator|mediator analysis|mediator model|mediation model|proposed mediator|mediated by"
hit<-ifelse(has.pattern(x,c(int,mod,med),tolower=T)==1,TRUE,FALSE)
inter<-c("with interaction","with moderator","with mediator")[hit]
if(length(inter)==0) inter<-"none"
}}else inter<-"none"
return(inter)
}


