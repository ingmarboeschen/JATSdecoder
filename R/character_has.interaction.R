#' has.interaction
#'
#' Identifies mentiones of interaction/moderator/mediator effect in text.
#' @param x text string to process.
#' @return Character vector with type/s of identified interaction/moderator/mediator effect.
#' @export
 
has.interaction<-function(x){
if(length(x)>0){
  # split and reduce lines
  x<-tolower(unlist(strsplit(x,"[;,] | and ")))
  x<-grep("interact|mediat|moderat",x,value=TRUE)
  # remove lines with non interactions only if has interaction but no effect
  temp<-c("human[- ]"," child","mother","father","baby","infant","toddler","peer","teenager"," human"," cell","social")
  temp<-paste(paste0(temp,".*interaction"),collapse="|")
  # lines that have specific interaction 
  i1<-grep(temp,x)
  # lines that have clear interaction or mediator/moderator effect
  i2<-grep("mediat|moderat|interaction[s]*[- ]effect|interaction[s]*[- ]term|interactive[- ]effect|interactional[- ]effect|significant.*interaction",x)
  # remove text with interaction in lines with specific interaction and no interaction effect
  i<-i1[!is.element(i1,i2)]
  # remove text of specific interaction
  x[i]<-gsub(temp,"",x[i])

  if(length(x)>0){
  # search term definition
  int<-" [Aan][Nno] interaction|interaction[s]*[- ]effect|interaction[s]*[- ]term|interactive[- ]effect|interactional[- ]effect|significant.*interaction|interaction.*signif|[\\*].*? interaction|interaction analys|[^a-z]time[s ].*? interaction|[^a-z]way .*interaction| x .*? interaction|predicted interaction|proposed interaction|interaction.*[^a-zA-Z]F *\\([1-9][0-9]*, *[1-9][0-9]*\\)|[^a-zA-Z]F *\\([1-9][0-9]*, *[1-9][0-9]*\\).*interaction|the interaction of|and interaction of"
  mod<-"moderating[- ]effect|moderation[- ]effect|moderator[- ]effect|moderator.*variable|moderated[- ]effect|moderational[- ]effect|moderator and mediator|significant moderator|moderator analysis|moderation analysis|moderator.* model|moderation model|proposed moderator|[ w][ai]s moderated by |moderator.*signifi|moderated[- ]mediation"
  med<-"mediating[- ]effect|mediation[- ]effect|mediator[- ]effect|mediator.*variable|mediated[- ]effect|mediational[- ]effect|moderator and mediator|significant mediator|mediator analysis|mediation analysis|mediator.* model|mediated.* model|mediation.* model|proposed mediator|mediated by| mediat.*signifi|signifi.* mediat|mediated[- ]moderation"
  hit<-ifelse(has.pattern(x,c(int,mod,med),tolower=TRUE)==1,TRUE,FALSE)
  #inter<-character(0)
  inter<-c("interaction","moderator","mediator")[hit]
  if(length(inter)==0) inter<-character(0)
}else inter<-character(0)
  }else inter<-character(0)
return(inter)
}


has.pattern<-function(x,patterns=c(""),tolower=TRUE){
  res<-NULL
  for(i in 1:length(patterns)){
    if(tolower==T) res[i]<-ifelse(length(grep(tolower(patterns[i]),tolower(x)))>0,1,0)
    if(tolower!=T) res[i]<-ifelse(length(grep(patterns[i],x))>0,1,0)
  }
  names(res)<-patterns
  res
}


