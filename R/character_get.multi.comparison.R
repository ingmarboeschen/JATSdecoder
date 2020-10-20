#' get.multi.comparison
#'
#' Extract alpha-/p-value correction method for multiple comparisons from list with 14 correction methods
#' @param x text to process
#' @export
#' @examples
#' x<-"We used Bonferroni corrected p-values."
#' get.multi.comparison(x)

get.multi.comparison<-function(x){
# procedure search terms
proc<-c("Boole","Bonferroni|Bonf| bonfer","Holm","[^A-Z]FDR[^A-Z]","AlphaSim|Alpha[- ]Sim","[\U0160S]id[a\U00E1]k","Tukey","Benjamini","Hochberg","Dunnett|Dunet","Duncan","Newman","Keuls","Scheff[e\U00E9\U00E8]|Schef[e\U00E9\U00E8]")
# reduce to relavant lines
res<-lapply(x,get.sentence.with.pattern,"corrected|correction|corrected| adjust|multiple|[Pp]ost[- ][Hh]oc",tolower=F)
# split lines
res<-lapply(res,function(x) unlist(strsplit(x,", |; ")))
# remove lines that negate the use of a method
res<-lapply(res,function(x) grep("[Ii]nstead of| not |n[^ a-z]t|Since | since |rather than",x,invert=T,value=T))
# wich procedure is used
res<-unlist(lapply(res,which.term,tolower=F,hits=T,proc))

# clean up
res<-gsub("\\[\\^A-Z\\]","",res)
res<-gsub("\\[\U0160S\\]","\U0160",res)
res<-gsub("\\[e\U00E9\U00E8\\]","\U00E9",res)
res<-gsub("\\[a\U00E1\\]","\U00E1",res)
res<-gsub("Bonferroni\\|Bonf.*","Bonferroni",res)
res<-gsub("Dunnett\\|Dunet","Dunnett",res)
res<-unique(gsub("\\|.*","",res))
res<-unique(res)
return(res)
}
