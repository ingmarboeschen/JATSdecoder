#' get.multi.comparison
#'
#' Extracts alpha-/p-value correction method for multiple comparisons from list with 15 correction methods
#' @param x text to process
#' @export
#' @examples
#' x<-"We used Bonferroni corrected p-values."
#' get.multi.comparison(x)

get.multi.comparison<-function(x){
# convert to sentences if has length 1
if(length(x)==1) x<-text2sentences(x)
# procedure search terms
proc<-c(
        "AlphaSim|Alpha[- ]Sim",
        "[^A-Z]FDR[^A-Z]|[fF]alse [dD]iscovery [rR]ate",
        "Boole[^a-z]",
        "Bonferroni|[Bb]onff*err*onn*i",
        "Fisher LSD|[Ll][Ss][Dd][^A-Za-z].*post[- ]hoc|post[- ]hoc.*[^a-zA-Z][Ll][Ss][Dd]|LSD[^a-z].*Fisher|Fisher[^a-z].*LSD[^a-z]|[lL]east significant difference",
        "Holm[^a-z]",
        "Tukey|Tuckey",
        "Benjamini",
        "Hochberg",
        "Dunnett[^a-z]|Dunn*et",
        "Duncan",
        "Newman",
        "Keuls",
        "[\U0160S]id[a\U00E1]k",
        "Scheff[e\U00E9\U00E8]|Schef[e\U00E9\U00E8]|Scheff[^a-z]|Sheff[e\U00E9\U00E8]"
        )
# reduce to relavant lines
res<-grep("[Cc]orrected|[Cc]orrection|[Cc]orrected| *[Aa]djust|[Mm]ultiple|[Pp]ost[- ][Hh]oc",x,value=T)
# split lines
res<-unlist(strsplit(res,", |; "))
# remove lines that negate the use of a method
res<-grep("[Ii]nstead of| not |n[^ a-z]t|Since | since |rather than",res,invert=T,value=T)
# wich procedure is used
res<-which.term(res,terms=proc,tolower=F,hits=T)
# clean up
res<-gsub("\\[\\^A-Z\\]","",res)
res<-gsub("\\[\\^a-z\\]","",res)
res<-gsub("\\[\U0160S\\]","\U0160",res)
res<-gsub("\\[e\U00E9\U00E8\\]","\U00E9",res)
res<-gsub("\\[a\U00E1\\]","\U00E1",res)
res<-gsub("Bonferroni\\|Bonf.*","Bonferroni",res)
res<-gsub("Dunnett\\|Dunet","Dunnett",res)
res<-gsub("Fisher LSD","Fisher",res)
res<-unique(gsub("\\|.*","",res))
res<-unique(res)
return(res)
}
