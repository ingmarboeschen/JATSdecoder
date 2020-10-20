#' get.sig.adjectives
#'
#' Extract adjectives used for in/significance out of list with 37 potential adjectives
#' @param x text to process
#' @export
#' @examples
#' get.sig.adjectives(
#'  x<-"We found very highly significance for type 1 effect"
#' )

get.sig.adjectives<-function(x){
# preperation/split/uniformisation
a<-lapply(x,function(x) tolower(unlist(strsplit(x," and |,|;| or |as well| but | where| while|although"))))
a<-lapply(a,get.sentence.with.pattern,c("significant|significally|significanc"))
# remove brackets
a<-lapply(a,function(x) gsub("[\\(\\)]|\\[|\\'`]","",x))
# convert "n't" to " not"
a<-lapply(a,function(x) gsub("n[`']t"," not",x))
# convert "-" to " "
a<-lapply(a,function(x) gsub("-"," ",x))
# remove "statistical "
a<-lapply(a,function(x) gsub("statistical ","",x))
a<-lapply(a,function(x) gsub("  "," ",x))

# adjectives
words<-c("marginal","marginally","almost","highly","higher","lower","very","moderately","barely","strong","strongly","close to","remain","remained","even","potentially","weakly", "nearly","near","near to","slightly","still","most","mostly","least","more","less","fewer","borderline","practically","clinically","trend","trend toward","trend towards","tended to be","similarly","close to")

words<-sort(words)
# sig words
sig<-paste("",words,"sign")
# not/non/in sig words
# sig words
insig<-paste(paste("",words,"insign"),paste("",words,"not sign"),paste("",words,"non sign"),sep="|")

#c<-lapply(a,get.sentence.with.pattern,"marginal")
sig<-lapply(a,has.sentence.with.pattern,sig)
insig<-lapply(a,has.sentence.with.pattern,insig)
sig<-names(unlist(sig)[unlist(sig)>0])
insig<-names(unlist(insig)[unlist(insig)>0])

sig<-gsub("^ ","",sig)
insig<-gsub("^ |\\|.*","",insig)

return(list(sig_adjective=sig,insig_adjective=insig))

}


