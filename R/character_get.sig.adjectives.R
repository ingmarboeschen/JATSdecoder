#' get.sig.adjectives
#'
#' Extracts adjectives used for in/significance out of list with 37 potential adjectives.
#' @param x text string to process.
#' @param unique_only Logical. If TRUE returns unique hits only.
#' @return Character. Vector with identified adjectives.
#' @export
#' @examples
#' get.sig.adjectives(
#'  x<-"We found very highly significance for type 1 effect"
#' )

get.sig.adjectives<-function(x,unique_only=FALSE){
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
# add space in front
a<-lapply(a,function(x) paste0(" ",x))

# adjectives
words<-c("marginal","marginally","almost","highly","higher",
         "lower","very","moderately","barely","strong","strongly",
         "close to","remain","remaine[sd]","even","potentially","weakly", 
         "nearly","near","near to","slightly","still","most","mostly",
         "least","more","less","fewer","borderline","practically",
         "clinically","trend","trend toward","trend towards",
         "tend[eds]* to be *[a-z]*",
         "similarly","close to","approach[esd]* *[a-z]*")

words<-sort(words)
# sig words
sig<-paste("",words,"sign")
# not/non/in sig words
# sig words
insig<-paste(paste("",words,"insign"),paste("",words,"not sign"),paste("",words,"non sign"),sep="|")
#c<-lapply(a,get.sentence.with.pattern,"marginal")
sig<-lapply(a,has.pattern,sig)
insig<-lapply(a,has.pattern,insig)

sig<-names(unlist(sig)[unlist(sig)>0])
insig<-names(unlist(insig)[unlist(insig)>0])
# clean up
sig<-gsub("^ ","",sig)
insig<-gsub("^ |\\|.*","",insig)

# unify some uses
sig<-gsub("marginal ","marginally ",sig)
insig<-gsub("marginal ","marginally ",insig)
sig[sig=="approach[esd]* *[a-z]* sign"]<-"approaches sign"
insig[insig=="approach[esd]* *[a-z]* insign"]<-"approaches sign"
sig[sig=="tend[eds]* to be *[a-z]* sign"]<-"tends to be sign"
insig[insig=="tend[eds]* to be *[a-z]* insign"]<-"tends to be insign"
sig[sig=="remaine[sd] sign"]<-"remains sign"
insig[insig=="remaine[sd] insign"]<-"remains insign"
sig<-gsub("strong ","strongly",sig)
insig<-gsub("strong ","strongly",insig)
sig<-gsub("near ","nearly",sig)
insig<-gsub("near ","nearly",insig)
sig<-gsub("toward ","towards",sig)
insig<-gsub("toward ","towards",insig)

if(unique_only==FALSE) return(list(sig_adjective=sig,insig_adjective=insig))
if(unique_only==TRUE) return(list(sig_adjective=unique(sig),insig_adjective=unique(insig)))

}


