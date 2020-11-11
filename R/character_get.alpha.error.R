#' get.alpha.error
#'
#' Extract reported alpha error from text
#' @param x text to process
#' @export
#' @examples
#' x<-c("The threshold for significance was adjusted to .05/2",
#' "Type 1 error rate was alpha=.05.")
#' get.alpha.error(x)
#' x<-c("We used p<.05 as level of significance.",
#'      "We display .95 CIs and use an adjusted alpha of .10/3.",
#'      "The effect was significant with p<.025.")
#' get.alpha.error(x)

get.alpha.error<-function(x){
x<-grep("[Mm]eta[a ]|[Mm]eta-|[Aa]mplitude|Hz",x,invert=TRUE,value=TRUE)
split<-lapply(x,function(x) unlist(strsplit(x," and | with |, ")))
# tolower
lower<-unlist(lapply(split,tolower))
# select lines 
alpha1<-(lapply(lower,function(x) grep("alpha of|alpha error|type 1 error|type i error|level for significance|level of significance|significance level|cutoff|threshold|corrected|correction|adjust|confidence[- ]interval|[^a-z]ci[^a-rt-z]",x,value=TRUE)))
# select lines with numbers
alpha1<-lapply(alpha1,function(x) grep("[0-9]",x,value=TRUE))
# remove "-'"
alpha1<-lapply(alpha1,function(x) gsub("[-']"," ",x))
# remove punctuation at end
alpha1<-lapply(alpha1,function(x) gsub("[[:punct:]]$","",x))
# remove html
alpha1<-lapply(alpha1,function(x) gsub("<[/a-z\\].*?.*[a-z]>","",x))
#exclude cronbach|intercept
alpha1<-lapply(alpha1,function(x) grep("cronbach|bach s|internal|reliabilit|intercept",x,invert=TRUE,value=TRUE))
#exclude ranging
alpha1<-lapply(alpha1,function(x) grep(" ranges | ranged |ranging|[0-9] [0-9]",x,invert=TRUE,value=TRUE))
#exclude lines with special punctuation
alpha1<-lapply(alpha1,function(x) grep("[~\\+\\^]",x,invert=TRUE,value=TRUE))
# clean up spaces
alpha1<-lapply(alpha1,function(x) gsub("  "," ",gsub("  "," ",x)))
# unify "=" "<"
alpha1<-lapply(alpha1,function(x) gsub(" [=] |[=] | [=]","=",x))
alpha1<-lapply(alpha1,function(x) gsub(" [<] |[<] | [<]","<",x))
alpha1<-lapply(alpha1,function(x) gsub("\\(|\\)|[\\:]","",x))
# "==" to "="
alpha1<-lapply(alpha1,function(x) gsub("==","=",x))

# select lines with =< to [0-9]
alpha1<-lapply(alpha1,get.sentence.with.pattern,"[=<]| to \\.| to [0-9<]| of [0-9<]| of \\.| at [0-9<]| at [0-9<]")

# clean up
alpha1<-lapply(alpha1,function(x) gsub(".*alpha [<=]|.*alpha[<=]|.*alpha of ","alpha=",x))
alpha1<-lapply(alpha1,function(x) gsub(".*p [<=]|.*p[<=]|probability of ","p<",x))
alpha1<-lapply(alpha1,function(x) gsub(".*probability of |.*cutoff of |.*cutoff at |.*threshold of |.*threshold at ","p<",x))

# remove non numbers at end
alpha1<-lapply(alpha1,function(x){while(length(grep("[^0-9]$",x))>0){x<-gsub("[^0-9]$","",x)}
return(x)})

# select lines with numbers
alpha1<-lapply(alpha1,function(x) grep("[0-9]",x,value=TRUE))
# reduce front
alpha1<-lapply(alpha1,function(x) gsub(".* to[< ]|.* at |.* of |with a |.* value|.* level","alpha=",x))
alpha1<-lapply(alpha1,function(x) gsub("alpha","a",x))
alpha1<-lapply(alpha1,function(x) gsub("p<","a=",x))

# shorten string
alpha<-lapply(alpha1,function(x) substr(x,1,12))
# select lines with number only
alpha<-lapply(alpha,function(x) grep("[0-9]",x,value=TRUE))
# remove from first space
alpha<-lapply(alpha,function(x) gsub(" .*","",x))
# remove ",()" or " " after number
alpha<-lapply(alpha,function(x) gsub("([0-9%])[, \\)\\(].*","\\1",x))

# select lines with [=<]
alpha<-lapply(alpha,function(x) grep("[<=]",x,value=TRUE))
# convert % to number
alpha<-lapply(alpha,function(x) gsub("[%]","e-2",x))

# remove letters at end
alpha<-lapply(alpha,function(x){ while(length(grep("[^0-9]$",x))>0){x<-gsub("[^0-9]$","",x)}
return(x)})
# remove lines without "a="
alpha<-lapply(alpha,function(x) grep("a[=]",x,value=TRUE))
# remove lines with "q,z,r,g,s,k,#"
alpha<-lapply(alpha,function(x) grep("[qzrgsk#]",x,value=TRUE,invert=TRUE))
# remove after second "="
alpha<-lapply(alpha,function(x){
x[nchar(x)-nchar(gsub("=","",x))==2]<- gsub("(a=.*)=.*","\\1",x[nchar(x)-nchar(gsub("=","",x))==2])
return(x)
})

# remove lines with "\\.[2-9]"
#alpha<-lapply(alpha,function(x) grep("\\.[2-9]",x,value=TRUE,invert=TRUE))

# extract lines with corrected p-value
corrected<-NULL
i<-grep("/|adjust|correct",lower)
if(length(i)>0){
   corrected[i]<-alpha[i]}else corrected<-NA
# set lines in alpha without "/" but "adjust" to NA
i<-!unlist(grepl("/",lower))&unlist(grepl("adjust|correct",lower))
if(sum(i)>0){
alpha[i]<-NA
}   
   
# convert "/" to decimal 
corrected<-lapply(corrected,function(x){
if(length(grep("/",x))>0){
pre<-suppressWarnings(as.numeric(lapply(x[grep("/",x)],function(y) gsub(".*=","",gsub("/.*","",y)))))
post<-suppressWarnings(as.numeric(lapply(x[grep("/",x)],function(y) gsub(".*/","",gsub("([0-9])=.*","\\1",y)))))
# fraction to decimal
x[grep("/",x)]<-pre/post
# remove lines with pre>.1
x<-x[pre<=.1]
}
return(x)}
)

# delete after /num
alpha<-lapply(alpha,function(x) gsub("/[\\.0-9]*","",x))
# delete front
alpha<-lapply(alpha,function(x) gsub(".*[=<]","",x))
corrected<-lapply(corrected,function(x) gsub(".*[=<]","",x))
# numerise and round
alpha<-lapply(alpha,function(x) round(suppressWarnings(as.numeric(x)),5))
corrected<-lapply(corrected,function(x) round(suppressWarnings(as.numeric(x)),5))
# only include 0 < alpha <= 1 and not NA
alpha<-lapply(alpha,function(x) x[x>0&x<=1])
corrected<-lapply(corrected,function(x) x[x>0&x<=1])
alpha<-lapply(alpha,function(x) x[!is.na(x)])
corrected<-lapply(corrected,function(x) x[!is.na(x)])
# unique
alpha<-unique(unlist(alpha))
corrected<-unique(unlist(corrected))

## get alpha from 1-x% Confidence Intervall
# split x and lowerise
#split<-lapply(x,function(x) strsplit(x," and | with |, |; |\\("))
# select lines with ci
ci<-lapply(lower,function(x) grep("[^a-z]ci[^a-rt-z]|[^a-z]ci$|confidence[ \\-]intervall",x,value=TRUE))
# select lines with numbers
ci<-lapply(ci,function(x) grep("[0-9]",x,value=TRUE))
# select lines with % or .num
ci<-lapply(ci,function(x) grep("[\\%]|\\.[987]",x,value=TRUE))
# remove space in front of %
ci<-lapply(ci,function(x) gsub(" [%]","%",x))
# remove text after %
ci<-lapply(ci,function(x) gsub("[%].*","%",x))
# remove text after .[0-9]
ci<-lapply(ci,function(x) gsub(".*(\\.[0-9]*).*","\\1",x))
# remove till space or special characters
ci<-lapply(ci,function(x) gsub(".*[ ,;]","",x))
# select lines without "-"
ci<-lapply(ci,function(x) grep("\\-",x,value=TRUE,invert=TRUE))
# remove letters
ci<-unlist(lapply(ci,function(x) gsub("[a-z\\=]","",x)))
# remove ><
#ci<-lapply(ci,function(x) gsub(".*[ \\(=<>\\-\\+;]","",x))
# select lines starting with "[8-9][0-9]"
ci<-lapply(ci,function(x) grep("^[7-9]|^\\.[7-9]",x,value=TRUE))
# remove further punctuation
ci<-lapply(ci,function(x) gsub(".*[<>\\{]","",x))
# % to number
ci<-lapply(ci,function(x) gsub("[%]","e-02",x))

ci<-lapply(ci,function(x) round(1-as.numeric(x),4))
ci<-unique(unlist(ci))

#####

# return unique alpha errors
if(length(alpha)==0) alpha<-NULL
if(length(corrected)==0) corrected<-NULL
if(length(ci)==0) ci<-NULL

return(list(alpha_error=alpha,corrected_alpha=corrected,alpha_from_CI=ci))
}
