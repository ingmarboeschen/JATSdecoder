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
  # select lines with numbers  
  x<-grep("[0-9]",x,value=TRUE)
  # convert to sentences if has length 1
  if(length(x)==1) x<-text2sentences(x)
  ## unify
  # tolower
  x<-tolower(x)
  # remove alpha-num
  x<-gsub("alpha[-][0-9]|alpha [-][0-9]","",x)
  # remove html
  x<-gsub("<[/a-z\\].*?[a-z]>","",x)
  # convert "-" to space
  x<-gsub("  *"," ",gsub("[-]"," ",x))
  # remove hyphens
  x<-gsub("'","",x)
  # convert plural singular to alpha error 
  x<-gsub("values","value",x)
  x<-gsub("errors","error",x)
  x<-gsub("was set","set",x)
  x<-gsub("([a-z])(\\.[0-9])","\\1 \\2",x)
  # convert synonyms to alpha error 
  x<-gsub("alpha error level|alpha error probability|type 1 error|type i error|significance criterion|statistical threshold|criterion of significance|significance threshold","alpha error",x)
  #all operators to '='
  x<-gsub("[=][=]*","=",gsub("[<=>]","=",x)) 

  ## unify alpha error representation
  # if has no alpha error yet
  if(length(grep("alpha error",x))==0){
    x<-gsub("alpha set to|two tailed alpha at a|alpha was set at|was set at alpha|two tailed alpha at|alpha level of|alpha level was|with alpha set to|alpha level of|criterion for statistical significance","alpha error",x)
    x<-gsub("level of alpha|alpha level|significance level|level for significance|level of significance|level for statistical significance|level of statistical significance|significance criterion|significant at alpha","alpha error",x)
    x<-gsub("corrected error probability","corrected alpha error probability",x)
  }
  # if still has no alpha error yet but .0[15], 0.1 or 0.001 convert alpha to alpha error
  if(length(grep("alpha error",x))==0){
    ind<-grep("\\.0[15]|\\.1[^0-9]|\\.001",x)
    x[ind]<-gsub("alpha .*(\\.1[^0-9])|alpha .*(\\.0[15][^0-9])|alpha .*(\\.001[^0-9])","alpha error=\\1\\2",x[ind])
  }  
  # if line has 'alpha' and 'power, CI' or similar but no 'error|item|scale' add 'error' behind alpha
  i1<-grep("alpha",x)
  i2<-grep("power analy|power of|[0-9\\%] power|confidence interval|significance criterion|false discovery rate|FDR|two[ -]tailed|significance level|multiple comparison|significance at alpha|post[ -]hoc|a[ -]priori|significant at alpha|bonferroni correct|level of significance|consider[^0-9,;\\(]* significant",x)
  #  confidence level?
  i3<-grep("error|item|scale| fail| reach",x,invert=T)
  i4<-i1[is.element(i1,i2)]
  i<-i4[is.element(i4,i3)]
  if(length(i>0)) x[i]<-gsub("alpha|alpha value|alpha value","alpha error",x[i])
  # if line has 'p-value' and 'alpha error' synonym but no 'alpha error' convert p to alpha error
  i1<-grep("[^a-z]p value|[^a-z]p[^a-z]|probability of",x)
  i2<-grep("significance criterion|significance level|consider[^0-9,;\\(]* signif|significant at p|level of signif|criterion for .*signific",x)
  #confidence level|?
  i3<-i1[is.element(i1,i2)]
  # exclude if has
  i4<-grep("alpha error|scale|item| fail| reach",x,invert=T)
  i<-i3[is.element(i3,i4)]
  if(length(i)>0) x[i]<-gsub("[^a-z]p value|[^a-z]p[^a-z]|probability [ao][tf]"," alpha error ",x[i])
  # remove lines with patterns  
  x<-grep("[^a-z]meta |[^a-z]meta-|amplitude|[^a-z]hz",x,invert=TRUE,value=TRUE)
  # correct error of alpha
  x<-gsub("error [ao][ft] alpha","alpha error",x)
  x<-gsub("alpha error of p","alpha error",x)
  # if has number in front of alpha error move to back
  x<-gsub(" ([0-9\\.]*[0-9]) (alpha error)([^<=>][^<=>])"," \\2of \\1 ",x)
  # unify use of "="
  x<-gsub(" a priori"," ",x)
  x<-gsub(" of [<=]*| of a | value of "," of ",x)
  x<-gsub(" at [<=]*| at a "," at ",x)
  x<-gsub("[=] [=]","=",x)
  x<-gsub("  "," ",x)
  x<-gsub("[\\:] ([0-9\\.])","= \\1",x)
  # probability of to
  x<-gsub("probability of ","p =",x)
  # clean up
  x<-gsub("is set [at][to]|was set [at][to]|set [at][to]|set [at][to]|level [ao][tf]| was ([\\.0-9])| is ([\\.0-9])","= \\1\\2",x)
  x<-gsub("alpha error [ato][tof]","alpha error=",x)
  x<-gsub("of ([0-9\\.])","= \\1",x)
  x<-gsub("  *"," ",x)
  # remove operator if not neccessary
  x<-gsub(" of [<=]"," at ",x)
  x<-gsub("to [<=]","to ",x)
  x<-gsub("at [<=]","at ",x)
  x<-gsub("  "," ",x)
  # remove "'"
  x<-gsub("  *"," ",gsub("[']"," ",x))
  # insert "=" after alpha error with no operator
  x<-gsub("alpha error ([0-9\\.])","alpha error=\\1",x)
   # place number in front of alpha error to end if has no operator
  if(length(grep("alpha error =|alpha error=",x))==0){
    x<-gsub("([0-9\\.]*) alpha error."," alpha error=\\1 ",x)
  }
  # unify standard alphas (.1,.05,.01,.001) with no brackets
  x<-gsub("alpha [<=] *0.1([^0-9])","alpha error = 0.1\\1",x)
  x<-gsub("alpha [<=] *.1([^0-9])","alpha error = 0.1\\1",x)
  x<-gsub("alpha [<=] *0.0([15][^0-9])","alpha error = 0.0\\1",x)
  x<-gsub("alpha [<=] *.0([15][^0-9])","alpha error = 0.0\\1",x)
  x<-gsub("alpha [<=] *0.00([1][^0-9])","alpha error = 0.00\\1",x)
  x<-gsub("alpha [<=] *.00([1][^0-9])","alpha error = 0.00\\1",x)
  # all operators to =
  x<-gsub("[=<>][=<>]*","=",x)
  x<-gsub(" *[=<>] *","=",x)
  # split 
  x<-unlist(strsplit(x," and | with |, |; | \\(|\\)"))
  
  ## mark corrected alpha if has 'correction' but no standard alpha
  if(length(grep("correct|adjust|bonf|holm|tukey|benjamin|hochberg|hsd",x))>0){
    # lines with alpha error/s
    i1<-grep("alpha error",x)
    # lines without standard alpha levels
    i2<-grep("\\.1[^0-9]*|\\.05[^0-9]*|\\.01[^0-9]*|\\.001[^0-9]*",x,invert=TRUE)
    # lines to add 'corrected' to alpha error
    i<-i1[is.element(i1,i2)]
    x[i]<-gsub("alpha error","corrected alpha error",x[i])
    }  

  ## remove 'corrected' in lines with standard alpha if has 'correction'
  if(length(grep("correct|adjust",x))>0){
     # get lines with correction
    i1<-grep("adjust|correct",x)
    # lines with standard alpha levels
    i2<-grep("\\.1[^0-9]*|\\.05[^0-9]*|\\.01[^0-9]*|\\.001[^0-9]*",x)
    # lines to remove 'corrected'
    i<-i1[is.element(i1,i2)]
    x[i]<-gsub("correct[a-z]*|adjust[a-z]*","",x[i])
  }  
##############################################
## extract alpha error
############################################
  alpha<-get.alpha(x)
  corrected<-get.corrected(x)
  ci<-get.ci(x)
  return(list(alpha_error=alpha,corrected_alpha=corrected,alpha_from_CI=ci))
}
#######################################################
# definitions of functions to extract alpha  
get.alpha<-function(x){
# remove corrected alphas
x<-gsub("corrected alpha error[ \\.0-9=]*","",x)
# select lines 
alpha<-grep("alpha error",x,value=TRUE)
standard_alpha<-suppressWarnings(as.numeric(gsub("\\\\","",which.term(alpha,c("\\.1","\\.05","\\.01","\\.001"),hits_only=TRUE))))
# select lines with numbers
alpha<-text2num(alpha,exponent=FALSE,fraction=FALSE,product=FALSE,words=FALSE)
alpha<-grep("[0-9]",alpha,value=TRUE)
#exclude cronbach|intercept
alpha<-grep("cronbach|bach s|internal|reliabilit|intercept|beta",alpha,invert=TRUE,value=TRUE)
# exclude ranging
alpha<-grep(" ranges | ranged |ranging|[0-9] [0-9]",alpha,invert=TRUE,value=TRUE)
# exclude lines with special punctuation
alpha<-grep("[~\\+\\^]",alpha,invert=TRUE,value=TRUE)
# clean up spaces
alpha<-gsub("  *"," ",alpha)
# place number in front of alpha error to end if has no number
if(length(grep("alpha error= [0-9\\.]|alpha error = [\\.0-9]",alpha))==0){
  alpha<-gsub("([0-9\\.]*) alpha error."," alpha error=\\1 ",alpha)
}
# remove punctuation at end and '|'
alpha<-gsub("[[:punct:]]$","",alpha)
alpha<-gsub("\\(|\\)|[\\:]","",alpha)
# remove text between alpha error and number if no operatoe
alpha<-gsub("alpha error .*?([0-9\\.][0-9\\.][0-9\\]*)","alpha error=\\1",alpha)
# split before and after "alpha error=0.num*"
if(length(unlist(alpha))>0){
  alpha<-strsplit2(unlist(alpha),"alpha error[=]0\\.[0-9]|alpha error[=]\\.[0-9]","before")
  alpha<-strsplit2(unlist(alpha),"alpha error[=]0\\.[0-1][0-9][^0-9]|alpha error[=]\\.[0-1][0-9][^0-9]","after")
  alpha<-strsplit2(unlist(alpha),"alpha error[=]0\\.[0-1][0-9][0-9]|alpha error[=]\\.[0-1][0-9][0-9]","after")
}
alpha<-unlist(alpha)
# select lines with =< to [0-9\\.]
alpha<-grep("[=<] *[0-9\\.]| [ota][oft] [0-9<\\.]| at a [\\.0-9<]",alpha,value=TRUE)
# clean up
alpha1<-gsub("alpha error","alpha",alpha)
alpha1<-gsub(".*alpha [<=]|.*alpha[<=]|.*alpha of ","alpha=",alpha1)
alpha1<-gsub(".*p [<=]|.*p[<=]|probability of |p value of |p value","p<",alpha1)
alpha1<-gsub(".*probability of |.*cutoff of |.*cutoff at |.*threshold of |.*threshold at ","p<",alpha1)
# remove non numbers at end
alpha1<-gsub("[^0-9\\%]*?$","",alpha1)
# select lines with numbers
alpha1<-grep("[0-9]",alpha1,value=TRUE)
# reduce front
alpha1<-gsub(".* to[< ]|.* at |.* at a |.* of |with a |.* value|.* level","alpha=",alpha1)
alpha1<-gsub("alpha|alpha error","a",alpha1)
alpha1<-gsub("p<","a=",alpha1)
# shorten string
#alpha1<-substr(alpha1,1,12)
# remove lines without "a="
alpha1<-grep("a[=]",alpha1,value=TRUE)
# remove lines with "q,z,r,g,s,k,#"
alpha<-grep("[qzrgsk#]",alpha1,value=TRUE,invert=TRUE)
# remove after second "="
alpha[nchar(alpha)-nchar(gsub("=","",alpha))==2]<- gsub("(a=.*)=.*","\\1",alpha[nchar(alpha)-nchar(gsub("=","",alpha))==2])
# get value
alpha<-gsub(".*[=]","",alpha)
# set lines in alpha without "/" but "adjust" to NA
i<-!unlist(grepl("[0-9]/[0-9\\.]",alpha))&unlist(grepl("adjust|correct",alpha))
if(sum(i)>0) alpha[i]<-NA
# delete after /num
alpha<-gsub("/[\\.0-9]*","",alpha)
# delete front
alpha<-gsub(".*[=<]","",alpha)
# numerise and round
alpha<-round(suppressWarnings(as.numeric(alpha)),5)
# only include 0 < alpha <= 1 and not NA
alpha<-alpha[alpha>0&alpha<=1]
alpha<-alpha[!is.na(alpha)]
if(length(alpha)==0) alpha<-character(0)
return(unique(c(alpha,standard_alpha)))
}   

####################################################
get.corrected<-function(x){
  ## extract lines with corrected alpha arror
  corrected<-grep("alpha error",x,value=TRUE)
  corrected<-grep("[0-9]/[0-9\\.]|adjust|correct",corrected,value=TRUE)
  if(length(corrected)>0){
    # convert "/" to decimal 
    corrected<-text2num(corrected,words=FALSE)
    # delete front
    corrected<-gsub(".*[=<]","",corrected)
    # remove non numbers at end
    corrected<-gsub("[^0-9]*$","",corrected)
    # numerise and round
    corrected<-round(suppressWarnings(as.numeric(corrected)),5)
    # only include 0 < alpha <= 1 and not NA
    corrected<-suppressWarnings(corrected[corrected>0&corrected<=1])
    corrected<-suppressWarnings(corrected[!is.na(corrected)])
}else corrected<-character(0)
return(unique(corrected))
}

## get alpha from 1-x% Confidence Intervall
get.ci<-function(x){
ci<-grep("[^a-z]ci[^a-rt-z]|[^a-z]ci$|confidence interval",x,value=TRUE)
# correct missing %
ci<-gsub("([^\\.0-9][8-9][0-9])( confidence inter)","\\1%\\2",ci)
ci<-gsub("([^\\.0-9][8-9][0-9])( ci[^a-rt-z])","\\1%\\2",ci)
# convert .95 to 95%
ci<-gsub("0*\\.([0-9][0-9]*)( confidence inter)","\\1%\\2",ci)
ci<-gsub("0*\\.([0-9][0-9]*)( ci[^a-rt-z])","\\1%\\2",ci)
# select lines with % or .num
ci<-grep("[\\%]|\\.[987]",ci,value=TRUE)
# remove space in front of %
ci<-gsub(" [%]","%",ci)
# remove text after %
ci<-gsub("[%].*","%",ci)
# shorten string
ci<-substr(ci,nchar(ci)-4,nchar(ci))
# remove text after .[0-9]
#ci<-gsub(".*(\\.[0-9]*).*","\\1",ci)
# remove till first num number 
ci<-gsub(".*[^0-9\\.\\%]","",ci)
# remove letters
ci<-gsub("[a-z\\=]","",ci)
# remove further punctuation
ci<-gsub(".*[<>\\{\\[]","",ci)
# select lines starting with "[7-9]"
ci<-grep("^[7-9]|^\\.[7-9]",ci,value=TRUE)
# % to number
ci<-gsub("[%]","e-02",ci)
# to alpha (1-CI)
ci<-round(1-suppressWarnings(as.numeric(ci)),4)
if(length(ci)==0) ci<-character(0)
return(unique(ci))
}


