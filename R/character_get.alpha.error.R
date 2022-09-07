#' get.alpha.error
#'
#' Extracts reported and corrected alpha error from text and 1-alpha confidence intervalls.
#' @param x text string to process.
#' @param p2alpha Logical. If TRUE detects and extracts alpha errors denoted with critical p-value (what may lead to some false positive detections). 
#' @param output One of c("list","vector"). If output="list" returns a list containing: alpha_error, \cr
#' corrected_alpha, alpha_from_CI, alpha_max, alpha_min. If output="vector" returns unique alpha errors but no distinction of types.
#' @return Numeric. Vector with identified alpha-error/s.
#' @export
#' @examples
#' x<-c("The threshold for significance was adjusted to .05/2",
#' "Type 1 error rate was alpha=.05.")
#' get.alpha.error(x)
#' x<-c("We used p<.05 as level of significance.",
#'      "We display .95 CIs and use an adjusted alpha of .10/3.",
#'      "The effect was significant with p<.025.")
#' get.alpha.error(x)

get.alpha.error<-function(x,p2alpha=FALSE,output="list"){
  # convert to sentences if x is of length 1
  if(length(x)==1) x<-text2sentences(x)
  # select lines with numbers  
  x<-grep("[0-9]",x,value=TRUE)
  ## unify
  # tolower
  x<-tolower(x)
  # remove alpha-num
  x<-gsub("alpha[-][0-9]|alpha [-][0-9]","",x)
  # intervals and bounds to interval
  x<-gsub("intervall*s*|bounds*","interval",x)
  # remove html
  x<-gsub("<[/a-z\\].*?[a-z]>","",x)
  # convert less synonyms to '<'
  x<-gsub("less equal | smaller | smaller than | below | lower than | lower "," < ",x)
  # convert "-" to space
  x<-gsub("  *"," ",gsub("[-]"," ",x))
  # remove hyphens
  x<-gsub("'","",x)
  # convert plural singular to alpha error 
  x<-gsub("values","value",x)
  x<-gsub("errors","error",x)
  x<-gsub(" was set| is set"," set",x)
  x<-gsub("([a-z])(\\.[0-9])","\\1 \\2",x)
  # convert synonyms to alpha error 
  x<-gsub("alpha error level|alpha error probability|type 1 error|type i error|critical p value|significance criterion|criterion of significance|statistical threshold|significance threshold","alpha error",x)
  #all operators to '='
  x<-gsub("[=][=]*","=",gsub("[<=>]","=",x)) 
  # remove years
  x<-gsub("[ \\(][1-2][0-9]{3}[^0-9]"," ",x)
  # select lines with numbers  
  x<-grep("[0-9]",x,value=TRUE)
  # unify probability of to 'p ='
  x<-gsub("probability [ao][tf] |probability [ao][tf] a |probability [ao][tf] alpha","p =",x)
  # convert 'p value' to 'p'  
  x<-gsub("p value [oa][ft] *[<=>]*|p value *[<=>]*","p =",x)
  # 10% to 0.1
  x<-gsub("([^0-9])10[ ]*\\%","\\10.1",x)
  # 5% to 0.05
  x<-gsub("([^0-9])5[ ]*\\%","\\10.05",x)
  # 1% to 0.01
  x<-gsub("([^0-9])1[ ]*\\%","\\10.01",x)
  
## unify alpha error representation
  # if has no alpha error yet
  if(length(grep("alpha error",x))==0){
    x<-gsub("alpha set to|alpha was set at|was set at alpha|two tailed alpha at|alpha level of|alpha level was|with alpha set to|alpha level of|tailed alpha at a","alpha error=",x)
    x<-gsub("level of alpha|alpha level|significance level|level of significance|level for significance|level for statistical significance|level of statistical significance|criterion for statistical significance|significance criterion|significant at alpha|level of significan[a-z]*","alpha error",x)
    x<-gsub("corrected error probability","corrected alpha error probability",x)
  }
# if still has no alpha error yet but .0[15], 0.1 or 0.001 or bonf|tukey|holm|FRD|hochberg|scheff convert alpha to alpha error
  if(length(grep("alpha error",x))==0){
    ind<-grep("\\.0[15]|\\.1[^0-9]|\\.001|cohen|bonff*err*on|tukey[^a-z]|post[- ]hoc|false discovery rate|[^a-z]fdr|[^a-z]hsd",x)
    x[ind]<-gsub("alpha *=* *.*(\\.1[^0-9])|alpha *=* *(\\.0[15][^0-9])|alpha *=* *(\\.001[^0-9])","alpha error=\\1\\2\\3",x[ind])
  }  
  # if line has 'alpha' and 'power, CI' or similar but no 'error|item|scale' add 'error' behind alpha
  i1<-grep("alpha",x)
  i2<-grep("power analy|power of|[0-9\\%] power|confidence interval|significance criterion|two[ -]tailed|one[ -]tailed|bonff*err*on|tukey[^a-z]|post[- ]hoc|false discovery rate|significance level|multiple comparison|significance at alpha|post hoc|a priori|significant at alpha|level of significance|consider[^0-9,;\\(]* significant",x)
  #  
  i3<-grep("error|item|scale| fail| reach",x,invert=T)
  i4<-i1[is.element(i1,i2)]
  i<-i4[is.element(i4,i3)]
  if(length(i>0)) x[i]<-gsub("alpha|alpha value|alpha level","alpha error",x[i])

  # remove word between alpha error and number if no number is in front
  x<-gsub("([^0-9]*)alpha error [^=\\.0-9,;]* ([\\.0-9])","\\1alpha error=\\2",x)

  ###################################
  ## convert p value to alpha error
  if(p2alpha==TRUE){
    # remove lines with p values with star/s in front
    x<-grep("\\*\\** *p *[<=>]* *[0-9\\.]",x,value=TRUE,invert=TRUE)
    # if line has 'p-value' and 'alpha error' synonym or 'correction' author but no 'alpha error' convert only standard p to alpha error
    i1<-grep("[^a-z]p[^a-z]",x)
    i2<-grep("alpha error|significance criterion|significance level|statistical significance|consider[^0-9,;\\(]* signif|significant at |level of signif|criterion for .*signific|[^a-z]cohen[^a-z]|bonff*err*on|tukey[^a-z]|false discovery rate|[^a-z]fdr|[^a-z]hsd",x)
    i3<-i1[is.element(i1,i2)]
    # exclude if has pattern
    i4<-grep("alpha error|scale|item| fail| reach",x,invert=T)
    i<-i3[is.element(i3,i4)]
    # include only those lines with standard p values
    i5<-grep("[ \\(]p[<=> 0]*\\.1[^0-9]|[ \\(]p[<=> 0]*\\.05[^0-9]|[ \\(]p[<=> 0]*\\.01[^0-9]",x)
    i<-i[is.element(i,i5)]
    if(length(i)>0) x[i]<-gsub("  *"," ",gsub("[ \\(]p[<=> 0]*(\\.1)[^0-9]|[ \\(]p[<=> 0]*(\\.05)[^0-9]|[ \\(]p[<=> 0]*(\\.01)[^0-9]"," alpha error \\1\\2\\3 ",x[i]))
  } # end p2alpha

  # remove lines with patterns  
  x<-grep("[^a-z]meta |[^a-z]meta-|amplitude|[^a-z]hz|[^a-z]volt|[^a-z]ampere|item|scale|cronbach",x,invert=TRUE,value=TRUE)
  # correct error of alpha
  x<-gsub("error [ao][ft] alpha","alpha error",x)
  x<-gsub("alpha error o*f* *p","alpha error",x)
  # if has number in front of alpha error move to back
  x<-gsub(" ([0-9\\.]*[0-9]) (alpha error)([^<=>][^<=>])"," \\2of \\1 ",x)
  # unify use of "="
  x<-gsub(" a priori"," ",x)
  x<-gsub(" of [<=]*| of a | value of "," = ",x)
  x<-gsub(" at [<=]*| at a "," = ",x)
  x<-gsub("[=] [=]","=",x)
  x<-gsub("  "," ",x)
  x<-gsub("[\\:] ([0-9\\.])","= \\1",x)

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
#  x<-gsub("alpha [<=] *0\\.1([^0-9])","alpha error = 0.1\\1",x)
#  x<-gsub("alpha [<=] *\\.1([^0-9])","alpha error = 0.1\\1",x)
#  x<-gsub("alpha [<=] *0\\.0([15][^0-9])","alpha error = 0.0\\1",x)
#  x<-gsub("alpha [<=] *\\.0([15][^0-9])","alpha error = 0.0\\1",x)
#  x<-gsub("alpha [<=] *0\\.00([1][^0-9])","alpha error = 0.00\\1",x)
#  x<-gsub("alpha [<=] *\\.00([1][^0-9])","alpha error = 0.00\\1",x)
  
  # only select lines with alpha error or CI
  x<-grep("alpha error|[^a-z]ci[^a-rt-z]|[^a-z]ci$|confidence interval",x,value=TRUE)
  # all operators to =
  x<-gsub("[=<>][=<>]*","=",x)
  x<-gsub(" *[=<>] *","=",x)
  
  # remove brackets from numbered %
  #  x<-gsub("\\(([0-9][0-9]*\\%)\\)","\\1",x)
  #  x<-gsub("\\[([0-9][0-9]*\\%)\\]","\\1",x)
  
  # split text
  x<-unlist(strsplit(x," and | with |, |; | \\(|\\)"))
  
## mark corrected alpha if has 'correction' but no standard alpha
  if(length(grep("correct|adjust|bonff*err*on|holm[^a-z]|tukey|benjamini|hochberg|hsd|fdr",x))>0){
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
  
  alphamax<-suppressWarnings(max(as.numeric(c(alpha,corrected,ci)),na.rm=T))
  alphamax[alphamax==-Inf]<-NA
  alphamin<-suppressWarnings(min(as.numeric(c(alpha,corrected,ci)),na.rm=T))
  alphamin[alphamin==Inf]<-NA
  
  if(output=="list") return(list(alpha_error=alpha,corrected_alpha=corrected,alpha_from_CI=ci,alpha_max=alphamax,alpha_min=alphamin))
  if(output=="vector") return(unique(as.numeric(c(alpha,corrected,ci))))

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
# if has two alpha errors
alpha<-unlist(strsplit2(alpha,"alpha error","before"))
# clean up
alpha1<-gsub("alpha error","alpha",alpha)
alpha1<-gsub(".*alpha [<=]|.*alpha[<=]|.*alpha of|.*alpha at ","alpha=",alpha1)
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
return(unique(c(alpha)))#,standard_alpha)))
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
    # exclude values < .05
    corrected<-corrected[which(corrected<.05)]
    # only include 0 < alpha <= 1 and not NA
    corrected<-suppressWarnings(corrected[corrected>0&corrected<=1])
    corrected<-suppressWarnings(corrected[!is.na(corrected)])
}else corrected<-character(0)
return(unique(corrected))
}

## get alpha from 1-x% Confidence Intervall
get.ci<-function(x){
# if line has standard CI but no % -> collapse with line in front
i1<-grep("[89][095]\\%",x)
i2<-grep("[^a-z]*ci[^a-rt-z]|[^a-z]*ci$|confidence interval",x)
# remove indices that appear in both results
i2<-i2[!is.element(i2,i1)]
# has % after ci
i<-i2[is.element(i2,i1-1)]
if(length(i)>0){
  x[i]<-paste(x[i],x[i+1],collapse=" ")
  x<-x[-(i+1)]
}
# if has % before ci paste
i<-i2[is.element(i2-1,i1)]
if(length(i)>0){
  x[i-1]<-paste(x[i-1],x[i],collapse=" ")
  x<-x[-i]
}
# select lines with CI
x<-grep("[^a-z]ci[^a-rt-z]|[^a-z]ci$|confidence interval",x,value=TRUE)
# remove text between % and CI
x<-gsub("(\\% ).*[^a-z](ci[^a-rt-z]*)|(\\% ).*( confidence interval)","\\1\\2\\3\\4",x)
# if has number behind CI but none in front change order
x<-gsub("([^0-9][^\\%]* [^a-z]*)ci[^a-rt-z]* ([0-9\\.]*?\\%)","\\1 \\2 ci",x)
# correct missing %
x<-gsub("([^\\.0-9][8-9][0-9])( confidence inter)","\\1%\\2",x)
x<-gsub("([^\\.0-9][8-9][0-9])( ci[^a-rt-z])","\\1%\\2",x)
# convert .95 to 95%
x<-gsub("0*\\.([0-9][0-9]*)( confidence inter)","\\1%\\2",x)
x<-gsub("0*\\.([0-9][0-9]*)( ci[^a-rt-z])","\\1%\\2",x)
# select lines with % or .num
ci<-grep("[\\%]|\\.[987]",x,value=TRUE)
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


