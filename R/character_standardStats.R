#' standardStats
#'
#' Extract and standard statistical results like Z, t, Cohen's d, F, eta^2, r, R^2, chi^2, BF_10, Q, U, H, OR, RR, beta values
#' @param x result of get.stats()
#' @param stats.mode Select subset of standard stats. One of: "all", "checkable", "computable", "uncomputable"
#' @param recalculate.p Logical. If TRUE recalculates p values (for 2 sided test) if possible
#' @param alternative Character. Select sidedness of recomputed p-values from t-, r- and beta-values. One of c("undirected","directed","both")
#' @param estimateZ Logical. If TRUE detected beta-/d-value is divided by reported standard error "SE" to estimate Z-value ("Zest") for observed beta/d and recompute p-value. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE capital letter T is treated as t-statistic
#' @param R2r Logical. If TRUE capital letter R is treated as correlation
#' @param rm.na.col Logical. If TRUE removes all columns with only NA
#' @importFrom stats pf pchisq pt pnorm
#' @export
#' @examples
#' x<-c("t(38.8)<=>1.96, p=.002","F(2,39)<=>4, p<=>.05",
#' "U(2)=200, p>.25","Z<=>2.1, F(20.8,22.6)=200, p<.005, 
#' BF(01)<=>4","chi=3.2, r(34)<=>-.7, p<.01, R2=76%.")
#' standardStats(x)

standardStats<-function(x,stats.mode="all",recalculate.p=TRUE,alternative="undirected",estimateZ=FALSE,T2t=FALSE,R2r=FALSE,rm.na.col=TRUE){
# set warning massages to FALSE
warn.T2t<-FALSE;warn.R2r<-FALSE;warn.r<-FALSE;warn.R2<-FALSE;warn.p<-FALSE;warn.d<-FALSE;warn.eta<-FALSE;warn.multi.p<-FALSE
   x<-unlist(x)
# convert with get.stats() if has " [<=>] [0-9\\.-]"
if(length(grep(" [<=>] [0-9\\.-]|[<=>] [0-9\\.-]",x))>0) x<-allStats(x)
if(length(x)>0){
# take copy for raw output
y<-x

# remove space between letter and (
x<-gsub("([A-Za-z]) (\\([0-9Nnd])","\\1\\2",x)
# remove 'Letter'chi'letter'
x<-gsub("[A-Za-z]chi[a-z]","",x)

# capital T to small t
   if(T2t==TRUE){
     # need warning for T2t?
     if(length(grep("T",x))>0) warn.T2t<-TRUE
     x<-gsub("T","t",x)  
     }

# capital R to small r
   if(R2r==TRUE){
        # need warning for T2t?
     if(length(grep("^R\\(| R \\(|^R\\(| R\\(|^R[<>=]| R[<>=]",x))>0) warn.R2r<-TRUE
     x[grep("^R\\(| R\\(|^R[<>=]| R[<>=]",x)]<-gsub("R","r",x[grep("^R\\(| R\\(|^R[<>=]| R[<>=]",x)])  
     }
     
     
 # remove percent value in brackets
   x<-gsub(" \\([0-9\\.]*\\%\\)","",x)     
   # remove ")" in "), text...
   x<-gsub("[\\)\\][;,] ([a-zA-Z])",", \\1",x)
   # unify "[]" -> "()" ??
   x<-gsub("\\[","(", x)
   x<-gsub("\\]",")", x)

# remove label from one letter statistic
   x<-gsub("( [ZtFr]) [a-zA-Z]* (\\([0-9])","\\1\\2",x)
   x<-gsub("^([ZtFr]) [a-zA-Z]* (\\([0-9])","\\1\\2",x)
   
# function to convert percent to number   
percent2number<-function(x){
if(length(grep("\\%|[0-9] percent",x))>0){
    x<-gsub("([0-9]) percent","\\1%",x)
    x<-gsub("([0-9]) \\%","\\1%",x)
    x<-unlist(strsplit2(x,"[0-9][%]","after"))
    i<-grep("[0-9][%]|[0-9][%]$",x)
    if(length(i)>0){
        stop<-FALSE
        while(stop!=TRUE){
            i<-grep("[0-9][%]|[0-9][%]$",x)[1]
            m <- regexpr("[0-9\\.]*?[%]|[0-9\\.]*?[%]$", x[i])
            remove<-regmatches(x[i], m)
            insert<-as.numeric(gsub("%","",remove))/100
            x[i]<-gsub(remove,insert,x[i])
            if(length(grep("[0-9][%]|[0-9][%]$",x))==0) stop<-TRUE
        }
    }
# clean up
    x<-gsub("  "," ",x<-paste(x,collapse=" "))
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$",".",x)
    }
    return(x)
}
# use function to convert %
x<-unlist(lapply(x,percent2number))
   # remove ^ from "letter^2"
   x<-gsub("([a-zA-Z])[\\^]2","\\12",x)
   # unify p-value
   x<-gsub("([^a-z])p[- ]value","\\1p",x)
   # remove " all"
   x<-gsub("all ","",x)
   x<-gsub("-","-",x)
   # remove ) in "=num);"
   x<-gsub("([<>=][-0-9\\.]*?)\\)([,;])","\\1\\2",x)
   # remove "|" 
   x<-gsub("\\| | \\|| \\| |\\|","",x)
   # space clean up
   x<-gsub("^ *|(?<= ) | *$", "", x, perl = TRUE)
   # remove " partial " "change"
   x<-gsub(" [pP]artial | [Cc]hange |[pP]artial|[Cc]hange","",x)
   # remove hyphen
   x<-gsub("'","",x)
   x<-unlist(strsplit(x,"children|child|childrens"))
   # remove " [text]"
   x<-gsub(" \\[[a-zA-Z]*?\\]","",x)
   x<-gsub("\\[[a-zA-Z]*?\\]","",x)
   # add space between letter=letter
   x<-gsub("([a-zA-Z])=([a-zA-Z])","\\1 \\2",x)
   # convert prob to p
   x<-gsub("[pP]robability|[pP]rob","p",x)
   # remove brackets with letters, operator only
   x<-gsub("\\([a-zA-Z<>=]*\\)","",x)
   # remove brackets with letters, followed by numbers  only
   x<-gsub("\\([a-zA-Z][0-9]*\\)","",x)
   x<-gsub("\\([a-zA-Z][a-zA-Z]*[0-9]*\\)","",x)
   # unify delta
   x<-gsub("delta([<=>])","d\\1",x)
   # remove delta if not followed by [<>=]
   x<-gsub("[dD]elta ([^<=>])","\\1",x)
   x<-gsub("[dD]elta([^ <=>])","\\1",x)
   # unify beta
   x<-gsub("\u00DF","beta",x) # sharp s
   x<-gsub("\u0392","beta",x) # greek capital BETA
   x<-gsub("[Bb]etas","beta",x) # singular
   # unify chi2s -> chi2
   x<-gsub("X2|[Cc]hi2s|X 2 ","Chi2",x) 
   x<-gsub("[Cc]hi[- ][Ss]quare|[Cc]hisquare","Chi2",x) 
   x<-gsub("[Cc]hi2","Chi2",x) 
   x<-gsub("[Cc]hi2 \\(","chi2(",x) 
   x<-gsub("[Cc]hi2","Chi2",x) 
   x<-gsub("^[Xx](\\([0-9]*\\))2","chi2\\1",x)
   x<-gsub("[^a-zA-Z][Xx](\\([0-9]*\\))2"," chi2\\1",x)
   x<-gsub("^[Xx](\\([0-9]*\\))","chi2\\1",x)
   x<-gsub("[^a-zA-Z][Xx](\\([0-9]*\\))"," chi2\\1",x)
   x<-gsub("\\(chi","chi",x)
   # add space after captial letter in front of chi
   x<-gsub("([A-Z])chi","\\1 chi",x)
   # correct ",[a-zA-Z]" to", [a-zA-Z]" 
   x<-gsub("([;,])([a-zA-Z])","\\1 \\2",x)
   # remove "-" between letters 
   x<-gsub("([a-z])-([A-Ra-z])","\\1 \\2",x) 
   # remove wrongly set "="
   x<-gsub("=(\\([0-9\\.,]*?\\)=)","\\1",x)
   # unify eta2(p/g)
   x<-gsub("eta2\\([GgPp]\\)|eta\\([GgPp]\\)|eta\\([GPpg]\\)2|eta2[GPgp]","eta2",x)
   x<-gsub("eta G|eta\\([gG]\\)","eta",x)
   x<-gsub("eta2|eta2 G|eta2\\([gG]\\)","eta2",x)
   # unify SE
   x<-gsub("[sS]\\.[eE]\\.","SE",x)
   # plural to singular
   x<-gsub("([^a-z][tFrpZd])s([^a-z])","\\1\\2",x)
   x<-gsub("^([tFrpZd])s([^a-z])","\\1\\2",x)
   x<-gsub("([pPtFraid2])[' ]s([<=>])","\\1\\2",x)
   x<-gsub("([pPtFraid2])' s([<=>])","\\1\\2",x)
   x<-gsub("\\)s([<=>])",")\\1",x)
  # add coma after number followed by letter of satistic
   x<-gsub("([0-9]) ([a-zA-z])","\\1, \\2",x)
 
# correct f(df1,df2) -> F(df1,df2)
   x<-gsub("([^a-zA-Z])f(\\([0-9]*?,[ 0-9]*?\\))","\\1F\\2",x)
   x<-gsub("([^a-zA-Z])f (\\([0-9]*?,[ 0-9]*?\\))","\\1F\\2",x)
   x<-gsub("^f(\\([0-9]*?,[ 0-9]*?\\))","F\\1",x)
   x<-gsub("^f (\\([0-9]*?,[ 0-9]*?\\))","F\\1",x)
   # remove all second results (.num.num)
   x<-gsub("(\\.[0-9]*?)\\.[0-9\\.]*","\\1",x)
   #   gsub("([<=>][\\.0-9]*?)[^a-zA-Z]*","\\1 ",x)
# remove coma in BF(0,1), B
  x<-gsub("(BF[(][01])[,;]([01][)])","\\1\\2",x)
# remove front of F value   
  x<-gsub("[a-zA-Z]*?(F[(][0-9\\.]*?,)","\\1",x)
# remove space between letter or 2 and "(num"
  x<-gsub("([A-Za-z2]) (\\([0-9])","\\1\\2",x)
# remove label from "statistic label ([0-9]"
  x<-gsub("^([rtFQH])[a-z0-9]*? (\\([0-9])","\\1\\2",x)
  x<-gsub("^([rtFQH]) [a-z0-9]*?(\\([0-9])","\\1\\2",x)
  x<-gsub("^([tFQH])[a-z0-9]*?(\\([0-9])","\\1\\2",x)
  x<-gsub("2[a-z0-9](\\([0-9])","2\\1",x)
  x<-gsub("2 [a-z0-9](\\([0-9])","2\\1",x)
  x<-gsub("([^a-zA-Z][rtFQH]) [a-z0-9]*?(\\([0-9])","\\1\\2",x)
  x<-gsub("([^a-zA-Z][tFQH])[a-z0-9]*? (\\([0-9])","\\1\\2",x)
  x<-gsub("([^a-zA-Z][tFQH])[a-z0-9]*?(\\([0-9])","\\1\\2",x)
  # remove [ in front of letter(
  x<-gsub("\\[([a-zA-z][(][0-9])","\\1",x)
  # remove [ in front of words
  x<-gsub("\\[([a-zA-z]*? [a-zA-z]*? )","\\1",x)
  # remove ; in front of words
  x<-gsub("\\; ([a-zA-z]*? [a-zA-z]*? )"," \\1",x)
  
  # unify use of [0-9]^-[1-9] 
if(length(grep("([0-9]*?)\\^\\-[0-9]",x))>0){
# index
i<-grep("([0-9]*?)\\^\\-[0-9]",x)
v<-as.numeric(gsub(".*[^0-9\\.]","",gsub("([0-9]*?)\\^\\-[0-9\\.].*","\\1",x[i])))
e<-as.numeric(gsub("[^0-9\\.].*","",gsub(".*\\^\\-([0-9\\.]*?)","\\1",x[i])))
input<-round(1/(v^e),7)
 for(j in 1:length(i)){
  x[i[j]]<-gsub("([0-9\\.]*?)\\^\\-[0-9\\.]*",input[j],x[i[j]])
 }
}

# unify use of e^[\\-1-9] 
 x<-gsub("([0-9])[Ee]\\^-","\\1e-",x)
 x<-gsub("([0-9])[Ee]\\^\\+","\\1e+",x)
 x<-gsub("\\^\\+","e+",x)
 x<-gsub("([0-9])[Ee]\\^([0-9])","\\1e+\\2",x)
 x<-gsub("([0-9])[Ee]([0-9])","\\1e+\\2",x)

# remove second number after second operator: ps<.9<05/6 ->ps<.9
 x<-gsub("([0-9\\.][0-9])[<>=]+[0-9\\./]*","\\1",x)
 x<-gsub("([0-9\\.]/[0-9])[<>=]+[0-9\\./]*","\\1",x)
 
# function to convert e num
e2num<-function(x){
if(length(grep("[0-9][Ee][-\\+\\.0-9]",x))>0){
    x<-gsub("([0-9])[Ee]([0-9])","\\1e+\\2",x)
    x<-gsub("([0-9])E([-\\+\\.0-9])","\\1e\\2",x)
    x<-unlist(strsplit2(x," [-\\.0-9]*e[\\-\\+][1-9]","before"))
    # add space to end
    x<-paste0(x,"  ")
    # if has one e[\\-\\+1-9] convert to number and replace with result
    ind<-(1:length(x))[nchar(x)-nchar(gsub("[0-9]e-[\\.0-9]","",x))==4]
    suppressWarnings(if(length(ind)>0) for(i in ind) x[i]<-gsub("([-0-9\\.]*)e(-[0-9\\.]*)",format(round(as.numeric(gsub(".*?([-0-9\\.]*)e(-[0-9\\.]*).*","\\1e\\2",x[i])),10),scientific=F),x[i])
    )
    ind<-(1:length(x))[nchar(x)-nchar(gsub("[0-9]e\\+[\\.0-9]","",x))==4]
    suppressWarnings(if(length(ind)>0) for(i in ind) x[i]<-gsub("([-0-9\\.]*)e(\\+[0-9\\.]*)",format(as.numeric((gsub(".*?([-0-9\\.]*)e(\\+[0-9\\.]*).*","\\1e\\2",x[i]))),scientific=F),x[i]) )
    # collapse and clean up
    x<-gsub("  "," ",gsub("  "," ",gsub(" $","",paste(x,collapse=" "))))
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$",".",x)
}
    return(x)
}

# function to convert ^num
hight2num<-function(x){
if(length(grep("[0-9]\\^[-\\.0-9]",x))>0){
x<-unlist(strsplit2(x,"\\.$","before"))
x<-unlist(strsplit2(x,"[^-\\.0-9][-\\.0-9]*?\\^[-\\.0-9]","before"))
    # add space to end
    x<-paste0(x," ")
    # if has num^num calculate and and replace 
    ind<-grep("[^a-zA-Z][0-9]\\^[-\\.0-9]|^[0-9]\\^[-\\.0-9]",x)
    exponent <- function(a, pow) (abs(a)^pow)*sign(a)
    res<-suppressWarnings(format( exponent(as.numeric(gsub(".*[^-0-9\\.]","\\1",
                                                 gsub("(.*[-0-9\\.]*)\\^[-\\.0-9].*","\\1",x[ind]))),
                                       as.numeric(gsub("[^-0-9\\.].*","\\1",gsub(".*[-0-9\\.]*?\\^([-\\.0-9]*)","\\1",x[ind])))),
                                 scientific=F))

        # clean up white spaces
    res<-gsub("^ *|(?<= ) | *$", "", res, perl = TRUE)
    # remove only zeros at end
    res<-gsub("\\.[0]*$","",res)
    res<-gsub("(\\.[0-9]*?)0*$","\\1",res)
    # insert result
    suppressWarnings(if(length(ind)>0) for(i in 1:length(ind)) x[ind[i]]<-gsub("[-0-9\\.]*\\^[-\\.0-9]*",res[i],x[ind[i]]))
    # collapse
    x<-gsub("  "," ",gsub(" $","",paste(x,collapse=" ")))
    # clean up
    x<-gsub("  "," ",x)
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$|,\\.",".",x)
}
    return(x)
}


# split and insert results of functions 
x<-strsplit2(x,"[,;] ","after")
x<-lapply(x,e2num)
x<-lapply(x,hight2num)
# paste into lines again
x<-unlist(lapply(x,paste,collapse=""))

# remove "n=num"
x<-gsub(", [Nn]=[0-9]*","",x)
x<-gsub("[Nn]=[0-9]*, ","",x)

# convert fraction to digit number
frac2num<-function(x){
if(length(grep("/[-\\.0-9]|/ [-\\.0-9]",x))>0){
    x<-unlist(strsplit2(x,"\\.$","before"))
    x<-gsub("([0-9]) /([-\\.0-9])","\\1/\\2",x)
    x<-gsub("([0-9]) / ([-\\.0-9])","\\1/\\2",x)
    x<-gsub("([0-9])/ ([-\\.0-9])","\\1/\\2",x)
    x<-unlist(strsplit2(x,"[^-\\.0-9][-\\.0-9]*?/[-\\.0-9]","before"))
    # get lines with fraction
    ind<-grep("[0-9]/[-0-9\\.]",x)
    # lines with only one fraction
    ind<-ind[nchar(x[ind])-nchar(gsub("/","",x[ind]))==1]
    if(length(ind)>0){
        # get num/num
        frac<-regmatches(x[ind],regexpr("[-\\.0-9]*/[-\\.0-9]*",x[ind]))
        # recompute num=num/num
        num<-sapply(frac, function(x) eval(parse(text=x)))
        num<-as.character(round(num,4))
        # insert num
        for(i in 1:length(ind)) x[ind[i]]<-gsub("([-\\.0-9]*/[-\\.0-9]*)",num[i],x[ind[i]])
    }
    # collapse and clean up
    x<-gsub("  "," ",gsub("  "," ",gsub(" $","",paste(x,collapse=" "))))
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$",".",x)
}
    return(x)
}
# apply
x<-unlist(lapply(x,frac2num))

# prepare results colnames
cnames<-c("result","Z_op","Z","F_op","F","eta2","omega2","t_op","t","d","SE","r_op","r","R2_op","R2","U_op","U","H_op","H","G2_op","G2","OR","RR","Chi2","Q_op","Q","df1","df2","beta","SEbeta","Zest","BF10_op","BF10","BF_op","BF","p_op","p","recalculatedP","p_H0_less","p_H0_greater")
res<-matrix(NA,nrow=length(x),ncol=length(cnames))
colnames(res)<-cnames

## extract beta and d and standarderror than calculate Zest
# unify
x<-gsub("[Ss]lope|[Bb]eta"," b",x)
x<-gsub(" [Ss][Ee]\\([a-zA-Z]*)"," SE",x)
x<-gsub(" se([<=>])"," SE\\1",x)
x<-gsub("^B([<=>])","b\\1",x)
x<-gsub(", B([<=>])",", b\\1",x)
x<-gsub("  "," ",x)
# unify Cohen's d
x<-gsub("Cohen.s d.*?([<=>])","d\\1",x)
x<-gsub("Cohens d.*?([<=>])","d\\1",x)

# if has beta extract beta and SE beta
if(length(grep("^b[<=>]| b[<=>]",x))>0){
   index<-grep("^b[<=>]| b[<=>]",x)
   # extract
   beta<-suppressWarnings(as.numeric(gsub("[,; ].*","",unlist(lapply(strsplit(x[index]," b[<=>]*|^b[<=>]*"),"[",2)))))
   SE<-suppressWarnings(as.numeric(gsub("[,; ].*","",gsub(".* SE[<=>]*","",x[index]))))
   # add to res
   res[index,"beta"]<-beta
   res[index,"SEbeta"]<-SE
   res[index,"result"]<-y[index]
   if(estimateZ==TRUE) res[index,"Zest"]<-beta/SE
   
}

## get d and SE if has d and SE, t or Z error than calculate Zest
if(length(grep("^d[<=>]| d[<=>]",x))>0&length(grep(" SE[<=>]|^t\\([0-9]| t\\([0-9]|[Zbzt]=",x))>0){
   index<-grep("^d[<=>]| d[<=>]",x)
   # extract
   d<-suppressWarnings(as.numeric(gsub("[,; ].*","",gsub(".* d[<=>]*|^d[<=>]*","",x[index]))))
   SE<-suppressWarnings(as.numeric(gsub("[,; ].*","",gsub(".* SE[<=>]*","",x[index]))))
   # add to res
   res[index,"d"]<-d
   res[index,"SE"]<-SE
   res[index,"result"]<-y[index]
   if(estimateZ==TRUE) res[index,"Zest"]<-d/SE
}

## if has SE but no beta or d extract SE
if(length(grep(" SE[<=>]",x))>0&length(grep("^b[<=>]| b[<=>]",x))==0&length(grep("^d[<=>]| d[<=>]",x))==0){
   index<-grep(" SE[<=>]",x)
   # extract
   SE<-suppressWarnings(as.numeric(gsub("[,; ].*","",gsub(".* SE[<=>]*","",x[index]))))
   # add to res
   res[index,"SE"]<-SE
   res[index,"result"]<-y[index]

}

## extract t value and df in t value
# remove "(" in front of t
x<-gsub("(\\()t"," t",x)
# remove number of numbered t-values
x<-gsub("( )t[0-9]*?\\(([1-9])|^t[0-9]*?\\(([1-9])","\\1t(\\2\\3",x)
x<-gsub("( )t[a-zA-Z]\\(([1-9])|^t[a-zA-Z]\\(([1-9])","\\1t(\\2\\3",x)
# lines with t values
index<-grep(" t[<>=]|^t[<>=]| t[(][0-9 df=]*[)]|^t[(][0-9 df=]*[)]",x)
if(length(index>0)){
  tval<-x[index]
# remove till first t value if has 2
  ft<-function(x){lapply(strsplit2(tval,"^t[<>=(]| t[<>=(]","before"),function(x) grep("^t[<>=(]| t[<>=(]",x,value=TRUE)[1])}
  tval<-unlist(ft(tval))
  tval<-gsub(".* t([(<>=])","t\\1",tval)
  # remove'df='
  tval<-gsub("\\(df=([1-9])","(\\1",tval)
  # df
  tdf<-rep(NA,length(tval))
  # index for lines with df
  ind<-grep(" t[(]|^t[(]",tval)
  if(length(ind)>0) tdf[ind]<-gsub("[,;].*","",gsub("[)].*","",gsub(".*t[(]","",tval[ind])))
  ind<-grep(" df=",tval)
  if(length(ind)>0) tdf[ind]<-gsub(",;.*","",gsub("[^0-9\\.].*","",gsub(".* df=","",tval[ind])))
  # get sign
  sign<-gsub("[^<>=].*","",sub("[^<>=]*([=<>])", "\\1",sub(".* t([\\(=<>])","\\1",tval)))
  # clean up t value
  tval<-gsub("\\([0-9\\.,;]*\\)|\\]","",tval) # remove df within brackets
  tval<-suppressWarnings(as.numeric(gsub(".*[=<>]","",gsub("[;,] .*| .*|[;,]$","",gsub(".*t[(<>=]|^t[(<>=]","",tval)))))
  # insert results to res
  res[index,c("t_op","t","df2")]<-cbind(sign,tval,tdf)
  res[index,"result"]<-y[index]
}

## get F-value and its df1 and df2
# remove number of numbered F-values
x<-gsub("([ \\[\\(])F[0-9]*?\\(([1-9])|^F[0-9]*?\\(([1-9])","\\1F(\\2\\3",x)
x<-gsub(",F", ", F",x)
# remove letter of labeled F-values in lines with (df1,df2)
if(length(grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)",x))>0){ 
  x[grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)",x)]<-gsub("([ \\[\\(])F[a-zA-Z ]*?\\(([1-9])|^F[a-zA-Z ]*?\\(([1-9])","\\1F(\\2\\3",x[grep("\\([0-9\\.]*?[,;](+)?[0-9\\.]*?\\)",x)])}

# extract eta2 
ind<-grep(" eta[2<=>]|^eta[2<=>]",x)
if(length(ind)>0){
eta<-rep(NA,length(ind))
 eta<-gsub("[^0-9\\.].*","",gsub(".*[<=>]","",gsub("[,;] .*| [a-zA-Z].*","",unlist(lapply(strsplit(x[ind]," eta|^eta"),"[",2)))))
# insert results to res
res[ind,"eta2"]<-eta
res[ind,"result"]<-x[ind]
}

# extract omega2 
ind<-grep(" omega[2<=>]|^omega[2<>=]",x)
if(length(ind)>0){
omega<-rep(NA,length(ind))
 omega<-gsub("[^0-9\\.].*","",gsub(".*[<=>]","",gsub("[,;] .*| [a-zA-Z].*","",unlist(lapply(strsplit(x[ind]," omega|^omega"),"[",2)))))
# insert results to res
res[ind,"omega2"]<-omega
res[ind,"result"]<-x[ind]
}


# extract F-values without (df1,df2)  in brackets
index<-grep("^F[<=>][0-9\\.]| F[<=>][0-9\\.]|^F[<=>]{2}[0-9\\.]| F[<=>]{2}[0-9\\.]|^F[<=>]{3}[0-9\\.]| F[<=>]{3}[0-9\\.]",x)
# extract F
if(length(index)>0){
Fval<-x[index]
sign<-sub(".*F([<=>]*).*","\\1",Fval)
# try get signs for those without sign yet
sign[grep("^[<>=]",sign,invert=T)]<-sub("^F([<=>]*).*","\\1",Fval[grep("^[<>=]",sign,invert=T)])
# get df1= and df2= if has these
df1<-rep(NA,length(Fval))
df2<-rep(NA,length(Fval))
# get df 1 and 2 if has df1= and df2=
ind<-grep("df1=",Fval)
if(length(ind)>0) df1[ind]<-gsub("[^0-9\\.].*","",gsub(".*df1[=]","",Fval[ind]))
ind<-grep("df2=",Fval)
if(length(ind)>0) df2[ind]<-gsub("[^0-9\\.].*","",gsub(".*df2[=]","",Fval[ind]))

if(length(grep("[^A-Za-z]F[<=>]*([0-9\\.]*)",Fval))>0) Fval<-(sub(".*F[<=>]*([0-9\\.]*).*","\\1",Fval))
if(length(grep("^F[<=>]*([0-9\\.]*)",Fval))>0) Fval<-(gsub("^F[<=>]*([0-9\\.]*).*","\\1",Fval))
Fval<-suppressWarnings(as.numeric(Fval))

# has F value without dfs? than don't compute pF
# if(length(Fval)==!is.na(df2)) 

# don't overwrite dfs (here copy value if not NA)
if(length(index)==1){
df2[which(!is.na(res[index,"df2"]))]<-res[index,"df2"][!is.na(res[index,"df2"])]
df1[which(!is.na(res[index,"df2"]))]<-res[index,"df1"][!is.na(res[index,"df2"])]
}
if(length(index)>1){
df2[which(!is.na(res[index,"df2"]))]<-res[index,][!is.na(res[index,"df2"]),"df2"]
df1[which(!is.na(res[index,"df2"]))]<-res[index,][!is.na(res[index,"df2"]),"df1"]
}


# insert results to res
res[index,c("F_op","F","df1","df2")]<-cbind(sign,Fval,df1,df2)
res[index,"result"]<-y[index]
}

# F-values with (df1,df2)
index<-grep("^F[\\[\\(]|[^A-Z]F[\\[\\(]",x)
if(length(index)>0){
# remove till df1
Fval<-gsub(".*[^A-Z]F[\\[\\(]|^F[\\[\\(]","",x[index])
# remove df's
 Fval<-gsub("\\(df=([1-9])","(\\1",Fval)
 Fval<-gsub(" df=([1-9])"," \\1",Fval)
 Fval<-gsub(" df[1-9]=([1-9])","\\1",Fval)
 Fval<-gsub("df[1-9]=([1-9])","\\1",Fval)

# if has no "<=>" left set to NA
i<-grep("[<=>][0-9\\.]",Fval,invert=TRUE)
Fval[i]<-NA
# extract df1 by removing text from first non number or \\.
df1<-gsub("[^0-9\\.].*","",Fval)
# If has 2 df's, extract df2 by removing text behind first ")" than til first non number or \\.
temp<-gsub("[)].*|[=].*","",Fval)
#temp<-gsub("\\.","",temp)
df2<-rep(NA,length(Fval))
df2[grep("[^0-9\\.]",temp)]<-gsub("[^0-9\\.]","",sub(".*[^0-9\\.]","",temp[grep("[^0-9\\.]",temp)]))

# get F sign
sign<-gsub("[^<>=].*","",sub("[^<>=]*([=<>])", "\\1",Fval))
# get F sign
sign<-gsub("[^<>=].*","",sub("^.*?[)]", "",Fval))
# extract F value by split at "=<>" and selection of second element from list, than remove from first nun number or"."
#Fval<-gsub("[^0-9\\.].*","",unlist(lapply(strsplit(Fval,"[=<>]"),"[",2)))
Fval<-suppressWarnings(as.numeric(gsub("[^0-9\\.].*","",gsub("[<=>]","",sub("^.*?[)]", "",Fval)))))

# correct if only has 1 df
df1[is.na(df2)]<-NA
df2[is.na(df1)]<-NA
Fval[is.na(df1)]<-NA
sign[is.na(df1)]<-NA

# insert results to res
res[index,c("F_op","F","df1","df2")]<-cbind(sign,Fval,df1,df2)
res[index,"result"]<-y[index]
}


## extract chi2 and df
# unify Chi2 -> chi
x<-gsub("[Cc]hi2","chi",x)
x<-gsub("([a-zA-Z])[Cc]hi","\\1 chi",x)
# remove text from chi_text(12)=
if(length(grep("^chi[A-Za-z]| chi[A-Za-z]",x))>0) x[grep("^chi[A-Za-z]| chi[A-Za-z]",x)]<-gsub("chi[A-Za-z]*?([\\(<>=])","chi\\1",x[grep("^chi[A-Za-z]| chi[A-Za-z]",x)])
index<-grep(" chi[<=>(]|^chi[<=>(]",x)
if(length(index)>0){
chi2<-x[index]
chidf<-rep(NA,length(chi2))
# find df by coding chi(df)
ind<-grep("chi[(]",chi2)
chidf[ind]<-gsub("[^0-9\\.].*","",gsub(".*chi[(]","",chi2[ind]))
# find df by coding chi(N=[0-9]*, df)
ind<-grep("chi[(][Nn][=]",chi2)
if(length(ind)>0) chidf[ind]<-gsub("[^0-9\\.].*|\\).*","",gsub(".*chi[(][Nn]=[0-9]*[, ;]*","",chi2[ind]))
# find df by coding chi(df, N=[0-9])
ind<-grep("chi[(][0-9]*?[,;] [Nn][=]",chi2)
if(length(ind)>0) chidf[ind]<-gsub("[^0-9\\.].*|\\).*","",gsub(".*chi[(]","",chi2[ind]))
# find df by coding df=
ind<-grep("df[=]",chi2)
chidf[ind]<-gsub("[^0-9\\.].*","",gsub(".*df[=]","",chi2[ind]))

# remove df's
 chi2<-gsub("\\(df=([1-9])","(\\1",chi2)
 chi2<-gsub(" df=([1-9])"," \\1",chi2)
 chi2<-gsub("\\(df[1-9]=([1-9])","(\\1",chi2)
 chi2<-gsub(" df[1-9]=([1-9])"," \\1",chi2)
# remove "n=num" at end
chi2<-gsub("[;,] [Nn]=[0-9]*?$","",chi2)
# extract chisqure result
chi2<-suppressWarnings(as.numeric(gsub("[^0-9\\.].*","",unlist(lapply(strsplit(gsub(".*chi[(]|.* chi|.* [Nn][=]","",chi2),"<=>|=|<=|>=|<|>"),"[",2)))))
# add to results
res[index,c("Chi2")]<-cbind(chi2)
res[index,"result"]<-y[index]
# if df 1 has no entry
i<-which(is.na(res[index,c("df1")]))
res[index,c("df1")][i]<-cbind(chidf[i])
x[index]<-gsub("^chi([<=>\\( ])","chi2\\1",x[index])
x[index]<-gsub(" chi([<=>\\( ])"," chi2\\1",x[index])
}

## extract r
# unify rho and rsp
x<-gsub("rho|r[Ss][Pp]|corr|cor","r",x)
index<-grep("[\\( ]r[<=>]|^r[<=>]|[\\( ]r \\([0-9]|^r \\([1-9]|[\\( ]r\\([0-9]|^r\\([1-9]",x)
if(length(index)>0){
r<-x[index]
# get df if has one
rdf<-rep(NA,length(r))
idf<-grep("[^a-z]r\\([1-9]|^r\\([1-9]",r)
if(length(idf)>0){
  rdf[idf]<-gsub("[^0-9\\.].*","",unlist(lapply(strsplit(r[idf],"r\\("),"[",2)))
}

# overwrite df if has df=
ind<-grep(" df=",r)
if(length(ind)>0) rdf[ind]<-gsub("[^0-9\\.].*","",gsub(".* df=","",r[ind]))

# remove df in brackets
r<-gsub("\\([0-9\\.]*?\\)","",r)
# remove N=
r<-gsub("[Nn]=","",r)

r<-gsub(".*? r(=)"," r\\1",r)
r<-gsub(".*? r(<)"," r\\1",r)
r<-gsub(".*? r(>)"," r\\1",r)

# get sign
rsign<-gsub("[^<>=].*","",gsub("r","",unlist(lapply(strsplit(r,"[,; ]"),function(x) grep("r[<>=]",x,value=TRUE)[1]))))
# get r value
r<-suppressWarnings(as.numeric(gsub("[^0-9\\.-].*","",gsub("[<=>]","",unlist(lapply(strsplit(r,"r[<=>]"),"[",2))))))
# add to results
res[index,"r_op"]<-rsign
res[index,"r"]<-r
res[index,"result"]<-y[index]
## only add df2 if no df2 has been recorded yet
use<-is.na(res[index,"df2"])
res[index,"df2"][use]<-rdf[use]
}

## extract H
index<-grep("[\\( ]H[<=>]|^H[<=>]|[\\( ]H \\([0-9]|^H \\([1-9]|[\\( ]H\\([0-9]|^H\\([1-9]",x)
if(length(index)>0){
H<-x[index]
H<-gsub(", [Nn]=[0-9]*","",H)
H<-gsub("[Nn]=[0-9]*, ","",H)

# get df if has one
Hdf<-rep(NA,length(H))
if(length(grep("[^a-z]H\\([1-9]|^H\\([1-9]",H))>0){
  Hdf[grep("[^a-z]H\\([1-9]|^H\\([1-9]",H)]<-gsub("\\).*","",gsub(".*H\\(","",H[grep("[^a-z]H\\([1-9]|^H\\([1-9]",H)]))
}
H<-gsub("[(][1-9].*?H(=)","\\1",H)
H<-gsub("[(][1-9].*?H(<)","\\1",H)
H<-gsub("[(][1-9].*?H(>)","\\1",H)
# remove df in brackets
H<-gsub("\\([0-9]*?\\)","",H)
# get sign
Hsign<-gsub("[^<>=].*","",sub("[^<>=]*([=<>])", "\\1",sub(".*H([\\(=<>])","\\1",H)))
# get H value
H<-suppressWarnings(as.numeric(gsub("[^0-9\\.-].*","",gsub("[<=>]","",unlist(lapply(strsplit(H,"H[<=>]"),"[",2))))))
# add to results
res[index,"H_op"]<-Hsign
res[index,"H"]<-H
res[index,"result"]<-y[index]
## only add df2 if no df2 has been recorded yet
use<-is.na(res[index,"df1"])
res[index,"df1"][use]<-Hdf[use]
}

## extract G2
# remove [Dd] in front of G2
x<-gsub("[Dd]G2","G2",x)
# remove [^a-zA-Z] in front of G2
x<-gsub("[^a-zA-Z]G2"," G2",x)
# remove text from G2_text(12)=
if(length(grep("^G2[A-Za-z]| G2[A-Za-z]",x))>0) x[grep("^G2[A-Za-z]| G2[A-Za-z]",x)]<-gsub("G2[A-Za-z]*?([\\(=])","G2\\1",x[grep("^G2[A-Za-z]| G2[A-Za-z]",x)])

index<-grep("[\\( ]G2[<=>]|^G2[<=>]|[\\( ]G2 \\([0-9]|^G2 \\([1-9]|[\\( ]G2\\([0-9]|^G2\\([1-9]",x)
if(length(index)>0){
G2<-x[index]

# get df if has one
G2df<-rep(NA,length(G2))
if(length(grep("[^a-z]G2\\([1-9]|^G2\\([1-9]",G2))>0){
  G2df[grep("[^a-z]G2\\([1-9]|^G2\\([1-9]",G2)]<-gsub("\\).*","",gsub(".*G2\\(","",G2[grep("[^a-z]G2\\([1-9]|^G2\\([1-9]",G2)]))
}
G2<-gsub("[(][1-9].*?G2(=)","\\1",G2)
G2<-gsub("[(][1-9].*?G2(<)","\\1",G2)
G2<-gsub("[(][1-9].*?G2(>)","\\1",G2)
# remove df in brackets
G2<-gsub("\\([0-9]*?\\)","",G2)
# get sign
G2sign<-gsub("[^<>=].*","",sub("[^<>=]*([=<>])", "\\1",sub(".*G2([\\(=<>])","\\1",G2)))
# get G2 value
G2<-suppressWarnings(as.numeric(gsub("[^0-9\\.-].*","",gsub("[<=>]","",unlist(lapply(strsplit(G2,"G2[<=>]"),"[",2))))))
# add to results
res[index,"G2_op"]<-G2sign
res[index,"G2"]<-G2
res[index,"result"]<-y[index]
## only add df1 if no df1 has been recorded yet
use<-is.na(res[index,"df1"])
res[index,"df1"][use]<-G2df[use]
}


## extracts only R2 no type yet
index<-grep("^[Rr]2[(<= ]|[^A-Za-z][Rr]2[(<= ]|[Rr] 2[(<= ]|[^A-Za-z][Rr] 2[(<= ]",x)
if(length(index)>0){
R2<-x[index]
# unify
R2<-gsub("[Rr] 2","R2",R2)
R2<-gsub("[Rr]2","R2",R2)

types<-c("adj","change|delta|increase","Nagelkerke","pseudo","Cox|Snell","^partial R| partial R","semi[- ]partial R")
# find type of R2
R2type<-paste0(which.term(R2,types,tolower=T,hits_only=T),collapse=", ")

# get sign
R2sign<-gsub("[^<>=].*","",sub("[^<>=]*([=<>])", "\\1",sub(".*R2([\\(=<>])","\\1",R2)))

# remove text till <=> after R2
#R2<-gsub("[<=>]","",gsub(".*?[<=>](.+)", "\\1",gsub(".*R2","",R2)))
R2<-gsub("[<=>]","",gsub(".*?[<=>](.+)", "\\1",unlist(lapply(strsplit(R2,"R2"),"[",2))))
# remove text behind number %num-.[^-0-9\\.%]
R2<-gsub("[^-0-9\\.%].*","",R2)
# add to results
res[index,"R2"]<-R2
res[index,"R2_op"]<-R2sign
res[index,"result"]<-y[index]
#if(length(R2type)==0) R2type<-NA
#res[index,"R2type"]<-R2type
}

## extract U value
# lines with U value
x<-gsub("U-test","U",x)
index<-grep("[^A-Za-z]U[(=]|^U[(=]",x)
if(length(index)>0){
   Uval<-x[index]
   # get sign
Usign<-gsub("[^<>=].*","",sub("[^<>=]*([=<>])", "\\1",sub(".*U([\\(=<>])","\\1",Uval)))

# extract U value
Uval<-suppressWarnings(as.numeric(gsub("[^0-9\\.-].*","",gsub(".*U=|.*U[(][0-9].*?=","", Uval))))
# add U values to res
res[index,c("U")]<-Uval
res[index,c("U_op")]<-Usign
res[index,"result"]<-y[index]
}

## extract p value
# lines with p value
index<-grep("[\\(\\[ ]p[(<>=]|^p[(<>=]",x)
if(length(index)>0){
pval<-x[index]
pval<-gsub("^p","temp p",pval)
pval<-gsub("^( p[<>=\\.0-9]*)[^<>=\\.0-9].*","\\1",unlist(lapply(strsplit2(pval," p[<=>]","before"),"[",2)))
#pval<-unlist(lapply(strsplit2(pval," p[<=>]","before"),"[",2))
pval<-gsub("([0-9])[^0-9\\.].*","\\1",pval)
pval<-gsub(".*p[=]","=",pval)
pval<-gsub(".*p[>]",">",pval)
pval<-gsub(".*p[<]","<",pval)
# extract p sign
psign<-substr(pval,1,3)
psign<-gsub("[^<>=].*","",psign)
pval<-gsub("[<>=]","",pval)
# remove everything behind non number or dot
pval<-suppressWarnings(as.numeric(gsub("[^0-9\\.].*","",pval)))
# add p values to res
res[index,c("p_op")]<-psign
res[index,c("p")]<-pval
res[index,"result"]<-y[index]
}

## extract BayesFactor
# unify
x<-gsub("[bB]ayes [bF]actor","BF",x)
x<-gsub("BF01|BF 01","BF(01)",x)
x<-gsub("BF10|BF 10","BF(10)",x)
index<-grep(" BF[(]|^BF[(]",x)
if(length(index)>0){
BF<-x[index]
# remove till "BF("
BF<-gsub(".*BF[(]|^BF[(]","",BF)
# check type: BF(01) or BF(10)
type<-rep(NA,length(BF))
type[substring(BF,1,2)=="01"]<-"01"
type[substring(BF,1,2)=="10"]<-"10"
# remove bad types
BF<-BF[!is.na(type)]
index<-index[!is.na(type)]
type<-type[!is.na(type)]
# remove ":1" at end and from beginning of ,; or .[^0-9] or +-
BF<-gsub(":1$|[,;].*|[\\.][^0-9].*|\\+\\-.*","",BF)
# clean up
BF<-gsub(" \\.",".",BF)
BF<-gsub("\\^e","e",BF)
BF<-(gsub(" \\*10[ \\^]|\\*10[ \\^]", "e",BF))
BF<-(gsub(" -10[ \\^]", "e-",BF))
# remove first chars till ><=
sign<-gsub("[^<>=].*","",sub("[^<>=]*([=<>])", "\\1",BF))
# remove lines without sign
BF<-BF[nchar(sign)>0]
index<-index[nchar(sign)>0]
type<-type[nchar(sign)>0]
sign<-sign[nchar(sign)>0]

# get BF result
BF<-gsub(".*[>=<](.+)", "\\1",BF)
# clean up after last number or "." or ]
BF<-gsub("[<=].*", "",BF)
BF<-gsub("\\].*| .*", "",BF)
# compute BF with 1/ 
if(length(grep("^1/",BF))>0){
  ind<-grep("^1/",BF)
  BF[ind]<- as.character(round(1/as.numeric(unlist(lapply(strsplit(BF[ind],"[/]"),"[",2))),4))
}

BF<-suppressWarnings(as.numeric(BF))
# remove NAs
index<-index[!is.na(BF)]
sign<-sign[!is.na(BF)]
type<-type[!is.na(BF)]
BF<-BF[!is.na(BF)]

# convert BF01 -> BF10
BF[(type=="01")]<-(1/BF[(type=="01")])
# convert sign
BFsign<-sign
BFsign[(type=="01")&sign=="<"]<-">"
BFsign[(type=="01")&sign==">"]<-"<"

# add BayesFactor values to res
res[index,c("BF10")]<-BF
res[index,"BF10_op"]<-BFsign
res[index,"result"]<-y[index]
}

## extract Bayes factor without 01 or 10
# lines with BF value
index<-grep(" BF[(<>=]|^BF[(<>=]",x)
if(length(index)>0){
   BF<-x[index]
BF<-gsub(".*BF[=]|.*BF\\([0-9]*?\\)[=]","=",BF)
BF<-gsub(".*BF[>]|.*BF\\([0-9]*?\\)[>]",">",BF)
BF<-gsub(".*BF[<]|.*BF\\([0-9]*?\\)[<]","<",BF)
BFsign<-gsub("[^<=>].*","",BF)
BF<-suppressWarnings(as.numeric(gsub("[<=>]","",gsub("[,; ].*","",BF))))
# add BF values to res
res[index,c("BF")]<-BF
res[index,c("BF_op")]<-BFsign
res[index,"result"]<-y[index]
}



## extract OddsRatio value
# unify
x<-gsub("[Oo]dds[ \\-][Rr]atio|[aA]OR|ORa","OR",x)
# correct ORnum -> OR=num
x<-gsub("OR([0-9\\.])","OR=\\1",x)

# lines with OR value
index<-grep(" OR[(<>=]|^OR[(<>=]",x)
if(length(index)>0){
   OR<-x[index]
OR<-gsub(".*OR[=]|.*OR\\([0-9]*?\\)[=]","=",OR)
OR<-gsub(".*OR[>]|.*OR\\([0-9]*?\\)[>]",">",OR)
OR<-gsub(".*OR[<]|.*OR\\([0-9]*?\\)[<]","<",OR)
OR<-suppressWarnings(as.numeric(gsub("[<=>]","",gsub("[,; ].*","",OR))))

# add OR values to res
res[index,c("OR")]<-OR
res[index,"result"]<-y[index]
}

## extract RiskRatio value
# unify
x<-gsub("[Rr]isk[ \\-][Rr]atio|[aA]RR|RRa","RR",x)
# correct ORnum -> OR=num
x<-gsub("RR([0-9\\.])","RR=\\1",x)

# lines with OR value
index<-grep(" RR[(<>=]|^RR[(<>=]",x)
if(length(index)>0){
   RR<-x[index]
RR<-gsub(".*RR[=]|.*RR\\([0-9]*?\\)[=]","=",RR)
RR<-gsub(".*RR[>]|.*RR\\([0-9]*?\\)[>]",">",RR)
RR<-gsub(".*RR[<]|.*RR\\([0-9]*?\\)[<]","<",RR)
RR<-suppressWarnings(as.numeric(gsub("[<=>]","",gsub("[,; ].*","",RR))))

# add RR values to res
res[index,c("RR")]<-RR
res[index,"result"]<-y[index]
}

## extract Z value
# lines with Z value
index<-grep("[\\(\\[ ][zZ][(<>=]|^[zZ][(<>=]",x)
if(length(index)>0){
   Zval<-x[index]
Zval<-gsub(".*[zZ][=]|.*[zZ]\\([0-9]*?\\)[=]","=",Zval)
Zval<-gsub(".*[zZ][>]|.*[zZ]\\([0-9]*?\\)[>]",">",Zval)
Zval<-gsub(".*[zZ][<]|.*[zZ]\\([0-9]*?\\)[<]","<",Zval)
Zsign<-substr(Zval,1,3)
Zsign<-gsub("[^<=>].*","",Zsign)
Zval<-suppressWarnings(as.numeric(gsub("[<=>]","",gsub("[,; ].*","",Zval))))

# add Z values to res
res[index,c("Z")]<-Zval
res[index,c("Z_op")]<-Zsign
res[index,"result"]<-y[index]
}

## extract Q and df
# unify q -> Q
x<-gsub("q","Q",x)
# remove [^a-zA-Z] in front of Q
x<-gsub("[^a-zA-Z]Q"," Q",x)

# remove text from Q_text(12)=
if(length(grep("^Q[A-Za-z]|[\\(\\[ ]Q[A-Za-z]",x))>0) x[grep("^Q[A-Za-z]| Q[A-Za-z]",x)]<-gsub("Q[A-Za-z]*?([\\(=])","Q\\1",x[grep("^Q[A-Za-z]|[\\(\\[ ]Q[A-Za-z]",x)])

index<-grep("[\\(\\[ ]Q[<=>(]|^Q[<=>(]",x)
if(length(index)>0){
   Q<-x[index]
Qdf<-rep(NA,length(Q))
# find df by coding df=
ind<-grep("df[=]",Q)
Qdf[ind]<-gsub("[^0-9\\.].*","",gsub(".*df[=]","",Q[ind]))
# find df by coding Q(df)
ind<-grep("Q[(]",Q)
Qdf[ind]<-gsub("[^0-9\\.].*","",gsub(".*Q[(]","",Q[ind]))
# find df by coding Q(N=[0-9]*, df)
ind<-grep("Q[(][Nn][=]",Q)
if(length(ind)>0) Qdf[ind]<-gsub("[^0-9\\.].*|\\).*","",gsub(".*Q[(][Nn]=[0-9]*[, ;]*","",Q[ind]))
# find df by coding Q(df, N=[0-9])
ind<-grep("Q[(][0-9]*?[,;] [Nn][=]",Q)
if(length(ind)>0) Qdf[ind]<-gsub("[^0-9\\.].*|\\).*","",gsub(".*Q[(]","",Q[ind]))

# remove df in brackets
Q<-gsub("\\([0-9]*?\\)","",Q)
# get sign
Qsign<-gsub("[^<>=].*","",sub("[^<>=]*([=<>])", "\\1",sub(".*Q([\\(=<>])","\\1",Q)))
# get Q value
Q<-suppressWarnings(as.numeric(gsub("[^0-9\\.-].*","",gsub("[<=>]","",unlist(lapply(strsplit(Q,"Q[<=>]"),"[",2))))))
# add to results
res[index,"Q_op"]<-Qsign
res[index,"result"]<-y[index]
# add to results if no df1 has been captured yet
res[index,c("Q")]<-cbind(Q)
i2<-is.na(res[index,c("df1")])
res[index,c("df1")][i2]<-cbind(Qdf)[i2]
}


############
## clean up
##########
# set bad captures in df1 and df2 to NA   
if(length(grep("[^0-9\\.]",res[,"df1"]))>0) res[grep("[^0-9\\.]",res[,"df1"]),"df1"]<-NA
if(length(grep("[^0-9\\.]",res[,"df2"]))>0) res[grep("[^0-9\\.]",res[,"df2"]),"df2"]<-NA

# remove lines with only NA and set as matrix if is vector
res<-res[rowSums(is.na(res))!=dim(res)[2],]
if(length(res)>0){ # escape if nothing left
if(is.vector(res)){
   res<-matrix(res,ncol=length(res))
   colnames(res)<-cnames
   }



#######################
## recalculate p-value 
#####################
if(dim(res)[1]>0){
if(recalculate.p==TRUE){
# for undirected tests
suppressWarnings({
  recalculatedPH<-round(1-stats::pchisq(as.numeric(res[,"H"]),as.numeric(res[,"df1"])),5)
  recalculatedPG2<-round(1-stats::pchisq(as.numeric(res[,"G2"]),as.numeric(res[,"df1"])),5)
  recalculatedPQ<-round(1-stats::pchisq(as.numeric(res[,"Q"]),as.numeric(res[,"df1"])),5)
  recalculatedPr<-round(2*(1-stats::pt((abs(as.numeric(res[,"r"]))*sqrt(as.numeric(res[,"df2"])))/sqrt(1-as.numeric(res[,"r"])^2),as.numeric(res[,"df2"]))),5)
  recalculatedPchi<-round(1-stats::pchisq(as.numeric(res[,"Chi2"]),as.numeric(res[,"df1"])),5)
  recalculatedPt<-round(2*(1-stats::pt(abs(as.numeric(res[,"t"])),as.numeric(res[,"df2"]))),5)
  recalculatedPZ<-round(2*(1-stats::pnorm(abs(as.numeric(res[,"Z"])))),5)
  recalculatedPZest<-round(2*(1-stats::pnorm(abs(as.numeric(res[,"Zest"])))),5)
  recalculatedPF<-round(1-stats::pf(as.numeric(res[,"F"]),as.numeric(res[,"df1"]),as.numeric(res[,"df2"])),5)
})


# for directed tests alternative="directed|both" Z-,t-,r-values 
suppressWarnings({
  recalculatedPrg<-round((1-stats::pt((abs(as.numeric(res[,"r"]))*sqrt(as.numeric(res[,"df2"])))/sqrt(1-as.numeric(res[,"r"])^2),as.numeric(res[,"df2"]))),5)
  recalculatedPtg<-round((1-stats::pt(abs(as.numeric(res[,"t"])),as.numeric(res[,"df2"]))),5)
  recalculatedPZg<-round((1-stats::pnorm(abs(as.numeric(res[,"Z"])))),5)
  recalculatedPZgest<-round((1-stats::pnorm(abs(as.numeric(res[,"Zest"])))),5)
})

# for directed tests alternative="directed|both" Z-,t-,r-values 
suppressWarnings({
  recalculatedPrl<-round((stats::pt((abs(as.numeric(res[,"r"]))*sqrt(as.numeric(res[,"df2"])))/sqrt(1-as.numeric(res[,"r"])^2),as.numeric(res[,"df2"]))),5)
  recalculatedPtl<-round((stats::pt(abs(as.numeric(res[,"t"])),as.numeric(res[,"df2"]))),5)
  recalculatedPZl<-round((stats::pnorm(abs(as.numeric(res[,"Z"])))),5)
  recalculatedPZlest<-round((stats::pnorm(abs(as.numeric(res[,"Zest"])))),5)
})

# overwrite empty recalculated PZ with PZest 
recalculatedPZ[is.na(recalculatedPZ)]<-recalculatedPZest[is.na(recalculatedPZ)]
recalculatedPZl[is.na(recalculatedPZl)]<-recalculatedPZlest[is.na(recalculatedPZl)]
recalculatedPZg[is.na(recalculatedPZg)]<-recalculatedPZgest[is.na(recalculatedPZg)]




# take the most conservative (highest) p value if 2 or more were calculated
d<-data.frame(recalculatedPt,recalculatedPF,recalculatedPr,recalculatedPchi,recalculatedPZ,recalculatedPH,recalculatedPG2,recalculatedPQ)
# has multiple recomputable p-values
if(sum(rowSums(!is.na(d))>1)>0) warn.multi.p<-TRUE

# get p value by rank t, F, r, chi, Z, H, G2, Q
recalculatedP<-NULL
for(i in 1:dim(d)[1]) recalculatedP[i]<-d[i,][!is.na(d[i,])][1]
# add to res
  if(!is.null(dim(res))) res[,"recalculatedP"]<-recalculatedP
  if(is.null(dim(res))) res["recalculatedP"]<-recalculatedP

# take the most conservative directed p value if 2 or more were calculated
d<-data.frame(recalculatedPtl,recalculatedPrl,recalculatedPZl)
# get p value by rank t, r, Z
recalcPless<-NULL
for(i in 1:dim(d)[1]) recalcPless[i]<-d[i,][!is.na(d[i,])][1]
# add to res
  if(!is.null(dim(res))) res[,"p_H0_less"]<-recalcPless
  if(is.null(dim(res))) res["p_H0_less"]<-recalcPless
  
d<-data.frame(recalculatedPrg,recalculatedPtg,recalculatedPZg)
# get p value by rank r, t, Z
recalcPgreater<-NULL
for(i in 1:dim(d)[1]) recalcPgreater[i]<-d[i,][!is.na(d[i,])][1]
# add to res
  if(!is.null(dim(res))) res[,"p_H0_greater"]<-recalcPgreater
  if(is.null(dim(res))) res["p_H0_greater"]<-recalcPgreater
  
}
  
  # make matrix if is none
if(!is.matrix(res)){
res<-matrix(res,1)
colnames(res)<-cnames
}

# convert no character capture to NA
for(i in 1:dim(res)[2]) res[res[,i]=="",i]<-NA

# only select stats with recomputable p value
if(stats.mode=="computable") res<-res[!is.na(res[,"recalculatedP"]),]
if(!is.matrix(res)){
res<-matrix(res,1)
colnames(res)<-cnames
}

# only select stats with p value and recomputable p value
if(stats.mode=="checkable") res<-res[!is.na(res[,"recalculatedP"])&!is.na(res[,"p"]),]
if(!is.matrix(res)){
res<-matrix(res,1)
colnames(res)<-cnames
}

} # end recalculate p-values

if(!is.matrix(res)){
res<-matrix(res,1)
colnames(res)<-cnames
}

# only select stats with no recomputable p value
if(stats.mode=="uncomputable") res<-res[is.na(res[,"recalculatedP"]),]
if(!is.matrix(res)){
   res<-matrix(res,1)
   colnames(res)<-cnames
}

# remove sided test p-values if not requested by alternative and reduce cnames
if(is.element(alternative,c("undirected"))){
   res<-res[,-which(is.element(cnames,c("p_H0_greater","p_H0_less")))]
   cnames<-cnames[-which(is.element(cnames,c("p_H0_greater","p_H0_less")))]
   if(!is.matrix(res)){
   res<-matrix(res,1)
   colnames(res)<-cnames
   }}
   
## Warning massages:
# need warning massage for r > 1|r < -1
if(sum(as.numeric(res[,"r"])>1|as.numeric(res[,"r"])<(-1),na.rm=T)>0) warn.r<-TRUE
# need warning massage for R2 > 1|R2 < -1
if(sum(as.numeric(res[,"R2"])>1|as.numeric(res[,"R2"])<(0),na.rm=T)>0) warn.R2<-TRUE   
# set warn.T2t to FALSE if has no t value
if(sum(is.na(res[,"t"]))==length(res[,"t"])) warn.T2t<-FALSE
# set warn.R2r to FALSE if has no r value
if(sum(is.na(res[,"r"]))==length(res[,"r"])) warn.R2r<-FALSE
# need p warning
if(sum(as.numeric(res[,"p"])>1|as.numeric(res[,"p"])<0,na.rm=T)>0) warn.p=TRUE
# need warning massage for |d| > 1
if(sum(as.numeric(abs(as.numeric(res[,"d"])))>1,na.rm=T)>0) warn.d<-TRUE   
# need warning massage for eta^2 > .3
if(sum(as.numeric(abs(as.numeric(res[,"eta2"])))>.3,na.rm=T)>0) warn.eta<-TRUE   

## remove columns with only NA if something is left
if(!is.null(dim(res))){
if(rm.na.col==TRUE&length(dim(res))==2&dim(res)[1]>0){
   n<-colnames(res)
   n<-n[colSums(is.na(res))!=dim(res)[1]]
   res<-res[,colSums(is.na(res))!=dim(res)[1]]
   # if only statistical value/column left rename vector
   if(is.null(dim(res))&length(n)==1){
     res<-matrix(res,ncol=1)
     colnames(res)<-n}
   if(!is.matrix(res)){
   res<-matrix(res,1)
   colnames(res)<-n
   }
   }
   

# set to character(0) if result is empty 
if(rm.na.col==TRUE&is.null(res)) res<-character(0)

if(sum(dim(res)==c(0,length(cnames)))==2) res<-character(0)

# convert to data.frame and set column names
if(is.vector(res)){ 
   res<-data.frame(rbind(res),stringsAsFactors=FALSE,row.names=NULL)
   n<-names(res)
   }
if(is.matrix(res)){
   res<-data.frame(res,stringsAsFactors=FALSE)
   n<-colnames(res)
   }
names(res)<-n

} # end remove columns

# set empty result to character(0)
if(is.null(res)) res<-character(0)

if(!is.null(ncol(res))) if(ncol(res)==0) res<-character(0)
if(!is.null(ncol(res))) if(nrow(res)==0) res<-character(0)

}else res<-character(0)
}else res<-character(0) # end escape

## Warning massages
   report<-NULL
if(T2t==TRUE&warn.T2t==TRUE) report<-c(report,"- Capital T was converted to small t. Maybe T is not t-distributed.\n")
if(R2r==TRUE&warn.R2r==TRUE) report<-c(report,"- Capital R was converted to small r. Maybe R is not referring to a correlation.\n")
if(warn.r==TRUE) report<-c(report,"- One or more detected r-values are out of range for possible correlations [-1, 1].\n")
if(warn.R2==TRUE) report<-c(report,"- One or more detected R^2-values are out of range for possible coefficients of determination [0, 1].\n")
if(warn.p==TRUE) report<-c(report,"- One or more detected p-values are out of range for possible p-values [0, 1].\n")
if(warn.d==TRUE) report<-c(report,"- A rather big effect was detected. One or more |d|-values > 1.\n")
if(warn.eta==TRUE) report<-c(report,"- A rather big effect was detected. One or more eta^2-values > .3.\n")
if(warn.multi.p==TRUE) report<-c(report,"- There are one or more results with several recomputable test statistics. Please split the result manually and proceed checking.\n")
if(!is.null(report)) warning(report)
return(res)

}



