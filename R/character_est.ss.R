#' est.ss
#'
#' Function to estimate studies sample size by maximizing different conservative estimates.
#' Performs 4 different heuristic driven search tasks for reoprted sample size in abstract, text, stats and standardStats.
#' 
#' Sample size extraction from abstract:\cr
#' - Extracts N= from abstract if possible
#' 
#' Sample size extraction from statistical results:\cr
#' - Extracts "N=" in statistical result lines from get.stats() that contain p-value
#' 
#' Sample size extraction with result of standardStats(get.stats()):\cr
#' - Extracts df1 and df2 if possible and neither containing a ".", than calculates quantile of (df1+1)+(df2+2) (at least 2 group comparison assumed)
#'
#' @param abstract abstracts text
#' @param text main text to process (usually method section)
#' @param quantileDF quantile of (df1+1)+(df2+2) to extract
#' @param max.only Logical. If TRUE only the final estimate will be returned, if FALSE all sub estimates are returned as well
#' @param max.parts Logical. If FALSE outputs all captured sample sizes in sub inputs
#' @importFrom utils head
#' @importFrom stats quantile
#' @export
#' @examples
#'  a<-"One hundred twelve students participated in our study."
#'  x<-"Our sample consists of three hundred twenty five undergraduate students.
#'      For one sub group the F-test indicates significant differences in means F(2,102)=3.21, p<.05."
#'  est.ss(abstract=a,text=x)

###############################################################
## Function to combine results and find max of extracted N's
##########################################################
est.ss<-function(abstract=NULL,text=NULL,quantileDF=.75,max.only=FALSE,max.parts=TRUE){
  # estimate from text different sources
#  suppressWarnings({if(length(abstract)>0) a<-est.ss.abstract(abstract,max.only=max.parts) else a<-NA})
  suppressWarnings({if(length(abstract)>0) a<-est.ss.n(abstract) else a<-NA})
  # get SS from first 10 lines max of text input (POS-tagging leads to overload in JAVA heap space)
  suppressWarnings({if(length(text)>0){
    # convert to sentences
    if(length(text)==1) text<-text2sentences(text)
    # remove lines with bootstrap|iteration
    text<-grep("[Bb]ootstrap|[Ii]teration|[Rr]epetition",text,value=TRUE,invert=TRUE)
#    t<-est.ss.text(text,max.only=max.parts)
    stats<-unlist(get.stats(text,output="stats"))
    standardStats<-standardStats(stats)
  } else{t<-NA;stats<-NULL;standardStats<-NULL}
  })
  suppressWarnings({if(length(stats)>0) s<-est.ss.stats(stats,max.only=max.parts) else s<-NA})
  suppressWarnings({if(length(standardStats)>0) ss<-est.ss.standardStats(standardStats,quantileDF=quantileDF,quantile.only=max.parts) else ss<-NA})
  # combine # ,SStext=t[length(t)]
  res<-c(SSabstract=a[length(a)],SSstats=s[length(s)],SSstandardStats=ss[length(ss)])
  # find max of estimates
  max<-max(suppressWarnings(as.numeric(res)),na.rm=T,warnings=F)
  if(max==0) max<-NA
  # output of estimate/s
  if(max.only==T) return(unname(c(estimated.sample.size=max)))
  if(max.only==F) return(c(SSabstract=a,SSstats=s,SSstandardStats=ss,estimated.sample.size=max))
}#,SStext=t

#######################################################
## Function to etimate sum(N) from first line with N=
# used inside est.ss.abstract and est.ss.stats
#####################################################
est.ss.n<-function(x){
  x<-unlist(x)
  # unify "="
  n<-gsub(" [=] |[=] | [=]","=",x)
  # reduce to lines with n=
  n<-get.sentence.with.pattern(n,"[^a-z]n=[1-9]")
  # correct 1,000 to 1000
  for(i in 1:2) n<-gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",n)
  # remove NA
  n<-n[!is.na(n)]
  n<-(n[length(n)>0])
  # get first sentence
  n<-n[1]
  # split before n=
  n<-unlist(strsplit2(n,"[Nn]=[0-9]*","before"))
  # remove first line
  n<-n[-1]
  # remove first 2 characters
  n<-gsub("^..","",n)
  # remove from first non number
  n<-gsub("[^0-9].*","",n)
  # sum up Ns
  n<-sum(as.numeric(n,warn=F),na.rm=T,warn=F)
  if(length(n)==0|n==0) n<-NA
  return(c(Nsum=n))
}


######################################################
## Function to etimate N from N= in lines with p<=>)
##################################################
est.ss.stats<-function(stats,max.only=F){
  # find max of N= in stats
  if(length(stats)>0){
    # select lines with p value
    Nstat<-grep("p[<=>]",stats,value=TRUE)
    # and N=
    Nstat<-grep("^[nN]=|[^A-Za-z][nN]=",Nstat,value=TRUE)
    # go on if something left
    if(length(Nstat)>0){
      # remove till N=
      Nstat<-gsub("^[nN]=|.*[^A-Za-z][nN]=","",Nstat)
      # remove from first non number
      Nstat<-gsub("[^0-9].*","",Nstat)
      # set to numeric
      Nstat<-as.numeric(unlist(Nstat))
      # only select lines with content
      Nstat<-Nstat[nchar(Nstat)>0]
      if(length(Nstat)==0) Nstat<-NA
      # get max of Ns
      NstatMax<-max(Nstat,na.rm=T,warn=F)
      # set output for bad captures
      if(NstatMax==-Inf|NstatMax==0) NstatMax<-NA
    }else{
      Nstat<-NA
      NstatMax<-NA
    }
  }else{
    Nstat<-NA
    NstatMax<-NA
  }
  # prepare result as list with detected N's and/or max of detected N's
  if(max.only==F) resStats<-list(Nstat=Nstat,statMaxN=NstatMax)
  if(max.only==T) resStats<-c(statMaxN=NstatMax)
  return(resStats)
  
}


##########################################################
## Function to etimate N from stats max((df1-1)+(df2+2))
#####################################################
est.ss.standardStats<-function(standardStats,quantileDF=.75,quantile.only=F){
  if(length(standardStats)>0){
    matrix<-standardStats
    # reset matrix names
    if(is.null(dim(matrix))){
      n<-names(matrix)
      matrix<-matrix(matrix,ncol=length(matrix))
      colnames(matrix)<-n
    }
    # remove columns that only have NAs   
    matrix<-matrix[,!colSums(is.na(matrix))==dim(matrix)[1]]
    # reset matrix names
    if(is.null(dim(matrix))){
      n<-names(matrix)
      matrix<-matrix(matrix,ncol=length(matrix))
      colnames(matrix)<-n
    }
    
    # if line has t-value round up df2
    if(length(grep("^t$",colnames(matrix)))==1&length(grep("df2",colnames(matrix)))==1&nrow(matrix)>0) matrix[is.numeric(matrix[,"t"]),"df2"]<-ceiling(matrix[is.numeric(matrix[,"t"]),"df2"])
    
    # remove corrected df's: set df1 and df2 to NA if one of both contains a "." (correction)
    if(length(grep("df1|df2",colnames(matrix)))==2){ 
      matrix[grep("\\.",paste(factor(matrix[,"df1"]):factor(matrix[,"df2"]))),c("df1","df2")]<-NA
    }
    
    # if only df1 contains "."
    if(length(grep("df1",colnames(matrix)))==1) matrix[grep("\\.",paste(matrix[,"df1"])),"df1"]<-NA
    # if only contains df2
    if(length(grep("df2",colnames(matrix)))==1) matrix[grep("\\.",paste(matrix[,"df2"])),"df2"]<-NA
    # remove lines with Chi2
    if(length(grep("Chi2",colnames(matrix)))==1) matrix<-matrix[is.na(matrix[,"Chi2"]),]
    
    
    # get df1 values and subtract 1
    if(sum(colnames(matrix)=="df1")==1){
      # convert to numeric -1
      df1<-as.numeric(matrix[,"df1"])-1
    }else df1<-rep(NA,ifelse(is.vector(matrix),1,dim(matrix)[1]))
    # get df2 values and add 2
    if(sum(colnames(matrix)=="df2")==1){
      # convert to numeric+2
      df2<-as.numeric(matrix[,"df2"])+2
      # if df1 is NA set df2 to NA
      df1[is.na(df2)]<-NA
    }else df2<-rep(NA,ifelse(is.vector(matrix),1,dim(matrix)[1]))
    # calculate sum of (df1-1)+(df2+2)
    sumdf<-rowSums(cbind(df1,df2),na.rm=T)
    sumdf<-sumdf[which(sumdf>0)]
    ## To do: remove overly high captures (e.g. Box Sperizity test, in repeated measure design) instead of quantile
    
    # calculate quantile
    quantDF<-stats::quantile(sumdf,quantileDF,na.rm=T,type=7)
    if(length(grep("[^0-9\\.]",quantDF))>0) quantDF<-NA
  }
  else{
    sumdf<-NA;quantDF<-NA
  }
  # set Zero result to NA if is not NA already
  if(!is.na(quantDF)) if(quantDF==0) quantDF<-NA
  
  if(quantile.only==F) return(c(sumdf,quantileDF=quantDF))
  if(quantile.only==T) return(c(quantileDF=quantDF))
  
}
