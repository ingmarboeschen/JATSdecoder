#' text2sentences
#'
#' Convert floating text to a vector with sentences via fine tuned regular expressions or NLP sentence tokenization
#' @param x text to process
#' @export
#' @examples
#' x<-"Some text with result (t(18)=1.2, p<.05). This shows how text2sentences works."
#' text2sentences(x)

# Depends on strsplit2, NLP & openNLP
text2sentences<-function(x){
x[is.na(x)]<-""
t<-x
  # unify
  temp<-gsub("  "," ",gsub("  "," ",t))
  temp<-gsub(" \\. ","\\. ",gsub(" \\, ","\\, ",temp))
  # break lines after "[a-z\\)]\\. [A-Z|0-9|<]"
  temp<-unlist(strsplit2(temp,";;|[A-Za-z\\)0-9\\%\\.\\'\\>]\\. [A-Z0-9\\<]|</fig> [A-Z0-9<]|\\]\\. [A-Z]|['`]\\. [A-Z]|[^0-9]\\. [(]|\\.\\) [A-Z]","after",T))
  # only clean up if line splitted to sentences
if(length(temp)>1){
  # extract last character and paste to front of line
  temp[-1]<-paste(substr(temp,nchar(temp),nchar(temp))[-length(temp)],temp[-1],sep="")
  # remove last 2 characters at end of lines
  temp[-length(temp)]<-substr(temp[-length(temp)],1,nchar(temp[-length(temp)])-2)
  # correction for "vs." and "e.g.", "cf." and "Exp." at end of line
  while(length(grep("vs\\.$|e\\.g\\.$|cf\\.$|Exp\\.$|Fig\\.$|et al\\.$",temp))>0){
    ind<-grep("vs\\.$|e\\.g\\.$|cf\\.$|Exp\\.$|Fig\\.$|et al\\.$",temp)[1]
    temp[ind]<-paste(temp[ind],temp[ind+1])
    temp<-temp[-(ind+1)]
    }
    
# correct dot in first position and move to row above
movedot<-function(x){
  # lines ending without dot
  e<-grep("[^\\.]$",x)
  # lines starting with dot
  s<-grep("^\\.",x)
# lines that have movable dot
  i<-is.element(e+1,s)
# move dot
  x[e[i]]<-paste0(x[e[i]],".")
  x[e[i]+1]<-gsub("^\\.","",x[e[i]+1])
  return(x)
}    

temp<-movedot(temp)    
    
# correction: paste too short lines (<=10 chars) to row in front
  while(sum(nchar(temp)<=10)>0){    
   ind<-(1:length(temp))[nchar(temp)<=20][1]
    temp[ind]<-paste(temp[ind],temp[ind+1])
    temp<-temp[-(ind+1)]
   }
} 
  sentences<-temp
  return(sentences)
}

