#' get.n.studies
#'
#' Extract n studies/experiments from section titles or abstract text
#' @param x section titles or abstract text to process
#' @param tolower Logical. If TRUE lowerises text and search patterns for processing
#' @export

get.n.studies<-function(x,tolower=TRUE){
  s<-unlist(x)
  # Unify STUDY/EXPERIMENT
  s<-gsub("EXPERIMENT","Experiment",gsub("STUDY","Study",s))

  # define search patterns
  pattern<-"Study [1-9]|Studies [1-9]|Experiment [1-9]|Experiments [1-9]|Exp\\. [1-9]"
  #  if(tolower==TRUE) { pattern<-tolower(pattern);s<-tolower(s)}
  # select lines with patterns
  s<-get.sentence.with.pattern(s,pattern,tolower=tolower)
  # unify
  s<-gsub(" and | to | \\& | - ","-",s)

  # remove brackets for case of (Experiment [0-9])
  s<-gsub("\\((Experiment [1-9])\\)|\\((Study [1-9])\\)","\\1",s)
  s<-gsub("\\((Experiments [1-9]\\-[2-9])\\)|\\((Studies [1-9]\\-[2-9])\\)","\\1",s)
  # remove text between ()
  s<-gsub("\\(.*?.*\\)","",s)
  # remove lines with " et al" or 4 numbers with bracket at end (year of publication)
  s<-grep(" et al", s,value=TRUE,invert=TRUE)
#  s<-grep("[1-2][0-9][0-9][0-9]\\)", s,value=TRUE,invert=TRUE)
  # split lines
  if(is.character(s)) s<-s<-unlist(strsplit(s,";;|[,;]"))
  if(is.character(s)) s<-unlist(strsplit(s,"\\:"))

  s<-get.sentence.with.pattern(s,pattern,tolower=tolower)

  # remove lines with [0-9] item or [0-9]% and nchar>=250
  s<-(grep("[0-9][ \\-][Ii]tem",s,value=TRUE,invert=TRUE))
  # remove lines with [0-9]%
  s<-(grep("[0-9]\\%|[0-9] \\%",s,value=TRUE,invert=TRUE))
  s<-s[nchar(s)<250]

  # clean up 
  s<-gsub(".*[Ss]tudy |.*[Ss]tudies |.*[Ee]xperiment |.*[Ee]xperiments |.*[Ee]xp\\. ","",s)
  s<-substr(s,1,6) ##NEW
  s<-gsub("-[a-z].*|- [a-z].*","",s)  
  s<-gsub(".*[1-9]-","",s)
  s<-gsub("[^0-9].*","",s)
  s<-as.numeric(s)
  # set no captured study to 1
  if(length(s)==0|length(s)==sum(is.na(s))) s<-1
  s<-max(s,na.rm=T)
  # bad captures to 1
  if(s>100) s<-1
  return(s)
}
