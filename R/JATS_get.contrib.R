#' get.contrib
#'
#' Extract contrib tag/s from NISO-JATS coded XML file or text as vector of contributers
#' @param x a NISO-JATS coded XML file or text
#' @param remove.html Logical. If TRUE removes all html tags
#' @param letter.convert Logical. If TRUE converts hex and html coded characters to unicode
#' @export

get.contrib<-function(x,remove.html=FALSE,letter.convert=FALSE){
if(length(grep("<contrib",x))>0){
  x<-paste(x,collapse=" ")
# split at <contrib and remove after </contrib
  contrib<-gsub("</contrib.*","",unlist(strsplit(x,"<contrib"))[-1])
# remove spaces at front and end
  contrib<-gsub("^ | $","",contrib)
# paste "^-id" lines to line in front and remove it
  if(length(grep("^-id",contrib))>0){
    i<-grep("^-id",contrib)  
    contrib[i-1]<-paste(contrib[i-1],contrib[i])
    contrib<-contrib[-i]
    }
# select type specified lines
  contrib<-grep("-type",contrib,value=TRUE)
# remove <tags>
  if(remove.html==T) contrib<-gsub("^ | $","",gsub(".*>","",gsub(" ,",",",gsub("  "," ",gsub("<.*?.*>"," ",contrib)))))
# convert hex
  if(letter.convert==T) contrib<-letter.convert(contrib)

  }else contrib<-NA

  contrib<-unique(contrib)
  return(contrib)  
}

#x<-readLines(file.name[2900])
#get.contrib(x,F,T)

