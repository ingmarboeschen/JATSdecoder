#' get.editor
#'
#' Extracts editor tag from NISO-JATS coded XML file or text as vector of editors.
#' @param x a NISO-JATS coded XML file or text.
#' @param role Logical. If TRUE adds role to editor name, if available.
#' @param short.names Logical. If TRUE reduces fully available first names to one letter abbreviation.
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode.
#' @export

get.editor<-function(x,role=FALSE,short.names=FALSE,letter.convert=FALSE){
cleanup<-TRUE
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")
# extract contrib tag
if(length(grep("^contrib",x))!=length(x)) x<-get.contrib(x)
# extract editor
if(length(grep("\"editor[^a-z]",x))>0){
  temp<-grep("\"editor[^a-z]",unlist(x),value=TRUE)
  if(cleanup==T){
   # extract surname and name
   surname<-gsub(".*<surname>","",gsub("</surname.*","",temp))
   surname<-gsub("\\\".*","",gsub(".*id-type=\\\"","id-type: ",surname))
   names<-gsub(".*<given-names>","",gsub("</given-names.*","",temp))
   names<-gsub(".*\\\">","",names)
  # for names containing ^contrib extract <collab> 
  if(length(grep("^contrib-",surname))>0){
  index<-grep("^contrib-",surname)
  surname[index]<-gsub("</coll.*","",gsub(".*<collab>","",temp[index]))
  names[index]<-""
  } 
   if(length(grep("><",names))>0) names[grep("><",names)]=""
   # shorten name
   if(short.names==T){
    names<-gsub("[[:lower:]_]","",sub("[[:lower:]_]",".",names))
    names[grep("\\.$",names,invert=TRUE)]<-paste(grep("\\.$",names,value=TRUE,invert=TRUE),".",sep="")
    }
    
   # get role
   if(length(grep("<role>",temp))>0){
    r<-gsub("</role>.*","",gsub(".*<role>","",temp))
    }else r<-NA
   # paste names and role
   editor<-paste(surname,names,sep=", ")
   # remove <italic>
   editor<-gsub("</italic>",",",gsub("<italic>","",editor))
   # remove ", " at end
   editor<-gsub(", $|,$","",editor)
      if(role==T) editor<-paste(editor,r,sep="; ")
   # set empty name tag to NA
   if(length(grep("<surname>NA\\.</surname>",editor))>0)  editor<-NA
  }else editor<-temp
  
if(letter.convert==TRUE) editor<-letter.convert(editor)
editor<-gsub("[Ee]dited by[\\: ]","",editor)
# correct
editor<-editor[editor!="NA."]
editor<-editor[editor!="id-type: doi"]
editor<-editor[editor!="id-type: pmid"]
if(length(editor)==0) editor<-NA

}else editor<-NA

return(unique(editor))
}

