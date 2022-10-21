#' get.author
#'
#' Extracts author tag information from NISO-JATS coded XML file or text.
#' @param x a NISO-JATS coded XML file or text.
#' @param paste if paste!="" author list is collapsed to one cell with seperator specified (e.g. paste=";").
#' @param short.names Logical. If TRUE fully available first names will be reduced to single letter abbreviation.
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character vector with the extracted author name/s.
#' @export

get.author<-function(x,paste="",short.names=FALSE,letter.convert=FALSE){
  # run prechecks or readLines(x) if x is file
  x<-preCheck(x)
  
if(length(grep("^contrib",x))!=length(x)) x<-get.contrib(x)
# collapse x for cases with many spaces (cermine export)
if(length(grep("^contrib",x))==0) x<-paste(x,collapse=" ")
# remove double \\
x<-gsub("\\\\","",x)

 if(length(grep("contrib-type=\"author\"",x))>0){
  # author vector raw
  temp<-grep("contrib-type=\"author\"",x,value=TRUE)
  #temp<-unlist(strsplit(x,"contrib-type=\"author\""))[-1]
  #temp<-gsub("</contrib>.*","",temp)
 
  # cleanup authors names
  temp<-gsub(".*<surname>","<surname>",gsub("</name>.*","",temp))
  #  temp<-grep("surname",temp,v=T)
  if(letter.convert==TRUE) temp<-letter.convert(temp)

  # extract surname/type and name/ID
  surname<-gsub(".*<surname>","",gsub("</surname.*","",temp))
  surname<-gsub("\\\".*","",gsub(".*id-type=\\\"","id-type: ",surname))
  names<-gsub(".*<given-names>","",gsub("</given-names.*","",temp))
  names<-gsub(".*\\\">","",names)
  
  # shorten name
  if(short.names==T){
    names<-gsub("[[:lower:]_]","",sub("[[:lower:]_]",".",names)    )
    names[grep("\\.$",names,invert=TRUE)]<-paste(grep("\\.$",names,value=TRUE,invert=TRUE),".",sep="")
   }
  # for names containing ^contrib extract <collab> 
  if(length(grep("^contrib-",surname))>0){
  index<-grep("^contrib-",surname)
  surname[index]<-gsub("</coll.*","",gsub(".*<collab>","",temp[index]))
  names[index]<-""
  } 
  
  # clean up names containing <contrib contrib-type=
  if(length(grep("<contrib contrib-type=",surname))>0){
  surname<-gsub("^ *|(?<= ) | *$", "", surname, perl = TRUE)
  index<-grep("<contrib contrib-type=",surname)
  surname[index]<-gsub("<contrib contrib-type=","",surname[index])
  names[index]<-""
  }
 # convert 2nd and further letters to lower case if only capital letters in name
  if(sum(nchar(names)>1)==length(names)&length(grep("^.[A-Z]",names))>0){
    temp<-NULL;res<-NULL
    for(i in 1:length(names)){
      temp<-unlist(strsplit2(names[i]," |-|\\.","after"))
      temp<-paste(paste(substr(temp,1,1),tolower(substr(temp,2,nchar(temp))),sep=""),collapse=" ")
      res[i]<-temp
     }
   # clean up
    names<-gsub("- ","-",gsub("  |   "," ",res))
    }
  # convert 2nd and further letters to lower case if only capital letters in surname
  if(sum(nchar(surname)>1)==length(surname)&length(grep("^.[A-Z]",surname))>0){
  temp<-NULL;res<-NULL
  for(i in 1:length(surname)){
    temp<-unlist(strsplit2(surname[i]," |-|\\.","after"))
    temp<-paste(paste(substr(temp,1,1),tolower(substr(temp,2,nchar(temp))),sep=""),collapse=" ")
    res[i]<-temp
    }
  # clean up
  surname<-gsub("- ","-",gsub("  |   "," ",res))
  }

  # paste names and remove ", " at end
  authors<-gsub(", $","",paste(surname,names,sep=", "))
  # replace authors that are <collab> tagged with content of <collab>
  if(length(grep("<collab>",authors))>0) authors<-gsub("<.*?.*>","",gsub(".*<collab>","",gsub("</collab>.*","",authors)))
  # clean up
  if(sum(authors=="NA, NA")>0) authors<-NA
  authors<-gsub("<.*?.*>","",authors)
  if(length(grep("[A-Za-z]",authors))==0) authors<-NA
  # collapse output to one cell if paste !=""
  if(paste!=""&paste!=F) if(!is.na(authors[1])) authors<-paste(authors,collapse=paste)

# clean up (cermine exports)  
  authors<-gsub("^ *|(?<= ) | *$", "", authors, perl = TRUE)  # remove doubled spaces
  authors<-gsub(".*> ", "", authors)  # remove till name
  authors<-gsub(" $", "", authors)  # remove space at end
  authors<-gsub(" [^a-zA-Z\\.]*$","",authors) # remove numbers at end
  authors<-unique(gsub("\\\"","",authors))
  
  # remove email address
  authors<-gsub("[^ ]*@[^ ]*","",authors)
  authors<-gsub(" $|, $","",authors)
  
  # correct some errors
  authors<-gsub("(\\.org[^ ]*) ([^ ]*) ?","\\1\\2",authors)
  authors<-gsub("/$|;$","",authors)
  
  
}else authors<-NA

return(authors)
}

