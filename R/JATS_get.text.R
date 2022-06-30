#' get.text
#'
#' Extracts main textual content from NISO-JATS coded XML file or text as sectioned text.
#' @param x a NISO-JATS coded XML file or text.
#' @param sectionsplit search patterns for section split (forced to lower case), e.g. c("intro", "method", "result", "discus").
#' @param grepsection search pattern to reduce text to specific section namings only.
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode.
#' @param greek2text Logical. If TRUE some greek letters and special characters will be unified to textual representation (important to extract stats).
#' @param sentences Logical. IF TRUE text is returned as sectioned list with sentences.
#' @param paragraph Logical. IF TRUE "<New paragraph>" is added at the end of each paragraph to enable manual splitting at paragraphs.
#' @param cermine Logical. If TRUE CERMINE specific error handling and letter conversion will be applied. If set to "auto" file name ending with 'cermxml$' will set cermine=TRUE.
#' @param rm.table Logical. If TRUE removes <table> tag from text.
#' @param rm.formula Logical. If TRUE removes <formula> tags.
#' @param rm.xref Logical. If TRUE removes <xref> tag (citing) from text.
#' @param rm.media Logical. If TRUE removes <media> tag from text.
#' @param rm.graphic Logical. If TRUE removes <graphic> and <fig> tag from text.
#' @param rm.ext_link Logical. If TRUE removes <ext link> tag from text.
#' @export

get.text<-function(x,sectionsplit="",
                  grepsection="",
                  letter.convert=TRUE,
                  greek2text=FALSE,
                  sentences=FALSE,
                  paragraph=FALSE,
                  cermine="auto",
                  rm.table=TRUE,
                  rm.formula=TRUE,
                  rm.xref=TRUE,
                  rm.media=TRUE,
                  rm.graphic=TRUE,
                  rm.ext_link=TRUE
){
captions<-character(0)

# set cermine if "auto"
if(cermine=="auto") cermine<-ifelse(length(grep("cermxml$",x[1]))>0,TRUE,FALSE)
# readLines if x is file
if(file.exists(x[1])) x<-readLines(x,warn=FALSE,encoding="UTF-8")
# remove strangely converted lines in CERMXML (mostly from tables)
if(cermine==TRUE){
# remove lines with at least "num (num), num (num)" or "num (num) num (num)"
x<-grep("[-0-9\\.] \\([-0-9\\.]*\\). [-0-9\\.]* \\([-0-9\\.]*\\)|[-0-9\\.] \\([-0-9\\.]*\\) [-0-9\\.]* \\([-0-9\\.]*\\)",x,invert=TRUE,value=TRUE)
i<-NULL
# lines with no .,; and < nor word with at least 4 letters
i<-c(i,which(nchar(x)<15)[which(is.element(which(nchar(x)<15),grep("[-\\.,,;<)(]|[a-z]{4}|[a-zA-Z]{2}$",x,invert=TRUE)))])
# lines with three dotted numbers in a row
i<-c(i,grep("\\.[0-9]*? *?\\.[0-9]*? *?\\.[0-9]*?",x))
i<-c(i,grep("\\.[0-9]*? *?[0-9]\\.[0-9]*? *?[0-9]\\.[0-9]*?",x))
i<-c(i,grep("^[a-z0-9A-Z] [a-z0-9A-Z] [a-z0-9A-Z] ",x))
i<-c(i,grep("^[a-z0-9A-Z][a-z0-9A-Z] [a-z0-9A-Z][a-z0-9A-Z] [a-z0-9A-Z][< ]",x))
i<-c(i,grep("^[a-z0-9A-Z] [a-z0-9A-Z][a-z0-9A-Z] [a-z0-9A-Z][< ]",x))
i<-c(i,grep("^[a-z0-9A-Z] [a-z0-9A-Z][a-z0-9A-Z] [a-z0-9A-Z][a-z0-9A-Z][< ]",x))
i<-c(i,grep("^-[a-z0-9A-Z] [a-z0-9A-Z] [a-z0-9A-Z]",x))
i<-c(i,grep("^-[a-z0-9A-Z][a-z0-9A-Z] [a-z0-9A-Z][a-z0-9A-Z] [a-z0-9A-Z][< ]",x))
i<-c(i,grep("^-[a-z0-9A-Z] [a-z0-9A-Z][a-z0-9A-Z] [a-z0-9A-Z][< ]",x))
i<-c(i,grep("^-[a-z0-9A-Z] [a-z0-9A-Z][a-z0-9A-Z] [a-z0-9A-Z][a-z0-9A-Z][< ]",x))
i<-c(i,grep(" [a-z0-9A-Z] [a-z0-9A-Z] [a-z0-9A-Z] ",x))
i<-c(i,grep("[a-z0-9A-Z]{22}",x))
i<-c(i,grep("[,\\.;][a-zA-Z] [a-zA-Z0-9] ",x))
i<-c(i,grep("^.$",x))
i<-c(i,grep(" [;,] [;,] |,[a-zA-z]",x))
i<-c(i,grep("^. .$",x))
i<-c(i,grep("<p>[a-zA-Z]$",x))
i<-c(i,grep("<p>[a-zA-Z0-9] [a-zA-Z0-9] [a-zA-Z0-9] ",x))
i<-c(i,grep("<p>[0-9] [a-zA-Z0-9] [a-zA-Z0-9] |<p>[0-9][0-9] [a-zA-Z0-9] [a-zA-Z0-9] ",x))
i<-c(i,grep(" - [a-zA-Z] $",x))
i<-c(i,grep(" -[0-9] [a-zA-Z] $",x))
i<-c(i,grep(" [^ ] [^ ] [^ ] [^ ] ",x))
i<-c(i,grep("^[^ ] [^ ] [^ ] [^ ]",x))
i<-c(i,grep("^[A-Za-z] -[^ ] [^ ]",x))
i<-c(i,grep("[a-zA-Z]\\%",x))
i<-sort(unique(i))
# only take such lines that don't have an important tag
s<-grep("<xref|</xref|<sec[ >]|</sec>|article-type=| p [\\.0-9][0-9\\.]|[\\(\\[][0-9A-Za-z]*?[\\]\\)]",x)
i<-i[!is.element(i,s)]
# and such that don't have short words
s<-grep(" is | be | was | or | and | to | in | at | the | no | not | we | F\\( | t\\(",x)
i<-i[!is.element(i,s)]
s<-grep(" is^| be^| was^| or^| and^| to^| in^| at^| the^| no^| not^| we^| F \\( | t \\(|^[ \\(\\)\\-]*?$",x)
i<-i[!is.element(i,s)]
if(length(i>0)) x<-x[-i]
}

# for text with <sec> tag
if(length(grep("<sec |<sec>",x))>0&length(grep("article-type=\"correction\">",x))==0){
# collapse text to one line
temp<-paste(x,collapse=" ")
# remove text til end of abstract
temp<-gsub(".*</abstract","",temp)
# split text at sections
txt<-unlist(strsplit(temp,"<sec |<sec>"))[-1]
# grep section names
sec<-gsub(".*<title>","",gsub("</title>.*","",txt)) 

## section clean up
# remove html tags from section titles
sec<-gsub("<.*?.*>"," ",sec)
sec<-gsub("  "," ",sec)
# hex letter conversion
if(length(grep("\\&#",sec))>0&letter.convert==T) sec<-letter.convert(sec,cermine=cermine,greek2text=greek2text)
# remove numbers in front of line
while(length(grep("^[1-9]\\.|^[0-9] ",sec))>0) sec<-gsub("^[1-9]\\.|^[0-9] |^\\.","",sec)
# remove brakets in front of line
while(length(grep("^\\(.*?.*\\) |\\(.\\) ",sec))>0) sec<-gsub("^\\(.*?.*\\) |\\(.\\) ","",sec)

## text clean up
# remove text after begin of reference list or end of body
txt<-gsub("<ref-lis.*|</body.*","",txt)
# remove text til first </title>
txt<-sub(".*?(</title>)|.*?(<title/>)", "", txt)

# letter convert
if(letter.convert==TRUE) txt<-letter.convert(txt,cermine=cermine,greek2text=greek2text,warning=FALSE)

 
# unify most usual degree of fredoms noted in "[]" to "()" in statistical reports
txt<-gsub("([^a-zA-z][FRrzHQZTt2])\\[([0-9\\. ,;]*?)\\]","\\1(\\2)",txt)
txt<-gsub("([^a-zA-z][FRrzHQZTt2]) \\[([0-9\\. ,;]*?)\\]","\\1(\\2)",txt)
txt<-gsub("(^[FRrzQHZTt2])\\[([0-9\\. ,;]*?)\\]","\\1(\\2)",txt)
txt<-gsub("(^[FRrzQHZTt2]) \\[([0-9\\. ,;]*?)\\]","\\1(\\2)",txt)

# clean up white spaces
txt<-gsub("^ *|(?<= ) | *$", "", txt, perl = TRUE)

## split text to section split words
# for text with sections
if(length(sec)>1){
  # get index of lines to collapse
  index<-unique(
  c(1,  grep(paste(tolower(sectionsplit),collapse="|"),tolower(sec))  ,ifelse(is.element(length(sec),grep(paste(tolower(sectionsplit),collapse="|"),tolower(sec))),length(sec)+1,length(sec)+1))
  )
  section<-NULL;textred<-NULL
  # collapse lines by "sectionsplit"
  for(i in 1:(length(index)-1)){
    from<-index[i]
    to<-index[i+1]-1
    if(i==length(index)) to<-index[i+1]
    section<-c(section,paste(sec[from:to],collapse=";; "))
    textred<-c(textred,paste(txt[from:to],collapse=" "))
    }
  }else {textred<-txt;section<-sec}


# reduce content to lines with grepsection pattern
if(grepsection!=""){
textred<-textred[grep(grepsection,section)]
section<-section[grep(grepsection,section)]
}
  
  
# extract captions from tables and figures  
captions<-character(0)
if(sum(unlist(grepl("</caption>",textred)))>0){
# split lines with captions
temp<-unlist(textred)
captions<-unlist(strsplit2(temp,"</caption>","after"))
# select lines with </caption>
captions<-grep("</caption>",captions,value=TRUE)
# clean up
captions<-gsub("</caption>","",gsub(".*<caption>|<caption>|.*<caption.*?.*\"/>|<caption.*?.*\"/>|.*<caption.*?.*\">|<caption.*?.*\">","",captions))
# remove html
captions<-gsub("</[a-z]*>|</[a-z]>|<[a-z]*>|<[a-z]>|<caption.*?.*\"/>|<xref.*?.*\"/>|<xref.*?.*\">","",captions)
} 
 
## Text clean up after sentencing
# remove listing definition
textred<-lapply(textred,function(x) gsub("<list-item>","-",x))
textred<-lapply(textred,function(x) gsub("</list-item>","",x))
textred<-lapply(textred,function(x) gsub("<list list-type.*?>","",x))
# remove <table> from text
#if(rm.table==T) textred<-lapply(textred,function(x) gsub("<tr.*?</tr>","",x))
#if(rm.table==T) textred<-lapply(textred,function(x) gsub("<tr.*?</td>","",x))
#if(rm.table==T) textred<-lapply(textred,function(x) gsub("<tbody>.*?</tbody>","",x))
#if(rm.table==T) textred<-lapply(textred,function(x) gsub("<thead>.*?</thead>","",x))
if(rm.table==T) textred<-lapply(textred,function(x) gsub("<table.*?</table>|<table-wrap .*?</table-wrap>|<table-wrap-foot.*?</table-wrap-foot>","",x))
if(rm.table==T) textred<-lapply(textred,function(x) gsub("<table>|</table>|<table-wrap>|</table-wrap>|<table-wrap-foot>|</table-wrap-foot>","",x))

# remove <xref> from text
if(rm.xref==T)  textred<-lapply(textred,function(x) gsub("</xref>","",gsub("<xref.*?>","",x)))
#if(rm.xref==T)  textred<-lapply(textred,function(x) gsub("<xref.*?.*</xref>","",x))
# remove <media> from text
if(rm.media==T) textred<-lapply(textred,function(x) gsub("<media.*?.*</media>","",x))
# remove <ext-link> from text
if(rm.ext_link==T) textred<-lapply(textred,function(x) gsub("<ext-link.*?>|</ext-link>","",x))
# remove <graphic> from text
if(rm.graphic==T){
  textred<-lapply(textred,function(x) gsub("<graphic.*?.*</graphic>","removedGRAPHIC",x))
  textred<-lapply(textred,function(x) gsub("<fig.*?.*</fig>","removedGRAPHIC",x))
  textred<-lapply(textred,function(x) gsub("<object-id.*?.*</object-id>","",x))
  textred<-lapply(textred,function(x) gsub("<graphic.*?.*</fig>","removedGRAPHIC",x))
  textred<-lapply(textred,function(x) gsub("<fig.*?.*\">","removedGRAPHIC",x))
  textred<-lapply(textred,function(x) gsub("<label.*?.*</label>","",x))
  textred<-lapply(textred,function(x) gsub("<graphic.*?.*\"/>","removedGRAPHIC",x))
}

# remove formula-tags from text
if(rm.formula==T){
  textred<-lapply(textred,function(x) gsub("<disp .*?.*\"/>|</disp .*?.*>"," ",x))
  textred<-lapply(textred,function(x) gsub("<inline-f.*?>|</inline-f.*?>"," ",x))
  textred<-lapply(textred,function(x) gsub("<mml.*?>|</mml.*?>"," ",x))
}

# remove <caption> from text if some is left
textred<-lapply(textred,function(x) gsub("</caption>","",gsub("<caption.*?.*</caption>","",x)))
# add "<New paragraph>.
if(paragraph==TRUE){
  textred<-lapply(textred,function(x) gsub("</p>"," <New paragraph>. ",x))
  textred<-lapply(textred,function(x) gsub(" $| <New paragraph>. $","",x))
}
# remove <title> and <p> and <sec> <italic> <b> <bold>  <undeline> tag 
textred<-lapply(textred,function(x) gsub("<title>|</title>|<p>|</p>|<sec>|</sec>|<italic>|</italic>|<italic toggle=\"yes\">|<italic toggle=\"no\">"," ",x))
textred<-lapply(textred,function(x) gsub("<b>|</b>|<bold>|</bold>|</sup>|<sub>|</sub>|<underline>|</underline>"," ",x))
textred<-lapply(textred,function(x) gsub("<supplementary-material>|</supplementary-material>|<alternatives>|</alternatives>"," ",x))
# replace <sup> with ^
textred<-lapply(textred,function(x) gsub("<sup>","^",x))
# remove paragraph id
textred<-lapply(textred,function(x) gsub("<p id.*?.*>|<supplementary.*?.*\">","",x))
# remove citing breaks []
#textred<-gsub("\\[.*?.*]|\\[\\]","",textred)


# clean up ",," and unlist
textred<-lapply(textred,function(x) gsub(", ,|, , ,|,,|,,,",",",x))
textred<-lapply(textred,function(x) gsub(", \\.|,\\.",".",x))
textred<-unlist(textred)
#textred<-lapply(textred,paste,collapse=" ")
# clean up white spaces
textred<-gsub("^ *|(?<= ) | *$", "", textred, perl = TRUE)

# paste empty (nchar<2) textlines t with t+1, same for section
index<-(1:length(textred))[nchar(textred)<=2]
if(length(index)>0){
# reverse index
index<-index[length(index):1]
# remove 1
index<-index[index!=1]
if(length(index)>0){
# and paste empty text and section lines
for(i in index){
textred[i-1]<-paste(textred[i-1],textred[i],sep=";; ")
section[i-1]<-paste(section[i-1],section[i],sep=";; ")
}
textred<-textred[-index]
section<-section[-index]
}
}


# if first textelement is empty paste 1:2 sections and remove first text element
if(!is.na(textred[1])) if(nchar(textred[1])<=2){
 textred[1]<-paste(textred[1],textred[2],sep=";; ")
 textred<-textred[-2]
 section[1]<-paste(section[1],section[2],sep=";; ")
 section<-section[-2]
}

# remove double spaces and remaining space/end in front of line
textred<-gsub("^ *|(?<= ) | *$", "", textred, perl = TRUE)
# hex code to letter
if(letter.convert==T) textred<-letter.convert(textred,cermine=cermine,warning=FALSE)
# remove space before . and ,
textred<-gsub(" \\. ",". ",gsub(" , ",", ",textred))
textred<-gsub(" \\. ",". ",gsub(",,",",",textred))
# set ",." to "."
textred<-gsub(", \\.|,\\.",".",gsub(" , ",", ",textred))
# remove white space in brackets
textred<-gsub(" [)]",")",gsub("[(] ","(",textred))
textred<-gsub(" [\\]]","]",gsub("[\\[] ","[",textred))
# remove white space at beginning and end
textred<-gsub("^ | $","",textred)
# remove empty brakets
textred<-gsub(" \\(,\\)| \\(\\)","",textred)
# [ bracket clean up
textred<-gsub("\\[\\]|\\[[;,-] \\]|\\[[;,-] [;,-] \\]|\\[\\-\\]| \\[ \\]","",textred)
# superscript clean up
textred<-gsub(" \\^ | \\^|\\^ ","^",textred)

}else{textred<-"";section<-""}
if(!exists("section")) section<-""
if(!exists("textred")) textred<-""

section<-gsub(";; ;;",";; ",section)
# remove emty sections and text elements if length of section > 1
if(sum(section==""&textred=="")>0){
 if(length(section)>1){
  i<-which(section==""&textred=="")*-1
  textred<-textred[i]
  section<-section[i]
  }
 }

textred<-lapply(textred,function(x) gsub(" *<New paragraph>. *;;",";;",x))

if(sentences==TRUE) textred<-lapply(textred,text2sentences) 
if(sentences==TRUE) captions<-lapply(captions,text2sentences) 

# convert "<New paragraph>. " -> <New paragraph> and remove it at end of section
if(paragraph==T) textred<-lapply(textred,function(x) gsub("<New paragraph>. *","<New paragraph> ",x))
if(paragraph==T) textred<-lapply(textred,function(x) gsub("^<New paragraph> ","<New paragraph>",x))
if(paragraph==T) textred<-lapply(textred,function(x) gsub("<New paragraph> *$","",x))


# remove if text only contains html tag  
textred<-lapply(textred,function(x){
  out<-x[length(grep("^</*?[a-z]*>$|^</*?[a-z]*></*?[a-z]*>$",x))==0]
  if(length(out)==0) out<-""
  return(out)
})

captions<-lapply(captions,function(x){
  out<-x[length(grep("^</*?[a-z]*>$|^</*?[a-z]*></*?[a-z]*>$",x))==0]
  if(length(out)==0) out<-""
  return(out)
})

# output as list
list(section=section,text=textred,captions=captions)
}

