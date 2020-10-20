#' JATSdecoder
#'
#' Function to extract and structure NISO-JATS coded XML file or text into a list
#' @param x a NISO-JATS coded XML file or text
#' @param output selection of specific results to output c("all","title","author","affiliation","journal","volume","editor","doi","type","history","country","subject","keywords", "abstract", "sections", "text", "references")
#' @param sectionsplit search patterns for section split (forced to lower case), e.g. c("intro","method","result","discus")
#' @param letter.convert Logical. If TRUE converts hex and html coded characters to unicode
#' @param warning Logical. If TRUE outputs a warning if processing CERMINE converted PDF files
#' @param unify.country.name Logical. If TRUE tries to unify country name/s with list of country names from worldmap() 
#' @param greek2text Logical. If TRUE converts and unifies several greek letters to textual representation, e.g.: alpha
#' @export

#' @examples
#' URL <- "https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
#' download.file(URL,"file.xml")
#' JATSdecoder("file.xml")

# define function
JATSdecoder<-function(x,sectionsplit=c("intro","method","result","study","experiment","conclu","implica","discussion"),
                         output="all",letter.convert=TRUE,unify.country.name=TRUE, greek2text=FALSE,warning=FALSE){
# presettings
rm.na.history<-TRUE
rm.xref.text<-TRUE
rm.table.text<-TRUE
rm.graphic.text<-TRUE
# check if x is xml file else stop
if(length(grep("^<\\?xml",x))==0) if(length(grep("xml$|XML$",x[1]))==0) stop("file is not in XML nor NISO-JATS format")

# check if x is cermine file
ifelse(length(grep("cermxml$",x[1]))==1,cerm<-TRUE,cerm<-FALSE)
                         
# if x is file readLines
if(length(grep("\\.nxml$|cermxml$|\\.xml$|XML$",x[1]))==1){
    file<-x
    x<-readLines(x,warn=F)
    }else file<-NA

# remove emty lines
x<-x[nchar(x)>0]

# if x is not JATS coded stop
if(length(grep("!DOCTYPE",x[1:5]))==0) stop("x seems not to be a JATS coded file or text")

if(sum(is.element(c("all","sections","text"),output))>0){
temp<-get.text(x,sectionsplit,letter.convert=letter.convert,rm.table=rm.table.text,rm.xref=rm.xref.text, rm.graphic=rm.graphic.text,cermine=cerm,greek2text=greek2text)
text<-temp$text
sections<-temp$section
#if(is.na(sections)) section<-"NA"

}

# fill objects with content or NULL for list
ifelse(sum(is.element(c("all","file"),output))>0,       file<-x,                                  file<-NA)
ifelse(sum(is.element(c("all","title"),output))>0,       title<-get.title(x),                                  title<-NA)
ifelse(sum(is.element(c("all","author"),output))>0,      author<-get.author(x,letter.convert=letter.convert),  author<-NA)
ifelse(sum(is.element(c("all","affiliation"),output))>0, affiliation<-get.aff(x,remove.html=T),                affiliation<-NA)
ifelse(sum(is.element(c("all","journal"),output))>0,     journal<-get.journal(x),                              journal<-NA)
ifelse(sum(is.element(c("all","volume"),output))>0,      volume<-get.vol(x),                                   volume<-NA)
#ifelse(sum(is.element(c("all","editor"),output))>0,     editor<-get.editor(x,letter.convert=letter.convert),  editor<-NA)
ifelse(sum(is.element(c("all","doi"),output))>0,        doi<-get.doi(x),                                    doi<-NA)
ifelse(sum(is.element(c("all","type"),output))>0,        type<-get.type(x),                                    type<-NA)
ifelse(sum(is.element(c("all","history"),output))>0,     history<-get.history(x,remove.na=rm.na.history),      history<-NA)
ifelse(sum(is.element(c("all","country"),output))>0,     country<-get.country(get.aff(x)),                     country<-NA)
ifelse(sum(is.element(c("all","subject"),output))>0,     subject<-get.subject(x,letter.convert=letter.convert),subject<-NA)
ifelse(sum(is.element(c("all","keywords"),output))>0,    keywords<-get.keywords(x,letter.convert=letter.convert),keywords<-NA)
ifelse(sum(is.element(c("all","abstract"),output))>0,    abstract<-text2sentences(get.abstract(x,letter.convert=letter.convert,cermine=cerm)), abstract<-NA)
ifelse(sum(is.element(c("all","sections"),output))>0,    sections<-sections,                                   sections<-NA)
ifelse(sum(is.element(c("all","text"),output))>0,        text<-text,                                           text<-NA)
ifelse(sum(is.element(c("all","references"),output))>0,  references<-get.references(x,letter.convert=letter.convert,remove.html=T),references<-NA)

res<-list(
 title=title,
 author=author,
 affiliation=affiliation,
 journal=journal,
 volume=volume,
# editor=editor,
 doi=doi,
 history=history,
 country=country,
 type=type,
 subject=subject,
 keywords=keywords,
 abstract=abstract,
 sections=sections,
 text=text,
 references=references
)
if(unify.country.name==TRUE&sum(is.element(c("all","country"),output))>0) res$country=.unifyCountry(unlist(res$country))
# remove NULL results
#res<-res[!unlist(lapply(lapply(res,"[",1),is.na))]

# reduce output to list of desired outputs
if(!is.element("all",output)) res<-res[is.element(names(res),output)]

# unlist to vector if length of res==1
#if(length(res)==1) res<-unlist(res)
return(res)
if(cerm==TRUE&warning==FALSE) warning("You are processing a CERMINE converted pdf. Some special characters and operators may be lost in conversion process. At time the minus sign is converted to a '2' or not at all.")
}

