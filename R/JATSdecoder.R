#' JATSdecoder
#'
#' Function to extract and structure NISO-JATS coded XML file or text into a list
#' @param x a NISO-JATS coded XML file or text
#' @param sectionsplit search patterns for section split (forced to lower case), e.g. c("intro","method","result","discus")
#' @param grepsection search pattern to reduce text to specific section namings only
#' @param sentences Logical. IF TRUE text is returned as sectioned list with sentences
#' @param output selection of specific results to output c("all","title","author","affiliation","journal","volume","editor","doi","type","history","country","subject","keywords", "abstract", "sections", "text", "tables", "captions", "references")
#' @param letter.convert Logical. If TRUE converts hex and html coded characters to unicode
#' @param warning Logical. If TRUE outputs a warning if processing CERMINE converted PDF files
#' @param unify.country.name Logical. If TRUE tries to unify country name/s with list of country names from worldmap() 
#' @param greek2text Logical. If TRUE converts and unifies several greek letters to textual representation, e.g.: alpha
#' @export

# define function
JATSdecoder<-function(x,sectionsplit=c("intro","method","result","study","experiment","conclu","implica","discussion"),grepsection="",
                         sentences=FALSE,output="all",letter.convert=TRUE,unify.country.name=TRUE, greek2text=FALSE,warning=TRUE){
# presettings
rm.na.history<-TRUE
rm.xref.text<-TRUE
rm.table.text<-TRUE
rm.formula.text<-TRUE
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

if(sum(is.element(c("all","sections","text","captions"),output))>0){
temp<-get.text(x,sectionsplit=sectionsplit,grepsection=grepsection,letter.convert=letter.convert,rm.table=rm.table.text,rm.xref=rm.xref.text, rm.graphic=rm.graphic.text,rm.formula=rm.formula.text,cermine=cerm,greek2text=greek2text,sentences=sentences)
sections<-temp$section
text<-temp$text
captions<-temp$captions
}

# fill objects with content or NULL for list
ifelse(sum(is.element(c("all","file"),output))>0,       file<-file,                            file<-NA)
ifelse(sum(is.element(c("all","title"),output))>0,       title<-get.title(x),                                  title<-NA)
ifelse(sum(is.element(c("all","author"),output))>0,      author<-get.author(x,letter.convert=letter.convert),  author<-NA)
ifelse(sum(is.element(c("all","affiliation"),output))>0, affiliation<-get.aff(x,remove.html=T), affiliation<-NA)
ifelse(sum(is.element(c("all","journal"),output))>0,     journal<-get.journal(x),                              journal<-NA)
ifelse(sum(is.element(c("all","volume"),output))>0,      volume<-get.vol(x),                                   volume<-NA)
ifelse(sum(is.element(c("all","editor"),output))>0,     editor<-get.editor(x,letter.convert=letter.convert),  editor<-NA)
ifelse(sum(is.element(c("all","doi"),output))>0,        doi<-get.doi(x),                                    doi<-NA)
ifelse(sum(is.element(c("all","type"),output))>0,        type<-get.type(x),                                    type<-NA)
ifelse(sum(is.element(c("all","history"),output))>0,     history<-get.history(x,remove.na=rm.na.history),      history<-NA)
ifelse(sum(is.element(c("all","country"),output))>0,     country<-get.country(get.aff(x)),                     country<-NA)
ifelse(sum(is.element(c("all","subject"),output))>0,     subject<-get.subject(x,letter.convert=letter.convert),subject<-NA)
ifelse(sum(is.element(c("all","keywords"),output))>0,    keywords<-get.keywords(x,letter.convert=letter.convert),keywords<-NA)
ifelse(sum(is.element(c("all","abstract"),output))>0,    abstract<-text2sentences(get.abstract(x,sentences=sentences,letter.convert=letter.convert,cermine=cerm)), abstract<-NA)
ifelse(sum(is.element(c("all","sections"),output))>0,    sections<-sections,                                   sections<-NA)
ifelse(sum(is.element(c("all","text"),output))>0,        text<-text,                                           text<-NA)
ifelse(sum(is.element(c("all","tables"),output))>0,    tables<-get.tables(x),                                   tables<-NA)
ifelse(sum(is.element(c("all","captions"),output))>0,    captions<-captions,                                   captions<-NA)
ifelse(sum(is.element(c("all","references"),output))>0,  references<-get.references(x,letter.convert=letter.convert,remove.html=T),references<-NA)

res<-list(
 file=file,
 title=title,
 author=author,
 affiliation=affiliation,
 journal=journal,
 volume=volume,
 editor=editor,
 doi=doi,
 history=history,
 country=country,
 type=type,
 subject=subject,
 keywords=keywords,
 abstract=abstract,
 sections=sections,
 text=text,
 tables=tables,
 captions=captions,
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
if(cerm==TRUE&warning==TRUE)  warning("CERMINE specific letter conversion was used to correct for some conversion errors. '<=>' was inserted by letter.convert() to make statistics readable. The minus sign rarely gets converted to '2' what cannot always be handled correctly.")  
}

