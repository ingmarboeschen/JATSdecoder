#' JATSdecoder
#'
#' Function to extract and restructure NISO-JATS coded XML file or text into a list with metadata and text as selectable elements. Use \href{https://github.com/CeON/CERMINE}{CERMINE} to convert PDF to CERMXML files.
#' @param x a NISO-JATS coded XML file or text.
#' @param sectionsplit search patterns for section split of text parts (forced to lower case), e.g. c("intro", "method", "result", "discus").
#' @param grepsection search pattern in regex to reduce text to specific section only.
#' @param sentences Logical. IF TRUE text is returned as sectioned list with sentences.
#' @param paragraph Logical. IF TRUE "<New paragraph>" is added at the end of each paragraph to enable manual splitting at paragraphs.
#' @param abstract2sentences Logical. IF TRUE abstract is returned as vector with sentences.
#' @param output selection of specific results to output c("all", "title", "author", "affiliation", "journal", "volume", "editor", "doi", "type", "history", "country", "subject", "keywords", "abstract", "sections", "text", "tables", "captions", "references").
#' @param letter.convert Logical. If TRUE converts hexadecimal and HTML coded characters to Unicode.
#' @param warning Logical. If TRUE outputs a warning if processing CERMINE converted PDF files.
#' @param unify.country.name Logical. If TRUE tries to unify country name/s with list of country names from worldmap(). 
#' @param greek2text Logical. If TRUE converts and unifies several greek letters to textual representation, e.g.: "alpha".
#' @param countryconnection Logical. If TRUE outputs country connections as vector c("A - B","A - C", ...).
#' @param authorconnection Logical. If TRUE outputs connections of a maximum of 50 involved authors as vector c("A - B","A - C", ...).
#' @return List with extracted meta data, sectioned text and references.
#' @note A short tutorial on how to work with JATSdecoder and the generated outputs can be found at: \href{https://github.com/ingmarboeschen/JATSdecoder}{https://github.com/ingmarboeschen/JATSdecoder}  
#' @source An interactive web application for selecting and analyzing extracted article metadata and study characteristics for articles linked to PubMed Central is hosted at: \href{https://www.scianalyzer.com}{https://www.scianalyzer.com/}
#' @source The XML version of PubMed Central database articles can be downloaded in bulk from:\cr\href{https://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/}{https://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/}
#' @seealso \code{\link[JATSdecoder]{study.character}} for extracting different study characteristics at once.
#' @seealso \code{\link[JATSdecoder]{get.stats}} for extracting statistical results from textual input and different file formats.
#' @references Böschen (2021). "Software review: The JATSdecoder package - extract metadata, abstract and sectioned text from NISO-JATS coded XML documents; Insights to PubMed Central’s open access database.” \emph{Scientometrics.} doi: \href{https://link.springer.com/article/10.1007/s11192-021-04162-z}{10.1007/s1119202104162z}.
#' @export
#' @examples
#' # download example XML file via URL
#' x<-"https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
#' # file name
#' file<-paste0(tempdir(),"/file.xml")
#' # download URL as "file.xml" in tempdir()
#' download.file(x,file)
#' # convert full article to list with metadata, sectioned text and reference list
#' JATSdecoder(file)
#' # extract specific content (here: abstract and text)
#' JATSdecoder(file,output=c("abstract","text"))
#' # or use specific functions, e.g.:
#' get.abstract(file)
#' get.text(file)

JATSdecoder<-function(x,sectionsplit=c("intro","method","result","study","experiment","conclu","implica","discussion"),grepsection="",
                         sentences=FALSE,paragraph=FALSE,abstract2sentences=TRUE,output="all",letter.convert=TRUE,unify.country.name=TRUE, greek2text=FALSE,warning=TRUE,
                      countryconnection=FALSE,authorconnection=FALSE){
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
    x<-readLines(x,warn=FALSE,encoding="UTF-8")
    }else file<-NA

# remove emty lines
x<-x[nchar(x)>0]

# if x is not JATS coded stop
if(length(grep("!DOCTYPE",x[1:15]))==0) stop("x seems not to be a JATS coded file or text")

if(sum(is.element(c("all","sections","text","captions"),output))>0){
temp<-get.text(x,sectionsplit=sectionsplit,grepsection=grepsection,letter.convert=letter.convert,rm.table=rm.table.text,rm.xref=rm.xref.text, rm.graphic=rm.graphic.text,rm.formula=rm.formula.text,cermine=cerm,greek2text=greek2text,sentences=sentences,paragraph=paragraph)
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
ifelse(sum(is.element(c("all","history"),output))>0,     history<-as.list(get.history(x,remove.na=rm.na.history)),      history<-NA)
ifelse(sum(is.element(c("all","country"),output))>0,     country<-get.country(get.aff(x),unifyCountry=unify.country.name),                     country<-NA)
ifelse(sum(is.element(c("all","subject"),output))>0,     subject<-get.subject(x,letter.convert=letter.convert),subject<-NA)
ifelse(sum(is.element(c("all","keywords"),output))>0,    keywords<-get.keywords(x,letter.convert=letter.convert),keywords<-NA)
ifelse(sum(is.element(c("all","abstract"),output))>0,    abstract<-get.abstract(x,sentences=abstract2sentences,letter.convert=letter.convert,cermine=cerm), abstract<-NA)

if(sum(is.element(c("all","sections"),output))>0) sections<-sections else sections<-NA
if(sum(is.element(c("all","text"),output))>0) text<-text else text<-NA

if(sum(is.element(c("all","tables"),output))>0)          tables<-as.vector(get.tables(x)) else tables<-NA
if(sum(is.element(c("all","captions"),output))>0)     captions<-as.vector(captions) else captions<-NA

    if(sum(is.element(c("all","references"),output))>0) references<-as.vector(get.references(x,letter.convert=letter.convert,remove.html=T)) else references<-NA

ifelse(countryconnection==TRUE, countryconnections<-get.cons(country), countryconnections<-NA)
ifelse(authorconnection==TRUE, authorconnections<-get.cons(author,max.items=25), authorconnections<-NA)

# create list
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
 references=references,
 countryconnections=countryconnections,
 authorconnections=authorconnections
)

if(countryconnection!=TRUE) res<-within(res,rm("countryconnections"))
if(authorconnection!=TRUE) res<-within(res,rm("authorconnections"))

# remove NULL results
#res<-res[!unlist(lapply(lapply(res,"[",1),is.na))]

# reduce output to list of desired outputs
if(!is.element("all",output)) res<-res[is.element(names(res),output)]

# unlist to vector if length of res==1
#if(length(res)==1) res<-unlist(res)
return(res)
if(cerm==TRUE&warning==TRUE)  warning("CERMINE specific letter conversion was used to correct for some conversion errors. '<=>' was inserted by letter.convert() to make statistics readable. The minus sign rarely gets converted to '2' what cannot always be handled correctly.")  
}


## helper functions
# create connection vector
get.net<-function(x,max.items=Inf){
    n<-NULL
    if(is.character(x)&!is.null(x)){
        if(length(x)>1){
            if(is.null(max.items)|length(x)<=max.items){
                x<-sort(x)
                for(i in 1:(length(x)-1)){
                    for(j in (i+1):length(x)){
                        n<-cbind(rbind(n,c(x[i],x[j])))
                    }}
            }# end remove max.conn
        }}
    return(n)
}

get.cons<-function(x,max.items=Inf){
    connection<-get.net(x,max.items=max.items)
    connection<-paste(connection[,1],connection[,2],sep=" - ")
    return(connection)
}
