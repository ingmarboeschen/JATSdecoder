#' get.history
#'
#' Extracts available publishing history tags from NISO-JATS coded XML file or text and compute pubDate and pubyear.
#' @param x a NISO-JATS coded XML file or text.
#' @param remove.na Logical. If TRUE hides non available date stamps.
#' @seealso \code{\link[JATSdecoder]{JATSdecoder}} for simultaneous extraction of meta-tags, abstract, sectioned text and reference list.
#' @return Character vector with the extracted dates of publishing history.
#' @export

get.history<-function(x,remove.na=FALSE){
  # run prechecks or readLines(x) if x is file
  x<-preCheck(x)
  
  # prepare
temp<-paste(x,collapse=" ")
temp<-unlist(strsplit(temp,"<history>|<pub-history>|<pub-date"))[-1]
temp<-gsub("<history>|<pub-history>|<pub-date.*","",gsub("</history.*|</pub-*","",temp))
temp<-unlist(strsplit(temp,"</date>"))
# clean up
temp<-gsub("date>.*","",gsub("<permissions.*","",temp))
# remove text after patterns
temp<-gsub("article-type.*|doi.*|id>.*|<volume.*","",temp)
# remove lines without <year> tag
temp<-grep("<year>",temp,value=TRUE)
temp
# get dates
year<-ifelse(gsub("year","",temp)!=temp, gsub(".*<year>","",gsub("</yea.*","",temp)), NA)
month<-ifelse(gsub("month","",temp)!=temp, gsub(".*<month>","",gsub("</mo.*","",temp)), NA)
day<-ifelse(gsub("day","",temp)!=temp, gsub(".*<day>","",gsub("</day.*","",temp)), NA)
date<-paste(year,month,day,sep="-")
date<-gsub(".*>","",date)
date
# remove doublicated date types
date<-date[!duplicated(gsub(">.*","",gsub(".*-type=","",temp)))]
temp<-temp[!duplicated(gsub(">.*","",gsub(".*-type=","",temp)))]


ifelse(length(grep("\"rev-recd",temp))==1,rev_recd<-date[grep("\"rev-recd",temp)],rev_recd<-NA)
#ifelse(length(grep("published",temp))==1,published<-date[grep("published",temp)],published<-NA)
ifelse(length(grep("collection",temp))==1,collection<-date[grep("collection",temp)],collection<-NA)
ifelse(length(grep("received",temp))==1,received<-date[grep("received",temp)],received<-NA)
ifelse(length(grep("retracted",temp))==1,retracted<-date[grep("retracted",temp)],retracted<-NA)
ifelse(length(grep("\"submitted",temp))==1,submitted<-date[grep("\"submitted",temp)],submitted<-NA)
ifelse(length(grep("nihms-submitted",temp))==1,nihms_submitted<-date[grep("nihms-submitted",temp)],nihms_submitted<-NA)
#ifelse(length(grep("resubmitted",temp))==1,resubmitted<-date[grep("resubmitted",temp)],resubmitted<-NA)
ifelse(length(grep("accepted",temp))==1,accepted<-date[grep("accepted",temp)],accepted<-NA)
ifelse(length(grep("\"corrected",temp))==1,corrected<-date[grep("\"corrected",temp)],corrected<-NA)
ifelse(length(grep("epub",temp))==1,epub<-date[grep("epub",temp)],epub<-NA)
ifelse(length(grep("epreprint",temp))==1,epreprint<-date[grep("epreprint",temp)],epreprint<-NA)
ifelse(length(grep("ecorrected",temp))==1,ecorrected<-date[grep("ecorrected",temp)],ecorrected<-NA)
ifelse(length(grep("pmc-release",temp))==1,pmc<-date[grep("pmc-release",temp)],pmc<-NA)
ifelse(length(grep("print",temp))==1,print<-date[grep("print",temp)],print<-NA)
ifelse(length(grep("\"pub",temp))==1,pub<-date[grep("\"pub",temp)],pub<-NA)
ifelse(length(grep("ppub",temp))==1,ppub<-date[grep("ppub",temp)],ppub<-NA)

if(length(grep("type",invert=TRUE,temp))==1&is.na(collection)) collection<-date[grep("type",invert=TRUE,temp)]
hist<-c(collection,received,submitted,nihms_submitted,accepted,epreprint,epub,ecorrected,print,pub,ppub,rev_recd,corrected,retracted,pmc)
names(hist)<-c("collection","received","submitted","nihms_submitted","accepted","epreprint","epub","ecorrected","print","pub","ppub","rev-recd","corrected","retracted","pmc-release")

# remove spaces and 0 at first character
hist<-gsub("^0","",gsub(" ","",hist))
# set long/short/wrongly captured dates to NA
hist[nchar(hist)<4]<-NA
hist[nchar(hist)>10]<-NA
# identify first publishing date and extract year published if possible (else NA)
vals<-grep("NA",hist[c("epub","print","pub","ppub","pmc-release","collection")],value=TRUE,invert=TRUE)
if(length(vals) == sum(is.na(vals))) pubDate<-NA else pubDate<-as.character(min(as.Date(vals),na.rm=T))
# extract pubyear
d<-as.numeric(grep("NA",substr(hist[c("epub","print","pub","ppub","pmc-release","collection")],1,4),value=TRUE,invert=TRUE))
# get first pubyear
if(sum(is.na(d))<length(d)){
    pubyear<-min(d,na.rm=T)
    }else pubyear<-Inf

if(pubyear==Inf&length(date)==1) pubyear<-substr(date,1,4)

# calculate time to accept/publish
time2accept<-tryCatch(suppressWarnings(format(as.Date(hist["accepted"])-as.Date(min(c(as.Date(hist["received"])),as.Date(hist["submitted"]),na.rm=TRUE)))),error=function(e) "NA")
time2publish<-tryCatch(suppressWarnings(format(as.Date(pubDate)-as.Date(hist["accepted"]))),error=function(e) "NA")
submit2publish<-tryCatch(suppressWarnings(format(as.Date(pubDate)-as.Date(min(c(as.Date(hist["received"])),as.Date(hist["submitted"]),na.rm=TRUE)))),error=function(e) "NA")

if(!is.na(time2accept)) if(substr(time2accept,1,2)=="NA") time2accept<-NA
if(!is.na(time2publish)) if(substr(time2publish,1,2)=="NA") time2publish<-NA
if(!is.na(submit2publish)) if(substr(submit2publish,1,2)=="NA") submit2publish<-NA

# remove NAs
if(remove.na==T) hist<-hist[!is.na(hist)]

# add puDate/year, time to accept/publish
hist<-c(hist,pubyear,pubDate)
names(hist)[(length(hist)-1):length(hist)]<-c("pubyear","pubDate")
hist<-gsub("Inf|-Inf",NA,hist)

hist<-c(hist,time2accept,time2publish,submit2publish)
names(hist)[(length(hist)-2):length(hist)]<-c("time2accept","time2publish","submit2publish")

hist<-gsub("-Inf|Inf",NA,hist)
hist<-gsub("Inf|Inf",NA,hist)

return(hist)
}

