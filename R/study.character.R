#' study.character
#'
#' extracts study characteristics out of a JATS coded XML file or JATSdecoder result
#' @param x JATS coded XML file or JATSdecoder result
#' @param text.mode text parts to extract statistical results from (text.mode=1: abstract and full text, text.mode=2: method and result section, text.mode=3: result section only)
#' @param captions Logical. If TRUE captions text will be scanned for statistical results
#' @param stats.mode Character. Select subset of standard stats. One of: "all", "checkable", "computable"
#' @param recalculate.p Logical. If TRUE recalculates p values (for 2 sided test) if possible
#' @param alternative Character. Select sidedness of recomputed p-values for t-, r- and Z-values. One of c("auto","undirected","directed","both"). If set to "auto" 'alternative' will be be set to 'both' if get.test.direction() detects one-directional hypotheses/tests in text. If no directional hypotheses/tests are dtected only "undirected" recomputed p-values will be returned
#' @param estimateZ Logical. If TRUE detected beta-/d-value is divided by reported standard error "SE" to estimate Z-value ("Zest") for observed beta/d and recompute p-value. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE capital letter T is treated as t-statistic when extracting statistics with get.stats()
#' @param R2r Logical. If TRUE capital letter R is treated as correlation when extracting statistics with get.stats()
#' @param selectStandardStats Select specific standard statistics only (e.g.: c("t","F","Chi2"))
#' @param update.package.list if TRUE updates available R packages with available.packages() function
#' @param add.software additional software names to detect as vector
#' @param quantileDF quantile of (df1+1)+(df2+1) to extract for estimating sample size
#' @param N.max.only return only maximum of estimated sample sizes
#' @param output output selection of specific results c("all", "doi", "title", "year", "n.studies", "methods", "alpha.error", "power", "multi.comparison.correction", "assumptions", "OutlierRemovalInSD", "InteractionModeratorMediatorEffect", "test.direction", "sig.adjectives", "software", "Rpackage", "stats", "standardStats", "estimated.sample.size")
#' @param rm.na.col Logical. If TRUE removes all columns with only NA in extracted standard statistics
#' @export

study.character<-function(x,
                  stats.mode="all",
                  recalculate.p=TRUE,
                  alternative="auto",
                  estimateZ=FALSE,
                  T2t=FALSE,
                  R2r=FALSE,
                  selectStandardStats=NULL,
                  captions=TRUE,
                  text.mode=1,
                  update.package.list=FALSE,
                  add.software=NULL,
                  quantileDF=.75,
                  N.max.only=FALSE,
                  output="all",
                  rm.na.col=TRUE){

caps<-character(0)
# check if x is xml file or list else stop
if(length(grep("^<\\?xml",x))==0) if(length(grep("xml$|XML$",x[1]))==0&!is.list(x)) stop("file is not in XML nor NISO-JATS format nor a JATSdecoder result")
# if x is file readLines else copy to temp
if(length(grep("\\.nxml$|cermxml$|\\.xml$|XML$",x[1]))==1){
  temp<-readLines(x,warn=F)
  }else temp<-x

# if output only contains standardStats add stats
nostat<-ifelse(!is.element("stats",output)&is.element("standardStats",output), TRUE, FALSE) # remove stat if not set
if(is.element("standardStats",output)) output<-unique(c("stats",output))

# check if input is made by cermine
ifelse(length(grep("cermxml$",x[1]))==1, cermine<-TRUE,  cermine<-FALSE)

## get method and result text
if(length(grep("!DOCTYPE",temp[1:5]))>0|sum(is.element(c("sections","text"),names(temp)))==2){
# for JATS coded xml
if(length(grep("!DOCTYPE",temp[1:5]))>0){
   t<-get.text(temp,sectionsplit=c("intro|background","data|statistic|method|material|sample|solution|analys|procedure|measures","participants|subjects|animals|patients","experiment|study","result|finding","conclusion","discussion","implication|limitation","conflict of|author contrib|ethic"),letter.convert=TRUE,cermine=cermine,greek2text=TRUE)
   if(is.element("captions",names(t))) caps<-t$captions
   }

# for JATSdecoder() results
if(sum(is.element(c("sections","text"),names(temp)))==2){
   if(is.element("captions",names(t))) caps<-t$captions
   t<-list("section"=unlist(temp["sections"]),"text"=unlist(temp["text"]))
   t<-lapply(t,letter.convert,cermine=TRUE,greek2text=TRUE)
   }

## get text from participants sections
   sections<-"participants|subjects|animals|patients"
     # get method section text as one line
     j<-grep(tolower(sections),tolower(unlist(t["section"])))
     text<-paste(unlist(t["text"])[j],collapse=" ")
     # clean up white spaces
     text<-gsub("^ *|(?<= ) | *$", "", text, perl = TRUE)
     # participants sentences
     participants<-text2sentences(text)
     rm(text)
## get text from method sections
   sections<-"data|statistic|method|material|sample|solution|analys|participants|procedure|measures|subjects|experiment|study|animals|patients"
     # get method section text as one line
     j<-grep(tolower(sections),tolower(unlist(t["section"])))
     text<-paste(unlist(t["text"])[j],collapse=" ")
     # clean up white spaces
     text<-gsub("^ *|(?<= ) | *$", "", text, perl = TRUE)
     # method sentences
     method<-text2sentences(text)
     rm(text)
## get text from result sections
   sections<-"result|finding"
     # get result section text as one line
     j<-grep(tolower(sections),tolower(unlist(t["section"])))
     text<-paste(unlist(t["text"])[j],collapse=" ")
     # clean up white spaces
     text<-gsub("^ *|(?<= ) | *$", "", text, perl = TRUE)
     # result sentences
     result<-text2sentences(text)
     rm(text)
   
## merge method and result sentences
   both<-unique(c(method,result))

## for bad section capture of CERMINE split and get text
if(cermine==T&length(both)==0&length(unlist(t["section"]))==1){ 
  # split text
  text<-unlist(strsplit2(unlist(t["text"]),"Method|Procedure|Material|Result|Findings|Discuss|Conclusion","before"))
  if(length(text)>1){
  # get method texts as one line
  method<-text2sentences(paste(grep("Method|Material|Procedure",text,value=TRUE),collapse=" "))
  # clean up white spaces
  method<-gsub("^ *|(?<= ) | *$", "", method, perl = TRUE)
  # get result texts as one line
  result<-text2sentences(paste(grep("Result|Findings",text,value=TRUE),collapse=" "))
  # clean up white spaces
  result<-gsub("^ *|(?<= ) | *$", "", result, perl = TRUE)
  # merge
  both<-unique(c(method,result))
  }else{
  result<-""
  method<-""
  }
}# end bad section exception

}# end get method and result text
   

if(length(both)==0) both<-""
# get all sentences from full text as vector
fulltext<-unlist(lapply(t,text2sentences))

###################
### export results
# from JATSdecoder result
if(sum(is.element(c("doi","title"),names(temp)))==2){
   doi<-as.character(unlist(temp["doi"]))
   title<-as.character(unlist(temp["title"]))
   year<-as.numeric(unlist(lapply(temp["history"],"[","pubyear")))
   abstract<-text2sentences(as.character(unlist(temp["abstract"])))
   temp<-"!DOCTYPE"
   }else{
# from JATS coded text
   doi<-get.doi(temp)
      title<-get.title(temp)
      year<-as.numeric(get.history(temp)["pubyear"])
      abstract<-text2sentences(get.abstract(temp,cermine=cermine))
      }

# get n studies from sections
if(length(grep("!DOCTYPE",temp[1:10]))>0){
 # get n studies from sections
 n.studies<-get.n.studies(unlist(t["section"]),tolower=TRUE)
   # go on searching only if n.studies==1
   if(n.studies>0){
      # get n studies from abstract
        n.studies.abstract<-get.n.studies(text2sentences(gsub("second [Ss]tudy","Study 2",abstract)),tolower=FALSE)
      }else n.studies.abstract<-1
# go on searching only if also n.studies.abstract==1
   if(n.studies==1&n.studies.abstract==1){
      # get n studies from result text and method if nothing found in abstract
        reduced<-grep("[1-2][0-9][0-9][0-9]",both,invert=TRUE,value=TRUE)
        n.studies.text<-get.n.studies(gsub("second [Ss]tudy","Study 2",reduced),tolower=FALSE)
     }else n.studies.text<-1
     # take maximum 
#     n.studies<-max(n.studies,n.studies.abstract,na.rm=T)
     n.studies<-max(n.studies,n.studies.abstract,n.studies.text,na.rm=TRUE)
    # End n.studies==1
}else{n.studies<-NA} # End no !DOCTYPE
# get characteristics
if(sum(is.element(c("all","methods"),output))>0){ methods<-get.method(both,cermine=cermine) }else{methods<-NA}
if(sum(is.element(c("all","alpha.error"),output))>0){ alpha.error<-get.alpha.error(both) }else{alpha.error<-NA}
if(sum(is.element(c("all","multi.comp"),output))>0){ multi.comp<-get.multi.comparison(both) }else{multi.comp<-NA}
if(sum(is.element(c("all","power"),output))>0){ power<-get.power(both) }else{power<-NA}
if(sum(is.element(c("all","assumptions"),output))>0){ assumptions<-get.assumptions(both) }else{assumptions<-NA}
if(sum(is.element(c("all","OutlierRemovalInSD"),output))>0){ outlier<- get.outlier.def(both) }else{outlier<-NA}
if(sum(is.element(c("all","InteractionModeratorMediatorEffect"),output))>0){ InteractionModeratorMediatorEffect<-has.interaction(both) }else{InteractionModeratorMediatorEffect<-NA}
if(sum(is.element(c("all","test.direction","standardStats"),output))>0){ test.direction<-get.test.direction(both) }else{test.direction<-NA}
if(sum(is.element(c("all","software"),output))>0){ software<-get.software(both,add.software=add.software) }else{software<-NA}
if(sum(is.element(c("all","Rpackage"),output))>0){ Rpackage<-get.R.package(both,update.package.list=update.package.list) }else{Rpackage<-NA}

if(sum(is.element(c("all","stats"),output))>0){
   if(text.mode==1) stats<-c("\u2022 Results in abstract and full text:",allStats(c(abstract,fulltext)))
   if(text.mode==2) stats<-c("\u2022 Results in methods and results sections:",allStats(both))
   if(text.mode==3) stats<-c("\u2022 Results in results section/s:",allStats(result))
# add results from captions
if(captions==TRUE) stats<-c(stats,"\u2022 Results in captions:",allStats(caps))   
   
# get standard stats if output is desired
if(sum(is.element(c("all","standardStats"),output))>0){
# set direction to "both" for alternative if has one sided hypotheses/tests
if(length(grep("^one ",test.direction))>0){
   direction<-"both"}else{direction<-"undirected"}
   standardStats<-standardStats(stats,stats.mode=stats.mode,recalculate.p=recalculate.p,alternative=direction,T2t=T2t,R2r=R2r,estimateZ=estimateZ,rm.na.col=rm.na.col,select=selectStandardStats)
}else standardStats<-NA
}else{
 stats<-NA;standardStats<-NA}
 
 ifelse(sum(is.element(c("all","sig.adjectives"),output))>0,
 sig.adjectives<-get.sig.adjectives(both) ,
 # else
 sig.adjectives<-NA)
 
# SAMPLE SIZE
# if(sum(is.element(c("all","sample.size"),output))>0){
# sample.size<-est.ss(abstract=abstract,text=both,quantileDF=quantileDF,max.only=N.max.only)
# }else sample.size<-NA

## output
res<-list(
          doi=doi,
          title=title,
          year=year,
          n.studies=n.studies,
          methods=methods,
          alpha.error=alpha.error,
          power=power,
          multi.comparison.correction=multi.comp,
          assumptions=assumptions,
          OutlierRemovalInSD=outlier,
          InteractionModeratorMediatorEffect=InteractionModeratorMediatorEffect,
          test.direction=test.direction,
          sig.adjectives=sig.adjectives,
          software=software,
          Rpackage=Rpackage,
          stats=stats,
          standardStats=standardStats#,
 #         estimated.sample.size=sample.size
          )

if(nostat==TRUE)  output<-output[output!="stats"]
# reduce output to list of desired outputs
if(!is.element("all",output)) res<-res[is.element(names(res),output)]


 return(res)
}

