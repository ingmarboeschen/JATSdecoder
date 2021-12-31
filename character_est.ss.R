#' est.ss
#'
#' Function to estimate studies sample size by maximizing different conservative estimates.
#' Performs 4 different heuristical searches for sample size in abstract, text, stats and standardStats.
#' 
#' Sample size extraction from abstract:\cr
#' - Extracts N= from abstract text and performs POS search with list of synonyms of sample units
#' 
#' Sample size extraction from text:\cr
#' - Unifies and extracts textlines with age descriptions, than computes sum of hits as nage
#' - Unifies and extracts all "numeric male-female" patterns than computes sum of first male/female hit
#' - Unifies and extracts textlines with participant description than computes sum of first three hits as ntext
#' 
#' Sample size extraction from statistical results:\cr
#' - Extracts "N=" in statistical results extracted with get.stats() that contain p-value: e.g.: chi(2, N=12)=15.2, p<.05
#' 
#' Sample size extraction with result of standardStats(get.stats()):\cr
#' - Extracts df1 and df2 if possible and neither containing a ".", than calculates quantile of (df1+1)+(df2+2) (at least 2 group comparison assumed)
#'
#' @param abstract abstracts text
#' @param text main text to process (usually method and result sections). If text has content, arguments "stats" and "standardStats" are deactivated and filled with results by get.stats(text)
#' @param stats statistics extracted with get.stats(x)$stats (only activ if no text is submitted)
#' @param standardStats standard statistics extracted with get.stats(x)$standardStats  (only activ if no text is submitted)
#' @param quantileDF quantile of (df1-1)+(df2+2) to extract
#' @param max.only Logical. If TRUE only the final estimate will be returned, if FALSE all sub estimates are returned as well
#' @param max.parts Logical. If FALSE outputs all captured sample sizes in sub inputs
#' @importFrom utils head
#' @importFrom stats quantile
#' @importFrom NLP as.String
#' @importFrom NLP Annotation
#' @importFrom NLP annotate
#' @importFrom openNLP Maxent_Word_Token_Annotator
#' @export
#' @examples
#'  a<-"One hundred twelve students participated in our study."
#'  x<-"Our sample consists of three hundred twenty five undergraduate students.
#'      For one sub group the F-test indicates significant differences in means F(2,102)=3.21, p<.05."
#'  est.ss(standardStats=get.stats(x)$standardStats,stats=get.stats(x)$stats)

###############################################################
## Function to combine results and find max of extracted N's
##########################################################
#est.ssOld<-function(abstract=NULL,text=NULL,quantileDF=.9,max.only=FALSE,max.parts=TRUE){
est.ss<-function(abstract=NULL,text=NULL,stats=NULL,standardStats=NULL,quantileDF=.9,max.only=FALSE,max.parts=TRUE){
   # estimate from text different sources
   suppressWarnings({if(length(abstract)>0) a<-est.ss.abstract(abstract,max.only=max.parts) else a<-NA})
   # get SS from stats and standardStats
   suppressWarnings({
      if(length(text)>0){
         text[is.na(text)]<-""
         text<-unlist(text)
         t<-NA
#         t<-est.ss.text(text,max.only=max.parts)
         stats<-get.stats(unlist(text))
         standardStats<-stats$standardStats
         stats<-stats$stats
      } else{t<-NA}
   })
   
   
   suppressWarnings({if(length(stats)>0) s<-est.ss.stats(stats,max.only=max.parts) else s<-NA})
   suppressWarnings({if(length(standardStats)>0) ss<-ceiling(est.ss.standardStats(standardStats,quantileDF=quantileDF,quantile.only=max.parts)) else ss<-NA})
   # find max of estimates
   max<-max(suppressWarnings(as.numeric(unlist(c(SSabstract=a[length(a)],SStext=t[length(t)],SSstats=s[length(s)],SSdf1df2=ss[length(ss)])))),na.rm=T,warnings=F)
   # set max<=2 to NA
   if(max<=2) max<-NA
   
   # combine to output object
   #if(max.parts==TRUE) out<-unlist(c(SSabstract=unname(a[length(a)]),SStext=unname(t[length(t)]),SSstats=unname(s[length(s)]),SSdf1df2=unname(ss[length(ss)])))
   #if(max.parts==FALSE) out<-unlist(c(SSabstract=a,SStext=t,SSstats=s,SSdf1df2=ss))
   
   # output of estimate/s
   if(max.only==TRUE) return(c(estimated.sample.size=max))
   # ,SStext=t
   if(max.only==FALSE){
      out<-unlist(c(SSabstract=a,SSstats=s,SSdf1df2=ss,estimated.sample.size=max))
      if(max.parts==T) names(out)[1]<-"SSabstract"
      return(out)
   }
}


#######################################################
## Function to etimate sum(N) from first line with N=
# used inside est.ss.abstract and est.ss.stats
#####################################################
est.ss.n<-function(x){
   x<-unlist(x)
   # unify "="
   n<-gsub(" [=] |[=] | [=]","=",x)
   # reduce to lines with n=
   n<-get.sentence.with.pattern(n,"[^a-z]n=[1-9]")
   # correct 1,000 to 1000
   for(i in 1:2) n<-gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",n)
   # remove NA
   n<-n[!is.na(n)]
   n<-(n[length(n)>0])
   # get first sentence
   n<-n[1]
   # split before n=
   n<-unlist(strsplit2(n,"[Nn]=[0-9]*","before"))
   # remove first line
   n<-n[-1]
   # remove first 2 characters
   n<-gsub("^..","",n)
   # remove from first non number
   n<-gsub("[^0-9].*","",n)
   # sum up Ns
   n<-sum(as.numeric(n,warn=F),na.rm=T,warn=F)
   if(length(n)==0|n==0) n<-NA
   return(c(Nsum=n))
}

##############################################
## Function to etimate N from abstracts text
##########################################
est.ss.abstract<-function(abstract,max.only=FALSE){
   abstract<-unlist(abstract)
   # remove only text in brackets
   abstract<-gsub(" \\([a-zA-Z]*\\)","",abstract)
   # remove short words with -
   abstract<-gsub(" [a-z]{2}-([a-z])| [a-z]{3}-([a-z])"," \\1\\2",abstract)
   # big numbers without comma but space to number without space
   abstract<-gsub("([0-9]) ([0-9]{3}[^0-9])","\\1\\2",abstract)
   # correct 1,000 and 1.000 to 1000 twice
   for(i in 1:2) abstract<-gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",abstract)
   for(i in 1:2) abstract<-gsub("([0-9])\\.([0-9][0-9][0-9])","\\1\\2",abstract)
   
   # get estimate from lines with N=
   n<-est.ss.n(abstract)
   b<-unlist(abstract)
   
   # numerize textual numbers
   b<-unlist(lapply(tolower(paste0(" ",b)),text2num))
   
   # remove experiment/study + num
   b<-gsub("[Ee]xperiment [1-9]( and [1-9])?|[Ee]xperiments [1-9] and [1-9]","",b)
   b<-gsub("[Ss]tudy [1-9]( and ([Ss]tudy )?[1-9])?|[Ss]tudies [1-9] and [1-9]","",b)
   b<-gsub("[1-9] ([a-z]* )?[Ee]xperiments","",b)
   b<-gsub("[1-9] ([a-z]* )?[Ss]tudies","",b)
   b<-gsub("[Tt]ime [1-9]","",b)
   
   # remove number/s in front of year, month, etc
   pat<-"[0-9]*( year| month| week| day| sequence| face| group| sets| times| pictures)"
   b<-gsub(pat,"",b)
   b<-gsub("act of [0-9][0-9]*","",b)
   
   # remove numbers with .
   b<-gsub("[0-9]*\\.[0-9]*","",b)
   # remove '
   b<-gsub("'","",b)
   
   # get lines with numbers
   b<-grep("[1-9]",b,value=TRUE)
   
   # go on if something's left
   if(length(b)>0){
      # extract N from lines with sample and number in front
      sampleOf<-gsub(".*cohort of ([0-9]*).*|.*sample [a-z]* *of ([0-9]*).*|.*([0-9]*) samples.*","\\1\\2\\3",grep("sample|cohort",b,value=T)[1])
      if(length(sampleOf)>0) sampleOf<-suppressWarnings(as.numeric(sampleOf)) else sampleOf<-NA
      
      # add "twins" to monozygotic/dizygotic
      b<-gsub("  "," ",gsub("([0-9] (monozygotic|dizygotic)) *(^twin|twins)*","\\1 twins ",b))
      # male synonyms
      b<-gsub("[( ]boys[;, )]|[( ]man[;, )]|[( ]fathers[;, )]|[( ]males[;, )]|[( ]male[;, )]"," men ",b)
      # female synonyms
      b<-gsub("[( ]girls[;, )]|[( ]woman[;, )]|[( ]females[;, )]|[( ]female[;, )]|[( ]mothers[;, )]"," women ",b)
      # children synonyms
      b<-gsub(" kids| infants| toddler| teenager| youths| youth| adolescents| preschoolers"," children",b)
      # adults synonyms
      b<-gsub(" caregivers| parents"," adults",b)
      # participants synonyms
      b<-gsub(" respondents| responders"," participants",b)
      # students synonyms
      b<-gsub(" undergraduates"," students",b)
      
      # control synonyms
      b<-gsub(" non[ -]patients|healthy participants| healthy| waiting list| controls| matched"," control",b)
      b<-gsub("([0-9] control )[a-z]*","\\1",b)
      
      # people synonyms
      b<-gsub(" individuals| citizens| subjects| employees| workers| members| teachers| volunteers| natives| native| bilinguar"," people",b)
      b<-gsub(" users| managers| CEOs| members| founders| subordinates| entrepreneurs| directors| officers| nurses| smokers"," people",b)
      b<-gsub(" incumbents| lawyers| representatives| cases| experts| persons"," people",b)
      
      # reports synonyms
      b<-gsub("-structured","structured",b)
      b<-gsub(" questionaires| interviews| observations| acquisitions*"," reports",b)
      # animals synonym
      b<-gsub(" monkeys| apes| rats| birds| pigeons| rabbits"," animals",b)
      # couples synonyms
      b<-gsub(" twins| twin| pairs| dyads| partners| siblings| sibling"," couples",b)
      # firms synonyms
      b<-gsub(" companies| ventures| establishments"," firms",b)
      # teams synonyms
      b<-gsub(" families| alliances"," teams",b)
      
      # delete text between number and people
      b<-gsub(" (and|,;)[^0-9]*(control|men|women|couples|patients|children|animals|participants|people|firms|animals|teams|reports|students|adults)","",b)
      b<-gsub("([0-9] )[^,;0-9]* (control|men|women|couples|patients|children|animals|participants|people|firms|animals|teams|reports|students|adults)","\\1\\2",b)
      b
      # select lines with patterns
      pattern<-"[^a-z]men |[^a-z]women| patients| students| children| control| participants| adults| adult| people| animals| reports| couples| firms| teams"
      b<-grep(pattern,b,value=TRUE)
      # select first line with patterns?
      #b<-grep(pattern,b,value=TRUE)[1]
      
      # create empty result objects
      male<-NA; female<-NA; patients<-NA; students<-NA; children<-NA; control<-NA; participants<-NA; adults<-NA; people<-NA; reports<-NA; animals<-NA; couples<-NA; firms<-NA; teams<-NA
      
      # only go on if something left
      if(length(b)>0|!is.na(sampleOf)){
         # simplify and unify
         b<-gsub("[\\.\\?]$","",b)
         b<-unlist(strsplit(b,"\\:|\\[|\\]|\\(|\\)| of "))
         # remove [0-9]...old
         b<-gsub("[0-9]*-[, ].* old","",b)
         # remove brackets
         b<-gsub("\\[|\\]|\\(|\\)","",b)
         b<-grep(pattern,b,value=TRUE)
         # convert "[0-9] [A-Z]"->"[0-9] [a-z]"
         # if has "[0-9] [A-Z]" get first and lowerize capital letter
         while(sum(nchar(gsub(".*[0-9] ([A-Z]).*",tolower("\\1"),b))==1)>0){
            i<-grep("[0-9] ([A-Z])",b)[1]
            l<- tolower(gsub(".*[0-9] ([A-Z]).*",tolower("\\1"),b[i]))
            # convert capital to lowerized letter
            b[i]<- gsub("([0-9]) ([A-Z])",paste("\\1",l),b[i])
         }
         
         # remove "-" in case of no "ty-number"
         b<-gsub("([^t][^y])-([a-z]{6})","\\1\\2",b)
         # select lines with pattern
         b<-grep(pattern,b,value=TRUE)
         # only go on if something left
         if(length(b)>0|!is.na(sampleOf)){
            if(length(b)>0){
               # get first 5 lines
               b<-utils::head(b,5)
               # extract N male and N female
               male<-gsub(".* ([0-9]*?) men[^a-z].*|^([0-9]*?) men[^a-z].*|.* ([0-9]*?) men$||^([0-9]*?) men$","\\1\\2\\3\\4",grep("[0-9] men[^a-z]|[0-9] men$",b,value=TRUE)[1])
               female<-gsub(".* ([0-9]*?) women[^a-z].*|^([0-9]*?) women[^a-z].*|.* ([0-9]*?) women$|^([0-9]*?) women$","\\1\\2\\3\\4",grep("[0-9] women",b,value=TRUE)[1])
               # get POS-tags
               b<-get.POStagged(b,"tagged")
               # remove coma at listing of subjectives
               b<-gsub("(/JJ) ,/,( [a-z\\-]*?/JJ)","\\1\\2",b)
               # remove and at listing of subjectives
               b<-gsub("(/JJ) and/CC( [a-z\\-]*?/JJ)","\\1\\2",b)
               
               # correct numbers/VBG or /RP to number/CD
               b<-gsub("([0-9]/)VBG","\\1CD",b)
               b<-gsub("([0-9]/)RP","\\1CD",b)
               b<-gsub("([0-9]/)NNS","\\1CD",b)
               b<-gsub("([0-9]/)JJ","\\1CD",b)
               
               # convert listing of adjectives with numbers to number of "people"
               b<-gsub("([0-9]*/CD [a-z]*/JJ) and/CC ([0-9]*/CD)( [a-z]*/JJ)? (couples|patients|animals|participants|people|firms|animals|controls|teams|reports|students|adults)","\\1 \\4/NN and/CC \\2\\3 \\4",b)
               # OLD #b<-gsub("([0-9]*/CD [a-z]*/JJ) and/CC ([0-9]*/CD [a-z]*/JJ) ","\\1 people/NN and/CC \\2 people/NN ",b)
               
               # split at split words
               #unlist(strsplit(b," and/CC| to/TO|[a-z]*?/DT| ,/,| ;/:|[a-z]*?/IN|[a-z]*?/WP|[a-z]*?/PRP|[a-z]*?/VBD|[a-z]*?/VBN|[a-z]*?/VBP|[a-z]*?/VBG|[a-z]*?/MD"))
               b<-grep(pattern,grep("[1-9]",unlist(strsplit(b," and/CC| to/TO|[a-z]*?/DT| ,/,| ;/:|[a-z]*?/IN|[a-z]*?/WP|[a-z]*?/PRP|[a-z]*?/VBD|[a-z]*?/VBN|[a-z]*?/VBP|[a-z]*?/VBG|[a-z]*?/MD")),value=TRUE),value=TRUE)
               # remove text behind /NN
               b<-gsub("(/NN).*","\\1",b)
               # remove lines with %
               b<-grep("\\%",b,value=TRUE,invert=TRUE)
               
               # remove text til "num/CD"
               b<-sub(".* ([0-9].*/CD)","\\1",b)
               
               # get first line of patterns and select first number/CD
               if(length(grep(" children/",b))<=1)
                  children<-grep("/CD",unlist(strsplit(grep(" children/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" control/",b))<=1)
                  control<-grep("/CD",unlist(strsplit(grep(" control/|controls/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" participants/",b))<=1)
                  participants<-grep("/CD",unlist(strsplit(grep(" participants/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" people/",b))<=1)
                  people<-grep("/CD",unlist(strsplit(grep(" people/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" adults/",b))<=1)
                  adults<-grep("/CD",unlist(strsplit(grep(" adults/| adult/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" patients/",b))<=1)
                  patients<-grep("/CD",unlist(strsplit(grep(" patients/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" students/",b))<=1)
                  students<-grep("/CD",unlist(strsplit(grep(" students/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" couples/",b))<=1)
                  couples<-grep("/CD|/JJ",unlist(strsplit(grep(" couples/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" reports/",b))<=1)
                  reports<-grep("/CD",unlist(strsplit(grep(" reports/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" animals/",b))<=1)
                  animals<-grep("/CD",unlist(strsplit(grep(" animals/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" firms/",b))<=1)
                  firms<-grep("/CD",unlist(strsplit(grep(" firms/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               if(length(grep(" teams/",b))<=1)
                  teams<-grep("/CD",unlist(strsplit(grep(" teams/",b,value=TRUE)[1]," ")),value=TRUE)[1]
               
               # get sum of captures
               if(length(grep(" children/",b))>1)
                  children<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("children",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" control/",b))>1)
                  control<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("control",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" participants/",b))>1)
                  participants<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("participants",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" people/",b))>1)
                  people<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("people",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" adults/",b))>1)
                  adults<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("adults",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" patients/",b))>1)
                  patients<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("patients",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" students/",b))>1)
                  students<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("students",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" couples/",b))>1)
                  couples<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("couples",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" reports/",b))>1)
                  reports<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("reports",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" animals/",b))>1)
                  animals<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("animals",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" firms/",b))>1)
                  firms<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("firms",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               if(length(grep(" teams/",b))>1)
                  teams<-sum(as.numeric(unlist(lapply(lapply(b,function(x) gsub("/[A-Z][A-Z].*","",grep("teams",x,value=TRUE))),"[",1)),warn=F),na.rm=T)
               
               
               # remove POS
               children<-paste(unlist(lapply(children,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               participants<-paste(unlist(lapply(participants,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               control<-paste(unlist(lapply(control,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               patients<-paste(unlist(lapply(patients,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               people<-paste(unlist(lapply(people,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               students<-paste(unlist(lapply(students,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               adults<-paste(unlist(lapply(adults,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               reports<-paste(unlist(lapply(reports,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               animals<-paste(unlist(lapply(animals,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               couples<-paste(unlist(lapply(couples,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               firms<-paste(unlist(lapply(firms,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               teams<-paste(unlist(lapply(teams,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
               
               
               # multiply extracted number of couples by two
               if(couples!="NA") couples<-2*sum(as.numeric(unlist(lapply(lapply(couples,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)
            }
            
            # create result vector
            res<-c(male=male,female=female,patients=patients,students=students,children=children,control=control,
                   participants=participants,adults=adults,people=people,reports=reports,animals=animals,couples=couples,firms=firms,teams=teams,sampleOf=sampleOf,n)
            # set lines with year, month, etc to NA
            pat<-" year| month| week| day| sequence| face| group| sets"
            if(length(grep(pat,res))>0) res[grep(pat,res)]<-"NA"
            
            # if length of pattern match was 0
         }else res<-c(male=NA,female=NA,patients=NA,students=NA,children=NA,control=NA,
                      participants=NA,adults=NA,people=NA,reports=NA,animals=NA,couples=NA,firms=NA,teams=NA,sampleOf=NA,n)
         # if length of input was 0
      }else res<-c(male=NA,female=NA,patients=NA,students=NA,children=NA,control=NA,
                   participants=NA,adults=NA,people=NA,reports=NA,animals=NA,couples=NA,firms=NA,teams=NA,sampleOf=NA,n)
      # if no number was detected
   }else res<-c(male=NA,female=NA,patients=NA,students=NA,children=NA,control=NA,
                participants=NA,adults=NA,people=NA,reports=NA,animals=NA,couples=NA,firms=NA,teams=NA,sampleOf=NA,n)
   
   
   # convert bad captures to NA
   res[which(res=="0")]<-NA
   # get max of captures with sum(male,female), sum(patients,control), sum(child, adult)
   num<-as.numeric(gsub("NA",NA,gsub(" .*","",res)))
   if(is.na(res["control"])|res["control"]=="NA")
      num<-max(c(sum(num[1:2],na.rm=T,warn=F),sum(num[c(3,6)],na.rm=T,warn=F),sum(num[c(5,8)],na.rm=T,warn=F),max(num[3:17],na.rm=T,warn=F)),na.rm=T,warn=F)
   if(!is.na(res["control"])&res["control"]!="NA")
      num<-max(c(sum(num[1:2],na.rm=T,warn=F),sum(num[3:14],na.rm=T,warn=F),num[length(num)]),na.rm=T,warn=F)
   
   if(num<=2) num<-NA
   gc()
   if(max.only==FALSE){
      res<-(c(res,maxSampleSize=num))
      res[res=="NA"]<-NA
      na<-names(res)
      res<-as.numeric(res)
      names(res)<-na
      res[which(res<=2)]<-NA
   }
   
   if(max.only==TRUE) res<-c(SSabstract=as.numeric(num))
   return(res)
}

###################################
## Function to etimate N from text
#################################
est.ss.text<-function(text,max.only=F){
   b<-unlist(text)
   # remove lines with citations
   b<-grep("[0-9]{4}\\[a-z]*)| et\\.? al[^a-z]| in press",b,value=TRUE,invert=TRUE)
   # remove lines with figure, table
   b<-grep("[Ff]igures* |^[Tt]ables* | [Tt]ables* |[Mm]odels* |[Hh]ypothes[ie]s ",b,value=TRUE,invert=TRUE)
   

   ## Synonyms
   # add "twins" to monozygotic/dizygotic
   b<-gsub("  "," ",gsub("([0-9] (monozygotic|dizygotic)) *(^twin|twins)*","\\1 twins ",b))
   # male synonyms
   b<-gsub("[( ]boys[;, )]|[( ]man[;, )]|[( ]fathers[;, )]|[( ]males[;, )]|[( ]male[;, )]"," men ",b)
   # female synonyms
   b<-gsub("[( ]girls[;, )]|[( ]woman[;, )]|[( ]females[;, )]|[( ]female[;, )]|[( ]mothers[;, )]"," women ",b)
   # children synonyms
   b<-gsub(" kids| infants| toddler| teenager| youths| youth| adolescents| preschoolers"," children",b)
   # adults synonyms
   b<-gsub(" caregivers| parents"," adults",b)
   # participants synonyms
   b<-gsub(" respondents| responders"," participants",b)
   # students synonyms
   b<-gsub(" undergraduates"," students",b)
   
   # control synonyms
   b<-gsub(" non[ -]patients|healthy participants| healthy| waiting list| controls| matched"," control",b)
   b<-gsub("([0-9] control )[a-z]*","\\1",b)
   
   # people synonyms
   b<-gsub(" individuals| citizens| subjects| employees| workers| members| teachers| volunteers| natives| native| bilinguar"," people",b)
   b<-gsub(" users| managers| CEOs| members| founders| subordinates| entrepreneurs| directors| officers| nurses| smokers"," people",b)
   b<-gsub(" incumbents| lawyers| representatives| cases| experts| persons"," people",b)
   
   # reports synonyms
   b<-gsub("-structured","structured",b)
   b<-gsub(" questionaires| interviews| observations| acquisitions*"," reports",b)
   # animals synonym
   b<-gsub(" monkeys| apes| rats| birds| pigeons| rabbits"," animals",b)
   # couples synonyms
   b<-gsub(" twins| twin| pairs| dyads| partners| siblings| sibling"," couples",b)
   # firms synonyms
   b<-gsub(" companies| ventures| establishments"," firms",b)
   # teams synonyms
   b<-gsub(" families| alliances"," teams",b)
   
   # select lines with patterns
   pattern<-"[^a-z]men |[^a-z]women| patients| students| children| control| participants| adults| adult| people| animals| reports| couples| firms| teams"
   b<-grep(pattern,b,value=TRUE)
   
   
   # remove number/s in front of year, month, etc
   pat<-"[0-9][0-9]*([ -]year|[ -]month|[ -]week|[ -]day|[ -]sequence|[ -]face|[ -]group|[ -]sets|[ -]times|[ -]pictures)"
   b<-gsub(pat,"",b)
   # remove numbers with . or -
   b<-gsub("[0-9]*[\\.-][0-9]*","",b)
   # remove '
   b<-gsub("'","",b)
   # remove numbers in brackets
   b<-gsub("(\\(|\\[)[0-9, ;&\\-]*(\\)|\\])","",b)
   # remove number-letter combinations
   b<-gsub("[0-9][0-9]*[a-z][a-z]*|[a-z][a-z]*[0-9][0-9]*","",b)
   # remove citations
   b<-gsub("(\\(|\\[).*, [0-9]{4}[a-z]?(\\]|\\))","",b)
   # remove '
   b<-gsub("'"," ",b)
   b<-gsub("  *"," ",b)
   
   # convert textual numbers
   b<-text2num(b)
   # remove 1 and 2
   b<-gsub("^(1|2) ([^0-9])| (1|2) ([^0-9])","\\1 \\2",b)
   
   # select lines with numbers
   b<-grep("[0-9]",b,value=TRUE)
   b
   # remove experiment/study + num
   b<-gsub("[Ee]xperiment [1-9]( and [1-9])?|[Ee]xperiments [1-9] and [1-9]"," ",b)
   b<-gsub("[Ss]tudy [1-9]( and ([Ss]tudy )?[1-9])?|[Ss]tudies [1-9] and [1-9]"," ",b)
   b<-gsub("[1-9] ([a-z]* )?[Ee]xperiments"," ",b)
   b<-gsub("[1-9] ([a-z]* )?[Ss]tudies"," ",b)
   b<-gsub("[Tt]ime [1-9]"," ",b)
   
   # delete listing of numbers
   b<-gsub("[0-9][0-9]*, [0-9][0-9]*","",b)
   # correct 1,000 and 1 000 to 1000 twice
   for(i in 1:2) b<- gsub("([0-9]) ([0-9][0-9][0-9])","\\1\\2",b)
   for(i in 1:2) b<- gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",b)

   # select lines with numbers
   b<-grep("[0-9]",b,value=TRUE)
   
   # remove lines with patterns
     b<-grep("taken out| missing|decline|remove|excluded|refuse| drop| not/|were not| sets| ended|cancel|abort",b,value=TRUE,invert=TRUE)
   b
   # delete text between number and synonyms
   #b<-gsub(" (and|,;)[^0-9]*(control|men|women|couples|patients|children|animals|participants|people|firms|animals|teams|reports|students|adults)","",b)
   b<-gsub("([0-9] )[^,;0-9]* (control|men|women|couples|patients|children|animals|participants|people|firms|animals|teams|reports|students|adults)","\\1\\2",b)
   
   # select lines with number followed by synonym
   b<-grep("[0-9] (control|men|women|couples|patients|children|animals|participants|people|firms|animals|teams|reports|students|adults)",b,value=TRUE)
   b
   
   ################
   
      # only go on if something left
      if(length(b)>0){
         # select first 10 lines
         b<-utils::head(b,10)
         # extract N male and N female
         male<-max(as.numeric(gsub(".* ([0-9]*?) men[^a-z].*|^([0-9]*?) men[^a-z].*","\\1\\2",grep("[0-9] men[^a-z]",b,value=TRUE))),warn=F)
         female<- max(as.numeric(gsub(".* ([0-9]*?) women[^a-z].*|([0-9]*?) women[^a-z].*","\\1\\2",grep("[0-9] women",b,value=TRUE))),warn=F)
         # add space in front
         b<-paste0(" ",b)
         # get first line of patterns and select number
         children<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) children.*","\\2",grep("[0-9] children",b,value=T))),warn=F)
         control<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) control.*","\\2",grep("[0-9] control",b,value=T))),warn=F)
         participants<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) participants.*","\\2",grep("[0-9] participants",b,value=T))),warn=F)
         people<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) people.*","\\2",grep("[0-9] people",b,value=T))),warn=F)
         adults<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) adults.*","\\2",grep("[0-9] adults",b,value=T))),warn=F)
         patients<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) patients.*","\\2",grep("[0-9] patients",b,value=T))),warn=F)
         
         students<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) students.*","\\2",grep("[0-9] students",b,value=T))),warn=F)
         couples<-2*max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) couples.*","\\2",grep("[0-9] couples",b,value=T))),warn=F)
         reports<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) reports.*","\\2",grep("[0-9] reports",b,value=T))),warn=F)
         animals<-max(as.numeric(gsub("(.*|^) ([0-9][0-9]*) animals.*","\\2",grep("[0-9] animals",b,value=T))),warn=F)
         
         # create result vector
         res<-c(male=male,female=female,patients=patients,students=students,children=children,control=control,
                participants=participants,adults=adults,people=people,reports=reports,animals=animals,couples=couples)
         res
      # if length of pattern match was 0
      }else res<-c(male=NA,female=NA,patients=NA,students=NA,children=NA,control=NA,
                   participants=NA,adults=NA,people=NA,reports=NA,animals=NA,couples=NA)
   
   # convert bad captures to NA
   res[which(res<=2)]<-NA
   # get max of captures with sum(male,female), sum(patients,control), sum(child, adult)
   num<-as.numeric(gsub("NA",NA,gsub(" .*","",res)),warn=FALSE)
   num<-max(c(sum(num[1:2],na.rm=T,warn=F),sum(num[c(3,6)],na.rm=T,warn=F),sum(num[c(5,8)],na.rm=T,warn=F),max(num[3:13],na.rm=T,warn=F)),na.rm=T,warn=F)
   if(num==0) num<-NA
   gc()
   if(max.only==F) res<-c(res,maxSStext=num)
   if(max.only==T) res<-c(SStext=num)
   suppressWarnings(res<-as.numeric(res))
   return(res)
}

######################################################
## Function to etimate N from N= in lines with p<=>)
##################################################
est.ss.stats<-function(stats,max.only=F){
   # find max of N= in stats
   if(length(stats)>0){
      # select lines with p value
      Nstat<-grep("p[<=>]",stats,value=TRUE)
      # and N=
      Nstat<-grep("^[nN]=|[^A-Za-z][nN]=",Nstat,value=TRUE)
      # go on if something left
      if(length(Nstat)>0){
         # remove till N=
         Nstat<-gsub("^[nN]=|.*[^A-Za-z][nN]=","",Nstat)
         # remove from first non number
         Nstat<-gsub("[^0-9].*","",Nstat)
         # set to numeric
         Nstat<-as.numeric(unlist(Nstat))
         # only select lines with content
         Nstat<-Nstat[nchar(Nstat)>0]
         if(length(Nstat)==0) Nstat<-NA
         # get max of Ns
         NstatMax<-max(Nstat,na.rm=T,warn=F)
         # set output for bad captures
         if(NstatMax==-Inf|NstatMax==0) NstatMax<-NA
      }else{
         Nstat<-NA
         NstatMax<-NA
      }
   }else{
      Nstat<-NA
      NstatMax<-NA
   }
   # prepare result as list with detected N's and/or max of detected N's
   if(max.only==F) resStats<-list(Nstat=Nstat,statMaxN=NstatMax)
   if(max.only==T) resStats<-c(statMaxN=NstatMax)
   return(resStats)
   
}


##########################################################
## Function to etimate N from stats quantile((df1-1)+(df2+2),quantileDF)
#####################################################
est.ss.standardStats<-function(standardStats,quantileDF=.9,quantile.only=FALSE){
   if(length(standardStats)>0){
      matrix<-standardStats
      # reset matrix names
      if(is.null(dim(matrix))){
         n<-names(matrix)
         matrix<-matrix(matrix,ncol=length(matrix))
         colnames(matrix)<-n
      }
      # remove columns that only have NAs   
      matrix<-matrix[,!colSums(is.na(matrix))==dim(matrix)[1]]
      # reset matrix names
      if(is.null(dim(matrix))){
         n<-names(matrix)
         matrix<-matrix(matrix,ncol=length(matrix))
         colnames(matrix)<-n
      }
      
      # remove corrected df's: set df1 and df2 to NA if one of both contains a "." (correction)
      if(length(grep("df1|df2",colnames(matrix)))==2){ 
         matrix[grep("\\.",paste(factor(matrix[,"df1"]):factor(matrix[,"df2"]))),c("df1","df2")]<-NA
      }
      
      # if only df1 contains "."
      if(length(grep("df1",colnames(matrix)))==1) matrix[grep("\\.",paste(matrix[,"df1"])),"df1"]<-NA
      # if only contains df2
      if(length(grep("df2",colnames(matrix)))==1) matrix[grep("\\.",paste(matrix[,"df2"])),"df2"]<-NA
      
      
      # get df1 values and subtract 1
      if(sum(colnames(matrix)=="df1")==1){
         # convert to numeric -1
         df1<-as.numeric(matrix[,"df1"])-1
      }else df1<-rep(NA,ifelse(is.vector(matrix),1,dim(matrix)[1]))
      # get df2 values and add 2
      if(sum(colnames(matrix)=="df2")==1){
         # convert to numeric+2
         df2<-as.numeric(matrix[,"df2"])+2
         # if df1 is NA set df2 to NA
         df1[is.na(df2)]<-NA
      }else df2<-rep(NA,ifelse(is.vector(matrix),1,dim(matrix)[1]))
      # calculate sum of (df1-1)+(df2+2)
      sumdf<-rowSums(cbind(df1,df2),na.rm=T)
      sumdf<-sumdf[which(sumdf>0)]
      ## To do: remove overly high captures (e.g. Box Sperizity test, in repeated measure design) instead of quantile
      
      # calculate quantile
      quantileDF<-stats::quantile(sumdf,quantileDF,na.rm=T,type=7)
      if(length(grep("[^0-9\\.]",quantileDF))>0) quantileDF<-NA
   }
   else{
      sumdf<-NA;quantileDF<-NA
   }
   # set Zero result to NA if is not NA already
   if(!is.na(quantileDF)) if(quantileDF==0) quantileDF<-NA
   
   if(quantile.only==F) return(c(sumdf,quantileDF=quantileDF))
   if(quantile.only==T) return(c(quantileDF=quantileDF))
   
}

##################################################
get.POStagged <-  function(x, type=c("tagged","tags")[1]) {
   gc()
   tags<-NULL; POStagged<-NULL
   for(i in 1:length(x)){
      a <- NLP::as.String(list(x[i]))
      word_token_annotator <- openNLP::Maxent_Word_Token_Annotator()
      a2 <- NLP::Annotation(1L, "sentence", 1L, nchar(a))
      a2 <- NLP::annotate(a, word_token_annotator, a2)
      a3 <- NLP::annotate(a, openNLP::Maxent_POS_Tag_Annotator(), a2)
      a3w <- a3[a3$type == "word"]
      POStags <- unlist(lapply(a3w$features, `[[`, "POS"))
      POStagged[i] <- paste(sprintf("%s/%s", a[a3w], POStags),collapse = " ")
      tags[i]<-paste(POStags,collapse = " ")
   }
   if(type=="tagged")  return(POStagged)
   if(type=="tags") return(tags)
}
