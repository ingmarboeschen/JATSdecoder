#' est.ss
#'
#' Function to estimate studies sample size by maximizing different conservative estimates.
#' Performs 4 different heuristical searches for sample size in abstract, text, stats and standardStats.
#' 
#' Sample size extraction from abstract:\cr
#' - Extracts N= from abstract if possible and performs POS search with list of synonyms
#' 
#' Sample size extraction from text:\cr
#' - Unifies and extracts textlines with age descriptions, than computes sum of hits as nage
#' - Unifies and extracts all "numeric male-female" patterns than computes sum of first male/female hit
#' - Unifies and extracts textlines with participant description than computes sum of first three hits as ntext
#' 
#' Sample size extraction from statistical results:\cr
#' - Extracts "N=" in statistical result lines from get.stats() that contain p-value
#' 
#' Sample size extraction with result of standardStats(get.stats()):\cr
#' - Extracts df1 and df2 if possible and neither containing a ".", than calculates quantile of (df1+1)+(df2+2) (at least 2 group comparison assumed)
#'
#' @param abstract abstracts text
#' @param text main text to process (usually method section)
#' @param quantileDF quantile of (df1+1)+(df2+2) to extract
#' @param max.only Logical. If TRUE only the final estimate will be returned, if FALSE all sub estimates are returned as well
#' @param max.parts Logical. If FALSE outputs all captured sample sizes in sub inputs
#' @importFrom utils head
#' @importFrom stats quantile
#' @export
#' @examples
#'  a<-"One hundred twelve students participated in our study."
#'  x<-"Our sample consists of three hundred twenty five undergraduate students.
#'      For one sub group the F-test indicates significant differences in means F(2,102)=3.21, p<.05."
#'  est.ss(abstract=a,text=x)

###############################################################
## Function to combine results and find max of extracted N's
##########################################################
est.ss<-function(abstract=NULL,text=NULL,quantileDF=.75,max.only=FALSE,max.parts=TRUE){
# estimate from text different sources
suppressWarnings({if(length(abstract)>0) a<-est.ss.abstract(abstract,max.only=max.parts) else a<-NA})
# get SS from first 10 lines max of text input (POS-tagging leads to overload in JAVA heap space)
suppressWarnings({if(length(text)>0){
          t<-est.ss.text(text,max.only=max.parts)
          stats<-get.stats(text)
          standardStats<-standardStats(stats)
          } else{t<-NA;stats<-NULL;standardStats<-NULL}
          })
suppressWarnings({if(length(stats)>0) s<-est.ss.stats(stats,max.only=max.parts) else s<-NA})
suppressWarnings({if(length(standardStats)>0) ss<-est.ss.standardStats(standardStats,quantileDF=quantileDF,quantile.only=max.parts) else ss<-NA})
# combine
res<-c(SSabstract=a[length(a)],SStext=t[length(t)],SSstats=s[length(s)],SSstandardStats=ss[length(ss)])
# find max of estimates
max<-max(suppressWarnings(as.numeric(res)),na.rm=T,warnings=F)
# output of estimate/s
if(max.only==T) return(c(estimatedSS=max))
if(max.only==F) return(c(SSabstract=a,SStext=t,SSstats=s,SSstandardStats=ss,estimatedSS=max))
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
est.ss.abstract<-function(abstract,max.only=F){
# get estimate from lines with N=
n<-est.ss.n(abstract)
b<-unlist(abstract)
# male synonyms
b<-gsub("[( ]boys[;, )]|[( ]man[;, )]|[( ]fathers[;, )]|[( ]males[;, )]|[( ]male[;, )]"," men ",b)
# female synonyms
b<-gsub("[( ]girls[;, )]|[( ]woman[;, )]|[( ]females[;, )]|[( ]female[;, )]|[( ]mothers[;, )]"," women ",b)
# children synonyms
b<-gsub(" kids| infants| toddler| teenager| youth| adolescents"," children",b)
# adults synonyms
b<-gsub(" caregivers| parents"," adults",b)
# participants synonyms
b<-gsub(" respondents"," participants",b)
# students synonyms
b<-gsub(" undergraduates"," students",b)
# people synonyms
b<-gsub(" individuals| citizens| subjects| employees| workers| teachers| volunteers| natives| native| bilinguar| users"," people",b)
# control synonyms
b<-gsub(" non[ -]patients|healthy participants| healthy| waiting list"," control",b)
# reports synonyms
b<-gsub("-structured","structured",b)
b<-gsub(" questionaires| interviews"," reports",b)
# animals synonym
b<-gsub(" monkeys| apes| rats| birds| pigeons"," animals",b)
# couples synonyms
b<-gsub(" twins| twin| pairs| dyads| partners"," couples",b)

# preselect lines with patterns
pattern<-"[^a-z]men |[^a-z]women| patients| students| children| control| participants| adult| people| animals| reports| couples"
b<-grep(pattern,b,value=TRUE)
# only go on if something left
if(length(b)>0){
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
 
 # correct 1,000 and 1.000 to 1000 twice
 for(i in 1:2) b<- gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",b)
 for(i in 1:2) b<- gsub("([0-9])\\.([0-9][0-9][0-9])","\\1\\2",b)
 # remove experiment/study + num
 b<-gsub("[Ee]xperiment [1-9]|[Ee]xperiments [1-9] and [1-9]","",b)
 b<-gsub("[Ss]tudy [1-9]|[Ss]tudies [1-9] and [1-9]","",b)
 b<-gsub("[Tt]ime [1-9]","",b)
 # remove "-" in case of no "ty-number"
 b<-gsub("([^t][^y])-([a-z]{6})","\\1\\2",b)
  b<-grep(pattern,b,value=TRUE)
 # numerize textual numbers
 b<-unlist(lapply(b,text2num))
 # get lines with numbers and patterns
 b<-grep("[1-9]",b,value=TRUE)
 b<-grep(pattern,b,value=TRUE)
 # only go on if something left
 if(length(b)>0){
   # get first 10 lines
   b<-utils::head(b,10)
   # extract N male and N female
   male<-gsub(".* ([0-9]*?) men[^a-z].*|^([0-9]*?) men[^a-z].*","\\1\\2",grep("[0-9] men[^a-z]",b,value=TRUE)[1])
   female<- gsub(".* ([0-9]*?) women[^a-z].*|([0-9]*?) women[^a-z].*","\\1\\2",grep("[0-9] women",b,value=TRUE)[1])
   # get POS-tags
   b<-get.POStagged(b,"tagged")
   # remove coma at listing of subjectives
   b<-gsub("(/JJ) ,/,( [a-z\\-]*?/JJ)","\\1\\2",b)
   # remove and at listing of subjectives
   b<-gsub("(/JJ) and/CC( [a-z\\-]*?/JJ)","\\1\\2",b)
   # convert listing of adjectives with numbers to number of people
   b<-gsub("([0-9]*/CD [a-z]*/JJ) and/CC ([0-9]*/CD [a-z]*/JJ) ","\\1 people/NN and/CC \\2 people/NN ",b)
   # correct numbers/VBG to number/CD
   b<-gsub("([0-9]/)VBG","\\1CD",b)
   b<-gsub("([0-9]/)RP","\\1CD",b)

   # split at split words
   b<-grep(pattern,grep("[1-9]",unlist(strsplit(b," and/CC| to/TO|[a-z]*?/DT| ,/,| ;/:|[a-z]*?/IN|[a-z]*?/WP|[a-z]*?/PRP|[a-z]*?/VBD|[a-z]*?/VBN|[a-z]*?/VBP|[a-z]*?/VBG|[a-z]*?/MD")),value=TRUE),value=TRUE)
   # remove lines with %
   b<-grep("\\%",b,value=TRUE,invert=TRUE)

   # remove text til "num/CD"
   b<-sub(".* ([0-9].*/CD)","\\1",b)

   # get first line of patterns and select first number/CD
   children<-grep("/CD",unlist(strsplit(grep(" children/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   control<-grep("/CD",unlist(strsplit(grep(" control/|controls/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   participants<-grep("/CD",unlist(strsplit(grep(" participants/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   people<-grep("/CD",unlist(strsplit(grep(" people/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   adults<-grep("/CD",unlist(strsplit(grep(" adults/| adult/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   patients<-grep("/CD",unlist(strsplit(grep(" patients/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   students<-grep("/CD",unlist(strsplit(grep(" students/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   couples<-grep("/CD",unlist(strsplit(grep(" couples/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   reports<-grep("/CD",unlist(strsplit(grep(" reports/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   animals<-grep("/CD",unlist(strsplit(grep(" animals/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   
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

   # get sum of captures
#   children<-sum(as.numeric(unlist(lapply(lapply(children,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)
#   control<-sum(as.numeric(unlist(lapply(lapply(control,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)
#   participants<-sum(as.numeric(unlist(lapply(lapply(participants,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)
#   animals<-sum(as.numeric(unlist(lapply(lapply(animals,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)
   couples<-2*sum(as.numeric(unlist(lapply(lapply(couples,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)
#   people<-sum(as.numeric(unlist(lapply(lapply(people,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)

  # create result vector
   res<-c(male=male,female=female,patients=patients,students=students,children=children,control=control,
          participants=participants,adults=adults,people=people,reports=reports,animals=animals,couples=couples,n)
   # set lines with year, month, etc to NA
   pat<-" year| month| week| day| sequence| face| group| sets"
   if(length(grep(pat,res))>0) res[grep(pat,res)]<-"NA"
# if length of pattern match was 0
 }else res<-c(male=NA,female=NA,patients=NA,students=NA,children=NA,control=NA,
       participants=NA,adults=NA,people=NA,reports=NA,animals=NA,couples=NA,n)
# if length of input was 0
}else res<-c(male=NA,female=NA,patients=NA,students=NA,children=NA,control=NA,
       participants=NA,adults=NA,people=NA,reports=NA,animals=NA,couples=NA,n)

# convert bad captures to NA
res[which(res=="0")]<-NA
# get max of captures with sum(male,female), sum(patients,control), sum(child, adult)
num<-as.numeric(gsub("NA",NA,gsub(" .*","",res)))
num<-max(c(sum(num[1:2],na.rm=T,warn=F),sum(num[c(3,6)],na.rm=T,warn=F),sum(num[c(5,8)],na.rm=T,warn=F),max(num[3:14],na.rm=T,warn=F)),na.rm=T,warn=F)
if(num==0) num<-NA
gc()
if(max.only==F) res<-(c(res,maxSS=num))
if(max.only==T) res<-c(SSabstract=as.numeric(num))
return(res)
}

###################################
## Function to etimate N from text
#################################
est.ss.text<-function(text,max.only=F){
b<-unlist(text)
# male synonyms
b<-gsub("[( ]boys[;, )]|[( ]man[;, )]|[( ]fathers[;, )]|[( ]males[;, )]|[( ]male[;, )]"," men ",b)
# female synonyms
b<-gsub("[( ]girls[;, )]|[( ]woman[;, )]|[( ]females[;, )]|[( ]female[;, )]|[( ]mothers[;, )]"," women ",b)
# children synonyms
b<-gsub(" kids| infants| toddler| teenager| youth| adolescents"," children",b)
# adults synonyms
b<-gsub(" caregivers| parents"," adults",b)
# participants synonyms
b<-gsub(" respondents"," participants",b)
# students synonyms
b<-gsub(" undergraduates"," students",b)
# people synonyms
b<-gsub(" individuals| citizens| subjects| employees| workers| teachers| volunteers| natives| native| bilinguar| users"," people",b)
# control synonyms
b<-gsub(" non[ -]patients|healthy participants| healthy| waiting list"," control",b)
# reports synonyms
b<-gsub("-structured","structured",b)
b<-gsub(" questionaires| interviews"," reports",b)
# animals synonym
b<-gsub(" monkeys| apes| rats| birds| pigeons"," animals",b)
# couples synonyms
b<-gsub(" twins| twin| pairs| dyads| partners"," couples",b)

# preselect lines with patterns
pattern<-"[^a-z]men |[^a-z]women| patients| students| children| control| participants| adult| people| animals| reports| couples"
b<-grep(pattern,b,value=TRUE)
# only go on if something left
if(length(b)>0){
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
 # correct 1,000 and 1.000 to 1000 twice
 for(i in 1:2) b<- gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",b)
 for(i in 1:2) b<- gsub("([0-9])\\.([0-9][0-9][0-9])","\\1\\2",b)
 # remove experiment/study + num
 b<-gsub("[Ee]xperiment [1-9]|[Ee]xperiments [1-9] and [1-9]","",b)
 b<-gsub("[Ss]tudy [1-9]|[Ss]tudies [1-9] and [1-9]","",b)
 b<-gsub("[Tt]ime [1-9]","",b)
 # remove "-" in case of no "ty-number"
 b<-gsub("([^t][^y])-([a-z]{6})","\\1\\2",b)
 b<-grep(pattern,b,value=TRUE)
 # numerize textual numbers
 b<-unlist(lapply(b,text2num))
 # get lines with numbers and patterns
 b<-grep("[1-9]",b,value=TRUE)
 b<-grep(pattern,b,value=TRUE)

 # only go on if something left
 if(length(b)>0){
 # select first 10 lines
   b<-utils::head(b,10)
   # extract N male and N female
   male<-gsub(".* ([0-9]*?) men[^a-z].*|^([0-9]*?) men[^a-z].*","\\1\\2",grep("[0-9] men[^a-z]",b,value=TRUE)[1])
   female<- gsub(".* ([0-9]*?) women[^a-z].*|([0-9]*?) women[^a-z].*","\\1\\2",grep("[0-9] women",b,value=TRUE)[1])
   # get POS-tags
   b<-get.POStagged(b,"tagged")
   # remove coma at listing of subjectives
   b<-gsub("(/JJ) ,/,( [a-z\\-]*?/JJ)","\\1\\2",b)
   # remove and at listing of subjectives
   b<-gsub("(/JJ) and/CC( [a-z\\-]*?/JJ)","\\1\\2",b)
   # convert listing of adjectives with numbers to number of people
   b<-gsub("([0-9]*/CD [a-z]*/JJ) and/CC ([0-9]*/CD [a-z]*/JJ) ","\\1 people/NN and/CC \\2 people/NN ",b)
   # correct numbers/VBG to number/CD
   b<-gsub("([0-9]/)[A-Z][A-Z][A-Z]","\\1CD",b)
   b<-gsub("([0-9]/)[A-Z][A-Z]","\\1CD",b)
   # split at split words
   b<-grep(pattern,grep("[1-9]",unlist(strsplit(b," and/CC| to/TO|[a-z]*?/DT| ,/,| ;/:|[a-z]*?/IN|[a-z]*?/WP|[a-z]*?/PRP|[a-z]*?/VBD|[a-z]*?/VBN|[a-z]*?/VBP|[a-z]*?/VBG|[a-z]*?/MD")),value=TRUE),value=TRUE)
   # select lines with number
   b<-grep("[1-9]",b,value=TRUE)
   # remove until first number
   b<-sub(".* ([0-9].*/CD)","\\1",b)
   # remove lines with patterns
   b<-grep("taken out| missing|remain|decline|remove|excluded|refuse| drop| not/|were not| year| week| month| day|percent|\\%| group|out of| sample|sets|ended|cancel|abort| code| separate| factor| item| range| rating| ms| dB| potential| face|picture| pics| pix| video|pairs| names|detected| noun| verb| adjectiv| type",b,value=TRUE,invert=TRUE)

   # get first line of patterns and select number
   children<-grep("/CD",unlist(strsplit(grep(" children/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   control<-grep("/CD",unlist(strsplit(grep(" control/|controls/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   participants<-grep("/CD",unlist(strsplit(grep(" participants/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   people<-grep("/CD",unlist(strsplit(grep(" people/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   adults<-grep("/CD",unlist(strsplit(grep(" adults/| adult/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   patients<-grep("/CD",unlist(strsplit(grep(" patients/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   students<-grep("/CD",unlist(strsplit(grep(" students/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   couples<-grep("/CD",unlist(strsplit(grep(" couples/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   reports<-grep("/CD",unlist(strsplit(grep(" reports/",b,value=TRUE)[1]," ")),value=TRUE)[1]
   animals<-grep("/CD",unlist(strsplit(grep(" animals/",b,value=TRUE)[1]," ")),value=TRUE)[1]

   # remove POS
   patients<-paste(unlist(lapply(patients,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
   students<-paste(unlist(lapply(students,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
   adults<-paste(unlist(lapply(adults,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
   children<-paste(unlist(lapply(children,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")   
   control<-paste(unlist(lapply(control,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
   participants<-paste(unlist(lapply(participants,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
   reports<-paste(unlist(lapply(reports,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")
   animals<-paste(unlist(lapply(animals,function(x) gsub("/[A-Z][A-Z].*","",x))),collapse=" ")

   # get sum of captures
   animals<-sum(as.numeric(unlist(lapply(lapply(animals,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)
   couples<-2*sum(as.numeric(unlist(lapply(lapply(couples,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)
   people<-sum(as.numeric(unlist(lapply(lapply(people,function(x) gsub("/[A-Z][A-Z].*","",x)),"[",1)),warn=F),na.rm=T)

   # create result vector
   res<-c(male=male,female=female,patients=patients,students=students,children=children,control=control,
          participants=participants,adults=adults,people=people,reports=reports,animals=animals,couples=couples)
   # set lines with year, month, etc to NA
   pat<-" year| month| week| day| sequence| face| group| sets"
   if(length(grep(pat,res))>0) res[grep(pat,res)]<-"NA"
# if length of pattern match was 0
 }else res<-c(male=NA,female=NA,patients=NA,students=NA,children=NA,control=NA,
       participants=NA,adults=NA,people=NA,reports=NA,animals=NA,couples=NA)
# if length of input was 0
}else res<-c(male=NA,female=NA,patients=NA,students=NA,children=NA,control=NA,
       participants=NA,adults=NA,people=NA,reports=NA,animals=NA,couples=NA)

# convert bad captures to NA
res[which(res=="0")]<-NA
# get max of captures with sum(male,female), sum(patients,control), sum(child, adult)
num<-as.numeric(gsub("NA",NA,gsub(" .*","",res)))
num<-max(c(sum(num[1:2],na.rm=T,warn=F),sum(num[c(3,6)],na.rm=T,warn=F),sum(num[c(5,8)],na.rm=T,warn=F),max(num[3:13],na.rm=T,warn=F)),na.rm=T,warn=F)
if(num==0) num<-NA
gc()
if(max.only==F) res<-(c(res,maxSS=num))
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
## Function to etimate N from stats max((df1-1)+(df2+2))
#####################################################
est.ss.standardStats<-function(standardStats,quantileDF=.75,quantile.only=F){
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
    word_token_annotator <- 
openNLP::Maxent_Word_Token_Annotator()
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
