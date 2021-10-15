#' est.ss
#'
#' Function to estimate studies sample size by maximizing different conservative estimates.
#' Performs 4 different heuristic driven search tasks for reoprted sample size in abstract, text, stats and standardStats.
#' 
#' Sample size extraction from abstract:\cr
#' - Extracts N= from abstract if possible
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
  suppressWarnings({if(length(abstract)>0) a1<-est.ss.abstract(abstract) else a1<-NA})
  suppressWarnings({if(length(abstract)>0) a2<-est.ss.n(abstract) else a2<-NA})
  a<-suppressWarnings(max(c(a1,a2),na.rm=TRUE))
  a[a==-Inf|a==0]<-NA
  # get SS from first 10 lines max of text input (POS-tagging leads to overload in JAVA heap space)
  suppressWarnings({if(length(text)>0){
    # convert to sentences
    if(length(text)==1) text<-text2sentences(text)
    # remove lines with bootstrap|iteration
    text<-grep("[Bb]ootstrap|[Ii]teration|[Rr]epetition",text,value=TRUE,invert=TRUE)
#    t<-est.ss.text(text,max.only=max.parts)
    stats<-unlist(get.stats(text,output="stats"))
    standardStats<-standardStats(stats)
  } else{t<-NA;stats<-NULL;standardStats<-NULL}
  })
  suppressWarnings({if(length(stats)>0) s<-est.ss.stats(stats,max.only=max.parts) else s<-NA})
  suppressWarnings({if(length(standardStats)>0) ss<-est.ss.standardStats(standardStats,quantileDF=quantileDF,quantile.only=max.parts) else ss<-NA})
  # combine # ,SStext=t[length(t)]
  res<-c(SSabstract=a[length(a)],SSstats=s[length(s)],SSstandardStats=ss[length(ss)])
  # find max of estimates
  max<-max(suppressWarnings(as.numeric(res)),na.rm=T,warnings=F)
  if(max==0) max<-NA
  # output of estimate/s
  if(max.only==T) return(unname(c(estimated.sample.size=max)))
  if(max.only==F) return(c(SSabstract=a,SSstats=s,SSstandardStats=ss,estimated.sample.size=max))
}#,SStext=t

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
    
    # if line has t-value round up df2
    if(length(grep("^t$",colnames(matrix)))==1&length(grep("df2",colnames(matrix)))==1&nrow(matrix)>0) matrix[is.numeric(matrix[,"t"]),"df2"]<-ceiling(matrix[is.numeric(matrix[,"t"]),"df2"])
    
    # remove corrected df's: set df1 and df2 to NA if one of both contains a "." (correction)
    if(length(grep("df1|df2",colnames(matrix)))==2){ 
      matrix[grep("\\.",paste(factor(matrix[,"df1"]):factor(matrix[,"df2"]))),c("df1","df2")]<-NA
    }
    
    # if only df1 contains "."
    if(length(grep("df1",colnames(matrix)))==1) matrix[grep("\\.",paste(matrix[,"df1"])),"df1"]<-NA
    # if only contains df2
    if(length(grep("df2",colnames(matrix)))==1) matrix[grep("\\.",paste(matrix[,"df2"])),"df2"]<-NA
    # remove lines with Chi2
    if(length(grep("Chi2",colnames(matrix)))==1) matrix<-matrix[is.na(matrix[,"Chi2"]),]
    
    
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
    quantDF<-stats::quantile(sumdf,quantileDF,na.rm=T,type=7)
    if(length(grep("[^0-9\\.]",quantDF))>0) quantDF<-NA
  }
  else{
    sumdf<-NA;quantDF<-NA
  }
  # set Zero result to NA if is not NA already
  if(!is.na(quantDF)) if(quantDF==0) quantDF<-NA
  
  if(quantile.only==F) return(c(sumdf,quantileDF=quantDF))
  if(quantile.only==T) return(c(quantileDF=quantDF))
  
}

est.ss.abstract <- function(anAbstract = "", minmax = "max") {
  if(is.null(anAbstract))
    return(0)
  # preprocessing of abstract returns an extraction of sentences with remaining integer values
  sentences <- perform.preprocessing(anAbstract)
  # have to multiply sample by 2 if doublePattern. Some 'pairs' are not samples (e.g. 'fraction pairs')
  doublePattern <- "dyads|couples|(sister|brother|father|mother(-| )infant) pairs"
  # the vector where potential sample size numbers are collected
  numVector <- c()
  subgroupFlag <- 0
  # turns to TRUE if a sentence contains a sample size
  foundSample <- FALSE
  isMetaOrReview <- grepl("(meta( |-)?analysis)|((this|systematic|present|our|we) ([a-z]+ )?review)", anAbstract, ignore.case = TRUE)
  # if n = in Text, then only those numbers with n = are seen as sample
  onlyNcounts <- grepl("n ?= ?", anAbstract, ignore.case = TRUE)
  # if there's a distinction between 'N = ' and 'n = ', If 'N" ist mentioned, then 'n' is a subsample
  n_BigAndSmall <-  grepl("n ?= ?", anAbstract, ignore.case = FALSE) &&  grepl("N ?= ?", anAbstract, ignore.case = FALSE)
  #if multiple studies, there are two or more samples - (decision by parameter minmax)
  multipleStudies <- has.multipleStudies(anAbstract)
  # first pursue some simple strategies
  if(!multipleStudies && !onlyNcounts && !isMetaOrReview){
    if(length(sent <- simple.sample.sentences(sentences)) > 0)
      sentences <- sent
  }
  if(multipleStudies && !isMetaOrReview){
    samp <- processMultipleStudies(anAbstract, minmax)
    if(samp > 0)
      return(samp)
  }
  for(eachSentence in sentences){
    if(!foundSample || multipleStudies){
      # remove last punctuation to get "clean" tokens
      theSentence <- substr(eachSentence, 1, nchar(eachSentence)-1)
      # tokenize sentences (word as token)
      words <- unlist(JATSdecoder::vectorize.text(theSentence))
      for (index in 1:length(words)){
        token <- tolower(words[index])
        #clean the token
        token <- gsub(",|;|\\(|\\)","", token)
        # check whether token is integer value
        if(!is.na(num <- strtoi(token))){
          # look at some words before and after current number
          nGramBefore <- currentNgram(index, words, -4,1)
          nGramBefore <- gsub(",", "", nGramBefore)
          nGramAfter <- currentNgram(index, words, 0,3)
          nPattern <- ifelse(n_BigAndSmall, paste0("N ?= ?",num), paste0("n ?= ?",num))
          factor <- ifelse(grepl(doublePattern, theSentence, ignore.case = TRUE),2, 1)
          isPossibleSample <- !grepl(get.exclusionKeys(), nGramAfter)
          # if found something like "total of...", "total sample" is followed by num, then it seems to be total sample size
          if(grepl("total ",nGramBefore, ignore.case = TRUE) && !multipleStudies && isPossibleSample)
            return(num*factor)
          # look wheter n/N = comes before current integer
          currentIsN <- grepl(nPattern, nGramBefore, ignore.case = ifelse(n_BigAndSmall, FALSE, TRUE))
          if(onlyNcounts){ # if 'n = ' in text, then only numbers with 'n = ' count
            if(currentIsN)
              numVector <- append(numVector, num)
          } else {
            isPossibleSample <- !grepl(get.exclusionKeys(), nGramAfter)
            # e.g. '100 participants, including 30 patients with ..."
            isSubgroupType1 <- grepl(paste("including (([a-z]+ )+)?",num, sep = ""), nGramBefore)
            # e.g. '100 students (40 female)...
            isSubgroupType2 <- (foundSample && grepl(paste("\\(",num," [a-z]+\\)", sep = ""),nGramBefore))
            # e.g. "250 participants from 30 communities"
            isFromTerm <- (foundSample && grepl(paste("(from |across |at )",num," [a-z]+", sep = ""),nGramBefore))
            subgroupFlag <- ifelse(isSubgroupType1, subgroupFlag + 1, subgroupFlag)
            if(isPossibleSample && !isSubgroupType2 && !isFromTerm)
              numVector <- append(numVector, num)
          }
          #only samples greater than 4 count
          numVector <- numVector[numVector > 4]
        }
        foundSample <- length(numVector) > 0
      }
      if(multipleStudies){
        # this case is only relevant, if abstract text cannot be devided into studies
        if(length(numVector > 0))
          numVector <- ifelse(minmax == "max", max(numVector), min(numVector))
      }
    }
  }
  if(length(numVector) == 0){
    # no sample size mentioned or sample size smaller than 5
    return(0)
  }
  # handle special case found in an abstract: 78 individuals who made up 39 [...] couples
  if(factor > 1 && length(numVector) == 2 && (sort(numVector,decreasing = TRUE)[1] == sort(numVector,decreasing = TRUE)[2]*factor))
    return(numVector[1])
  # process found numbers:
  estimation <- processNumbers(numVector, subgroupFlag)
  return(estimation*factor)
}

processNumbers <- function(aNumVector, aFlag){
  # if aNumVector contains only one number, nothing to process (return aNumVector)
  # else:
  #   if first number equals sum of all other numbers, return first number (the other
  #   numbers seem to be  reported subgroups
  #   if aFlag is set, only a part of subgroups is reported. Return first number
  #   else: add all numbers up.
  if((len <- length(aNumVector)) == 1)
    return(aNumVector)
  first <- aNumVector[1]
  rest <- aNumVector[2:len]
  subGroupsReported <- aFlag > 0
  return(ifelse(first == sum(rest)&&(len > 2), first, ifelse(subGroupsReported, first, sum(aNumVector))))
}

has.multipleStudies <- function(anAbstract){
  return(grepl("(study|experiment) 1\\,? |in the first (study|experiment)|(( |^)[0-9] (([A-z]+ ){,5})?(studies|experiments))", JATSdecoder::text2num(anAbstract), ignore.case = TRUE))
}

processMultipleStudies <- function(anAbstract, minmax = "max"){
  studVector <- unlist(JATSdecoder::strsplit2(anAbstract, "(([Ss]tud(y|ies)|[Ee]xperiments?) [0-9])|(([Ff]irst|[Ss]econd)( (study|experiment)|,))", "before"))
  if(length(studVector) < 2)
    return(0)
  numVector <- c()
  start <- ifelse(grepl(anAbstract, pattern = "[Ss]tudy 1[;\\.\\)]"), 1,2)
  #print(start)
  #print(studVector)
  for(index in start:length(studVector)){
    each <- gsub("([Ss]tudy|studies|[Ee]xperiments?)( [0-9] ?(to|-|and) ?[0-9])?","REP_STUD", studVector[index])
    s <- est.ss.abstract(each)
    numVector <- append(numVector,s)
  }
  if(minmax == "max")
    numVector <- sort(numVector, decreasing = TRUE)
  if(minmax == "min")
    numVector <- sort(numVector, decreasing = FALSE)
  return(ifelse(length(numVector) > 0, numVector[1], 0))
}
###################################################################################
#  this function return an n-gram around an element of aVector -
#  n-gram boundaries are the given lower- and upperLimit around anIndex
###################################################################################

currentNgram <- function (anIndex, aVector, lowerLimit, upperLimit){
  currNgram <- c()
  for(i in lowerLimit:upperLimit){
    if(anIndex + i > 0)
      currNgram <- append(currNgram,aVector[anIndex + i])
  }
  currNgram <- currNgram[!is.na(currNgram)]
  return(paste(currNgram, collapse = " "))
}


simple.sample.sentences <- function(sentenceVector){
  # find sentences with some often used sample references
  pattern1 <- "([Pp]articipants were [0-9]+( [a-z]+)+)"
  pattern2 <- "([0-9]+ ([A-z]+ )+(participated|were included))"
  pattern3 <- "[0-9]+ individuals"
  pattern4 <- "(sample (comprised|of) [0-9]+)"
  pattern5 <- "([0-9]+ ([A-z]+ ){0,4}students)"
  pattern6 <- "(data (was |were )?(drawn |collected )?from [0-9]+)"
  pattern7 <- "(A total of [0-9]+)"
  thePattern <- paste(pattern1, "|", pattern2, "|", pattern3, "|", pattern4,"|", pattern5,"|", pattern6,"|", pattern7, sep = "")
  sentences <- unlist(lapply(sentenceVector, JATSdecoder::get.sentence.with.pattern, thePattern))
  sentences <- sentences[sentences>0]
  return(sentences)
}

#***********************************************************************************************
# this function provides the fundamental list of key words to recognize non-relevant integers
# all 'exclusion keys' are put together as a regex pattern to filter them with pattern matching.
# Add further keys seperated by '|' to supplement this list.
# **********************************************************************************************

get.exclusionKeys <- function(){
  # returns pattern for words that are not kind of sample.
  # these words must not always be located directly behind a number but
  # also in the ngram after
  education <- "class(es|rooms)|schools|groups|universities|"
  locations <- "rooms|areas|locations|communities|organi(z|s)ations|hospitals|"
  study <- "studies|experiments|tasks|references|articles|papers|records|databases|"
  psych <- "trials|sessions|analyses|"
  testconstr <- "scales|items?|factors?|(sub)?tests|dimensions|guidelines|"
  grouping <- "domains|types|categor(y|ies)|units|facets|levels|blocks|"
  audioVisual <- "images|paintings|photo(s|graphs)?|pictures|tones?|"
  time <- "days|decades|"
  #last entry -> no OR-Pipe
  other <- "activities|facts|elements|objects|points?|steps?|concepts"
  return (paste0(education, locations, study, psych, testconstr, grouping, audioVisual, time, other))
}


perform.preprocessing <- function (anAbstract) {
  # patterns to remove bibliographic information from text
  bibPattern1 <- "\\((e\\.g\\.\\,? ?)?(([A-z] ?)+; )?(([A-Z]\\. )+)?([A-Z][a-z]+[^\\)]*\\, )?(1|2)(0|9)[0-9][0-9][a-z]?(\\, ([^\\)])+)?\\)"
  bibPattern2 <- "[A-Z][a-z]+( et al\\.)?, (1|2)(0|9)[0-9][0-9][a-z]?(\\)| ?;)"
  # remove capital letter shortcuts (like SDQ-20)
  shortcutPattern <- "([A-Z][A-Z]+( ?-? ?)|([0-9]+)?[A-z]+-)[0-9]+( and [0-9]+)?|[0-9]+-" #das muss ich noch mal pruefen
  range1 <- "( |\\()[0-9]+-? ?(-|to) ?[0-9]+( |[,;-]|\\)|\\.)"
  range2 <- "(between|from) [0-9]+ (to|and) [0-9]+"
  rangePattern <- paste0("(", range1, ")|(", range2, ")")
  # remove likert explanation in braces like 1 (very good) to 5 (very bad), so that range can be recognized
  preprocessed <- gsub(" ?\\([^0-9]+\\)", "", anAbstract)
  # replace '5- ' or '20-' (have to replace this before text2num()!!)
  preprocessed <- gsub(rangePattern, " REP _RANGE", anAbstract, ignore.case = TRUE)
  preprocessed <- gsub(shortcutPattern, " REP _SC", preprocessed, ignore.case = FALSE)
  # hope that sample size doesn't stand in braces without any letter! next line is important for functioning bib-pattern
  preprocessed <- gsub("\\([0-9]+( and [0-9]+)?\\)", "REP_BRCS", preprocessed)
  preprocessed <- gsub(bibPattern1, " REP _BIB", preprocessed, ignore.case = FALSE)
  preprocessed <- gsub(bibPattern2, " REP _BIB", preprocessed, ignore.case = FALSE)
  
  for(i in 1:2) # remove ',' from numbers (up to 9,999,999)
    preprocessed <- gsub("([0-9]),([0-9][0-9][0-9])","\\1\\2",preprocessed)
  # replace enumerations
  preprocessed <- gsub("\\[|\\]", "", preprocessed)
  enumPattern <- paste0("([0-9]+-?\\, )+(and|or) [0-9]+")
  preprocessed <- gsub(enumPattern, " REP _ENUM", JATSdecoder::text2num(preprocessed), ignore.case = TRUE)
  #handle per cent, F(x, xx) (and other upcoming)
  preprocessed <- gsub("[0-9]+(\\.[0-9]+)? ?(\\%|per )", " REP _PER ", preprocessed, ignore.case = TRUE)
  preprocessed <- gsub("[A-Z]+ ?\\([0-9]\\, [0-9]+\\)", " REP _F ", preprocessed, ignore.case = FALSE)
  preprocessed <- gsub("[0-9] [0-9]( [0-9])+", " REP _NUMCHAIN ", preprocessed)
  preprocessed <- gsub("[^Nn]( = |=)[0-9]+", " REP _EQU ", preprocessed)
  preprocessed <-gsub("[0-9]+ of [0-9]+", " REP _xOFy ", preprocessed)
  #eliminate some leading words and numbers like 'grade 6 and 7' or 'experiment 2 to 5'
  preprocessed <-gsub("(version |grade |experiments? |[Ff]ormula |stud(y|ies) )[0-9]+( (to |and )[0-9]+)?", " REP _LEAD ", preprocessed, ignore.case = TRUE)
  #eliminate years
  preprocessed <-gsub("(in |after |before |since |until |till |during )([a-z]+ )?(year )?(1|2)(0|9)[0-9][0-9]( and (1|2)(0|9)[0-9][0-9])?", " REP _YEAR", preprocessed, ignore.case = TRUE )
  preprocessed <-gsub("(< ?|> ?|than |over |under |at (least |most )?)[0-9]+", " REP_COMP ", preprocessed, ignore.case = TRUE )
  preprocessed <-gsub("[0-9]+ (in|or|out of|vs.) [0-9]+", " REP _OR ", preprocessed, ignore.case = TRUE )
  
  agePattern <- "( |\\()age(d|s)? (of )?([a-z]+ )?[0-9]+( and [0-9]+)?"
  preprocessed <- gsub(agePattern, " REP _AGE ", preprocessed, ignore.case = TRUE)
  preprocessed <- removeDateAndTimeInfo(preprocessed)
  preprocessed <- removeMeasure(preprocessed)
  preprocessed <- removeSubgroupDeclaration(preprocessed)
  preprocessed <- gsub("[0-9]+\\-", " REP _NUM_HyPHEN", preprocessed)
  preprocessed <- gsub("[0-9]+ (possible|main|specific|different|familiar|functional|dimensional|public|private)", "REP _ADJS", preprocessed)
  preprocessed <- gsub("(recruited from|identified|completed) [0-9]+ ", "REP _VERBS", preprocessed)
  preprocessed <- gsub("(from|at) [0-9]+ ([a-z]+ )?(schools|universities|hospitals)", " REP _FROM", preprocessed)
  preprocessed <- gsub("[Qq]uestionnaire ([A-z]+ )?[0-9]+", " REP _QUEST", preprocessed)
  preprocessed <- gsub(" (the(se)?|a|or|through|that|their|(out of)|(set of)|into|all) [0-9]+( |,|;|\\.|\\))", " _REP _WORD_AND_NUMBER_", preprocessed)
  # return all remaining sentences with integer values
  return(get.sentences.with.integers(preprocessed))
}

removeMeasure <- function(aText){
  meas <- paste ("([0-9]+ (and |or |to ))?[0-9]+",get.measurePattern())
  text <- gsub(meas, "REP _MEASURE", aText, ignore.case = TRUE)
  return(text)
}

removeSubgroupDeclaration <- function(aText){
  # just one subgroup is named? Have to ignore this (sometimes there is also a percentage mentioned with has been replaced before)
  subgroupPattern <- "\\([0-9]+ (women|girls|females?|men|boys|males?)( \\(REP_PC \\))?\\)"
  text <- gsub(subgroupPattern, " REP _SUBGR", aText, ignore.case = FALSE)
  return(text)
}

removeDateAndTimeInfo <- function(aText){
  "information like 'january, 31, 2021' or 'march 2001' or '22 february, 2000"
  year <- "[12][09][0-9][0-9]"
  day <- "[0-9]+(st|nd|rd|th)?"
  datePattern <- paste0("(",day," )?",get.monthPattern(), "( ",day,")?", ",? (",year,")?")
  text <- gsub(datePattern, " REP _DATE", aText, ignore.case = TRUE)
  text <- gsub(paste0(year," until ","(",year,")?"), " REP _DATE", text)
  "information like '20 weeks' or 'a 6 month follow up'"
  text <- gsub(" [0-9]+ (times|hours|days|weeks|months|years|waves)",
               " REP _TIME", text, ignore.case = TRUE)
  return(text)
}

###################################################################################
#  This function returns a vector of sentences which contain an integer value
###################################################################################

get.sentences.with.integers <- function(aText) {
  sentences <- unlist(JATSdecoder::text2sentences(aText))
  intPattern <- "( |\\(|^)[0-9]+(,[0-9]+)?( |,|;|\\)|\\.$)"
  return(sentences[grep(intPattern, sentences)])
}
###################################################################################
#  function to build a simple regex pattern with several measures collapsed by '|'
###################################################################################

get.measurePattern <- function(){
  return ("(db|(milli)?sec(onds)?|(n|m)?s|min(utes)?|(m|c|k)?m(/h)?|g|k?g(/m ?2)?|h(r|rs|z)?)( |\\)|,|;)")
}

#**************************************************************
# helping function: returns a RegEx pattern with all months like
#**************************************************************
get.monthPattern <- function(){
  #returns all months collapsed by | for regex-pattern search
  return ("(january|february|march|april|may|june|july|august|september|october|november|december)")
}

