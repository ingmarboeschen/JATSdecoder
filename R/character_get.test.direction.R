#' get.test.direction
#'
#' Extracts mentioned test direction/s (one sided, two sided, one and two sided) from text.
#' @param x text string to process.
#' @export

get.test.direction<-function(x){
if(length(x)==1) x<-text2sentences(x)
# remove punctuation and double spaces than lowerize
x<-tolower(gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",x), perl = TRUE))
# get index of lines with test|hypothes etc. or test result
# hypoth[ie]s|
i1<-grep(" test|[<=>] [0-9\\.]|[<=>][0-9\\.]|signific|analys|effect|relation",x)
# get index of lines with sided or tailed/tails
i2<-grep("sided|tailed|tails",x)
# select lines
x<-x[unique(c(i1,i2))]

# remove lines with 'no' in lines without 'significant'
i1<-grep(" no |^no | not |instead of|n't ",x)
if(length(i1)>0){
  i2<-grep("signific",x,invert=TRUE)
  i<-i1[is.element(i1,i2)]
  if(length(i)>0) x<-x[-i]
}

# unify
red<-gsub(" uni | 1 "," one ",x)
red<-gsub(" bi | un | 2 | both "," two ",red)
red<-gsub("two tails","two sided",red)

# unifi 'directional' -> 'sided' if has test or hypothesis
i<-grep("directional[^ ]* [a-z]* *[a-z]* *hypothes|directional[^ ]* [a-z]* *[a-z]* *test",red)
if(length(i>0)){
  red[i]<-gsub("un directional|bidirectional|undirectional|two directional"," two sided",red[i])
  red[i]<-gsub("unidirectional","one sided",red[i])
  red[i]<-gsub("[^m][^u][^l][^t][^i] directional|unidirectional|^directional"," one sided",red[i])
  #  red[i]<-gsub("directional","sided",red[i])
}
# 'tailed' -> 'sided'
red<-gsub("  *"," ",gsub("tailed|tails","sided",red))

# get lines with sided
red<-grep("sided",red,value=TRUE)
# remove lines with patterns
red<-grep("[^a-z]path[s]* |pathway|interact| book| page|questionaire| paper| coin| medal| form[^a-z]",red,value=TRUE,invert=TRUE)

# correct/unify uni- and bisided
red<-gsub("([^0-9])2([^0-9])","\\1two\\2",red)
red<-gsub("unisided|uni sided|onesided","one sided",red)
red<-gsub("bi *sided|twosided|bothsided|unsided","two sided",red)
# add "sided" to "one and two sided"
red<-gsub("one and two sided","one sided and two sided",red)
# split text lines
red<-unlist(strsplit(red," in | to | as | for | and | or | in "))
# get test direction
res<-which.term(red,c("one sided","two sided"))
if(sum(res==c(0,0))==2) out<-character(0)
if(sum(res==c(1,1))==2) out<-"one and two sided"
if(res[1]==0&res[2]==1) out<-"two sided"
if(res[1]==1&res[2]==0) out<-"one sided"

return(out)
}

