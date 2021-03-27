#' get.test.direction
#'
#' Extract mentioned test direction/s (one sided, two sided, one and two sided) from text
#' @param x text to process
#' @export

get.test.direction<-function(x){
# remove punctuation and double spaces than lowerize
x<-tolower(gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",x), perl = TRUE))
# get index of lines with test|hypothes etc. or test result
i1<-grep(" test|hypoth[ie]s|[<=>] [0-9\\.]|[<=>][0-9\\.]|signific|analys|effect|relation",x)
# get index of lines with sided or tailed/tails
i2<-grep("sided|tailed|tails",x)
# select lines
x<-x[unique(c(i1,i2))]
# unify
red<-gsub(" uni | 1 "," one ",x)
red<-gsub(" bi | un | 2 | both "," two ",red)
# unifi 'directional' -> 'sided' if has test or hypothesis
i<-grep("hypothes| test|relation",red)
if(length(i>0)){
 red[i]<-gsub("un directional|undirectional|two directional","two sided",red[i])
 red[i]<-gsub(" directional|unidirectional|^directional","one sided",red[i])
 red[i]<-gsub("directional","sided",red[i])
}
# 'tailed' -> 'sided'
red<-gsub("tailed|tails","sided",red)

# get lines with sided
red<-grep("sided",red,value=TRUE)
# correct/unify uni- and bisided
red<-gsub("unisided|uni sided|onesided","one sided",red)
red<-gsub("bisided|bi sided|twosided|bothsided|unsided","two sided",red)
# add "sided" to "one and two sided"
red<-gsub("one and two sided","one sided and two sided",red)
# split text lines
red<-unlist(strsplit(red," in | to | as | for | and | or | in "))
# get test direction
res<-which.term(red,c("one sided","two sided"))
if(sum(res==c(0,0))==2) out<-NA
if(sum(res==c(1,1))==2) out<-"one and two sided"
if(res[1]==0&res[2]==1) out<-"two sided"
if(res[1]==1&res[2]==0) out<-"one sided"

return(out)
}

