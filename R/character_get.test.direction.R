#' get.test.direction
#'
#' Extract mentioned test direction/s (one sided, two sided, one and two sided) from text
#' @param x text to process
#' @export

get.test.direction<-function(x){
# get lines with test
red<-get.sentence.with.pattern(x,c(" test|hypothes|[<=>] [0-9\\.]|[<=>][0-9\\.]"))
# remove punctuation and double spaces, than lowerize
red<-tolower(gsub("^ *|(?<= ) | *$", "",gsub("[[:punct:]]"," ",red), perl = TRUE))
# unify
red<-gsub(" uni | 1 "," one ",red)
red<-gsub(" bi | un | 2 | both "," two ",red)
# unifi directional|tailed -> sided
red<-gsub("un directional|undirectional|two directional","two sided",red)
red<-gsub(" directional|unidirectional|^directional","one sided",red)
red<-gsub("directional|tailed","sided",red)
# get lines with sided
red<-get.sentence.with.pattern(red,"sided")
# unify uni- and bisided
red<-gsub("unisided|uni sided|onesided","one sided",red)
red<-gsub("bisided|bi sided|twosided|bothsided|unsided","two sided",red)

# add "sided" to "one and two sided"
red<-gsub("one and two sided","one sided and two sided",red)

# split text lines
red<-unlist(strsplit(red," in | to | as | for | and | or | in "))

# get test direction
res<-which.term(red,c("one sided","two sided"))
if(sum(res==c(0,0))==2) out<-"not detected"
if(sum(res==c(1,1))==2) out<-"one and two sided"
if(res[1]==0&res[2]==1) out<-"two sided"
if(res[1]==1&res[2]==0) out<-"one sided"

return(out)
}

