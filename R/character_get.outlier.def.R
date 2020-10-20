#' get.outlier.def
#'
#' Extract outlier/extreme value definition/removal in standard deviations, if present in text
#' @param x text to process
#' @export
#' @examples
#' x<-"We removed 4 extreme values that were 3 SD above mean."
#' get.outlier.def(x)

get.outlier.def<-function(x){
x<-tolower(x)
# Extract potential lines with "Outlier removal"
out<-unlist(lapply(x,function(x) grep("outlier|extreme|remove|exclud|correct|preclude|except",x,value=TRUE)))
# reduce to relevant ones
#out<-lapply(out,get.sentence.with.pattern,"remove|exclud|correct|preclude|except")
# that also contain standard deviation
out<-unlist(lapply(out,get.sentence.with.pattern,"sd|standard d"))
if(length(out)>0){
# remove html
temp<-lapply(out,function(x) gsub("<[a-z].*?.*[a-z\"]>","",x))
# split at sd
temp<-lapply(temp,function(x) unlist(strsplit(x,"SD|sd|standard d")))
# select first element
temp<-lapply(temp,"[",1)
# convert textual number to digit number
temp<-lapply(temp,function(x) text2num(unlist(x)))
# select lines with number
temp<-unlist(lapply(temp,function(x) grep("[0-9]$",x,value=TRUE)))
# remove spaces
temp<-lapply(temp,function(x) gsub(" ","",x))
# remove text in front of number
temp<-lapply(temp,function(x) gsub(".*[a-zA-Z%]","",x))
# clean up front till first number or "."
temp<-unlist(lapply(temp,function(x) gsub(".*[^0-9\\.]","",x)))
# set text as numeric vector
if(length(temp)>0) temp<-unique(as.numeric(temp))
} else temp<-out
if(length(temp)==0) temp<-character(0)
return(unique(temp))
}
