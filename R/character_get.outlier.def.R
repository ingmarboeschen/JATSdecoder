#' get.outlier.def
#'
#' Extract outlier/extreme value definition/removal in standard deviations, if present in text
#' @param x text to process
#' @export
#' @examples
#' x<-"We removed 4 extreme values that were 3 SD above mean."
#' get.outlier.def(x)

get.outlier.def<-function(x){
# convert to sentences if has length 1
if(length(x)==1) x<-text2sentences(x)
# remove num%* SDS
x<-gsub("[0-9\\.]*%* SDS","",x)

# lowerize
x<-tolower(x)
# remove numbers with percent sign
x<-gsub("[0-9\\.]* *%","",x)
# SDs to SD
x<-gsub("([^a-z])sds([^a-z])|([^a-z])sd[' ]*s([^a-z])","\\1\\3sd\\2\\4",x)
# Extract potential lines with "Outlier removal"
out<-grep("outlier|extreme|remove|delete|discard| dropped| exclud|preclude|except| omit",x,value=TRUE)
# that also contain standard deviation
out<-grep("[^a-z]sd[^a-z]|standard dev",out,value=TRUE)
# and a number
# convert text2num and select lines with number
out<-grep("[0-9]",text2num(out),value=TRUE)
if(length(out)>0){
  # remove html
  temp<-gsub("<[a-z].*?.*[a-z\"]>","",out)
# unify standard deviation
  temp<-gsub("standard deviations*","sd",temp)
  # if has number behind sd and not in front change order
  temp<-gsub("[^0-9] sd ([0-9\\.]*)"," \\1 sd",temp)  
  # extract number in front of sd
  temp<-suppressWarnings(as.numeric(gsub(".* ([0-9\\.]*?) sd.*","\\1",temp)))
    # select non NAs
  temp<-temp[!is.na(temp)]
  # extract unique values and reduce to valid values >1
  if(length(temp)>0){
    temp<-unique(temp)
    temp<-temp[temp>1]
  } else temp<-character(0)
} else temp<-character(0)

return(temp)
}

