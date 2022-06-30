#' get.power
#'
#' Extracts a priori power and empirial power values from text.
#' @param x text string to process.
#' @export
#' @examples
#' x<-"We used G*Power 3 to calculate the needed sample with 
#' beta error rate set to 12% and alpha error to .05."
#' get.power(x)

get.power<-function(x){
# convert to sentences if has length 1
if(length(x)==1) x<-text2sentences(x)
# convert greek beta to 'beta'
x<-gsub("\u00df","beta",x)
# convert imputed operator to '='
x<-gsub("<=>","=",x)
# unify 1-beta
x<-gsub("([^0-9])1-b([^a-z])","\\11-beta\\2",x)
# correct missing beta in (1-) num
x<-gsub("\\(1 *- *\\) *([\\.0-9])","(1-beta) \\1",x)
# correct missing beta in 1- = num
x<-gsub("1 *- *= *([\\.0-9])","1-beta = ",x)
x<-gsub("1 b[^a-z]","1-b\\1",x)
x<-gsub("  *"," ",gsub("[(]1 *- *[^0-9a-z]","",x))
# remove number in bracket
x<-gsub("[(][0-9]*[)]","",x)
# remove number after second dot in badly captured numbers
x<-gsub("([0-9]*\\.[0-9]*)\\.[0-9]*","\\1",x)

# get lines with power/beta error
x<-get.sentence.with.pattern(x,"[^a-z]power[^a-z]|1[- ]*beta|1-b[^a-z]|type[- ]*2[- ]*error|type[- ]ii[- ]error|beta[- ]*error")
# exclude lines with patterns
x<-grep("mW|Hz|m\U00FCM|to the power|predicti|[0-9] *ms *|Volt[ a]| [Vv]olt[ a]|[0-9] [Ww]att|[0-9] W[^a-z]|[Pp]ower [1-9]\\.[0-9]|ship power|total power|factor of power",x,value=TRUE,invert=TRUE)
# unify
x<-gsub("was set","is set",x)
x<-gsub(" a[n]* | the "," ",x)
x<-gsub(" percent| \\%","\U0025",x)
# num% power -> power=num%
x<-gsub("([0-9\\.]*\\%) power","power=\\1",x)

if(length(x)>0){
# extract lines with power and 1-beta error
p<-grep("[^a-z][Pp]ower[^a-z]|^Power[^a-z]|1[- ][- ][Bb]eta|1-[Bb]eta|beta[- ]error",x,value=TRUE)
# unify power
p<-gsub("1[- ]*beta|1-b[^a-z]","power",p)

# split lines into a priori power and observed power
# exclude as a priori power
power<-grep("vs\\. |months| was | were |Runs|significant|observed|F *<sub>[(]|F *[(][0-9]|[^a-z]t *[(][0-9]|post[ -]hoc",p,value=TRUE,invert=TRUE)
# include as post hoc power
obs<-grep("vs\\. |months| was | were | Runs|significant|observed|F *<sub>[(]|F *[(][0-9]|[^a-z]t *[(][0-9]|post[ -]hoc",p,value=TRUE)
# both as list
power<-list(power=power,observed_power=obs)

# split and extract lines with power pattern again
power<-lapply(power,function(x) unlist(strsplit(x,"[;,] | and ")))
power<-lapply(power,function(x) gsub(" \\\U0025","\U0025",x))

# unify use of % and 2 numbers (num-num)
# "68.7-74.4\U0025" to "68.7\U0025-74.4\U0025" ## weak!?
power<-lapply(power,function(x) gsub("([0-9][0-9]\\.[0-9]*)[^0-9\U0025]","\\1\U0025 ",x))
power<-lapply(power,function(x) gsub("([0-9][0-9]\\.[0-9])$","\\1\U0025 ",x))

power<-lapply(power,function(x) gsub("([0-9])-[0-9][0-9].*?\\\U0025","\\1\U0025",x))
power<-lapply(power,function(x) gsub("([0-9]) - [0-9][0-9].*?\\\U0025","\\1\U0025",x))
power<-lapply(power,function(x) gsub("([0-9]) and [0-9][0-9].*?\\\U0025","\\1\U0025",x))
power<-lapply(power,function(x) gsub("([0-9]) to [0-9][0-9].*?\\\U0025","\\1\U0025",x))
# "0.num1-0.num2" to ".num1"
power<-lapply(power,function(x) gsub("([0-9])-[0-9]\\.[0-9].*?[^0-9]","\\1 ",x))
# move number in front of power to back if has none behind
power<-lapply(power,function(x) gsub("([0-9][\\.0-9]*[\\%]) power( *[^=0-9])"," power=\\1 \\2",unlist(x)))
# select lines with ".number" or "%"
power<-lapply(power,function(x) grep("\\.[0-9]|\\\U0025",x,value=TRUE))
beta<-lapply(beta,function(x) grep("\\.[0-9]|\\\U0025",x,value=TRUE))
# unify power synonyms
power<-lapply(power,function(x) gsub("1[ -]*[Bb]eta|1[ -]*[Bb]eta[- ]*error|\\(1-[Bb]\\)|[Pp]ower level","power",x))
# unify beta error synonyms
beta<-lapply(beta,function(x) gsub("[Tt]ype[- ]*2[- ]*[Ee]rror|[Tt]ype[- ]*[iI][iI][- ]*error|[Bb]eta[- ]*error|","betaerror",x))
# select lines with power and number
beta<-unlist(lapply(x,get.sentence.with.pattern,"power|betaerror"))
beta<-unlist(lapply(x,get.sentence.with.pattern,"[0-9]"))

# remove '()'
power<-lapply(power,function(x) gsub("[\\(\\)]","",x))
beta<-lapply(beta,function(x) gsub("[\\(\\)]","",x))
# strip double white spaces
power<-lapply(power,function(x) gsub("  *"," ",x))
beta<-lapply(beta,function(x) gsub("  *"," ",x))
# remove "rate"
power<-lapply(power,function(x) gsub(" rate "," ",x))
beta<-lapply(beta,function(x) gsub(" rate "," ",x))
# unify equals
power<-lapply(power,function(x) gsub(" \\= | \\=| \\=| equal to ","=",x))
beta<-lapply(beta,function(x) gsub(" \\= | \\=| \\=| equal to ","=",x))
power<-lapply(power,function(x) gsub("power [ioat][sfto] ([0-9\\.])","power=\\1",x))
beta<-lapply(beta,function(x) gsub("betaerror [ioat][sfto] ([0-9\\.])","betaerror=\\1",x))
power<-lapply(power,function(x) gsub(" is set [ta][to] | set [ta][to] | was set [ta][to] | was |is ","=",x))
beta<-lapply(beta,function(x) gsub(" is set [ta][to] | set [ta][to] | was set [ta][to] | was |is ","=",x))

# add '=' to power+number
power<-lapply(power,function(x) gsub("power ([\\.0-9])","power=\\1",x))
beta<-lapply(beta,function(x) gsub("betaerror ([\\.0-9])","betaerror=\\1",x))
# add '=' to nonumber+power+word+number
power<-lapply(power,function(x) gsub("([^0-9]*) power [a-z]* [aoi][fts] ([\\.0-9])","\\1 power=\\2",x))
power<-lapply(power,function(x) gsub("([^0-9]*) power [a-z]* [aoi][fts] ([\\.0-9])","\\1 power=\\2",x))
# remove space in front of %
beta<-lapply(beta,function(x) gsub(" \\\U0025","\U0025",x))
# add '=' to number+power
power<-lapply(power,function(x) gsub("([0-9][0-9]\\\U0025) power","power=\\1",x))
power<-lapply(power,function(x) gsub("(\\.[0-9][0-9]) power","power=\\1",x))
power<-lapply(power,function(x) gsub("([0-9][0-9]\\\U0025) statistical power","power=\\1",x))
power<-lapply(power,function(x) gsub("(\\.[0-9][0-9]) statistical power","power=\\1",x))
power<-lapply(power,function(x) grep("power",x,value=TRUE))
beta<-lapply(beta,function(x) grep("betaerror",x,value=TRUE))

## case 1: has "power="
power<-lapply(power,function(x) grep("power[=]",x,value=TRUE))
power<-lapply(power,function(x) gsub(".*power[=]","",x)) 
beta<-lapply(beta,function(x) grep("betaerror[=]",x,value=TRUE))
beta<-lapply(beta,function(x) gsub(".*betaerror[=]","",x)) 

# remove text after first match of non number \U0025 or .
power<-lapply(power,function(x) gsub("[^0-9\\\U0025\\.].*","",x)) 
beta<-lapply(beta,function(x) gsub("[^0-9\\\U0025\\.].*","",x)) 
# remove . at end
power<-lapply(power,function(x) gsub("\\.$","",x))
beta<-lapply(beta,function(x) gsub("\\.$","",x))
# only select lines with number
power<-lapply(power,function(x) grep("[0-9]",x,value=TRUE))
beta<-lapply(beta,function(x) grep("[0-9]",x,value=TRUE))
# convert "\U0025" to e-2
power<-lapply(power,function(x) gsub("\\\U0025","e-2",x))
beta<-lapply(beta,function(x) gsub("\\\U0025","e-2",x))
# convert to numeric
power<-suppressWarnings(lapply(power,as.numeric))
# calculate 1-beta
beta<-suppressWarnings(1-unlist(lapply(beta,as.numeric)))

# merge results of "power" and "1-beta" 
power[["power"]]<-c(power[["power"]],beta)
# reduce to unique values and remove NA
power["power"]<-lapply(power["power"],unique)
power<-lapply(power,function(x) x[!is.na(x)])
# only keep values [0;1]
power<-lapply(power,function(x) unique(x[which(x>=0&x<=1)]))
}else power<-list(power=numeric(0),observed_power=numeric(0))
return(power)
}

