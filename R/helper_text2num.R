#' text2num
#'
#' Converts special annotated number and written numbers in a text string to a fully digit representation.
#' Can handle numbers with exponent, fraction, percent, e+num, products and written representation (e.g. 'fourtys-one') of all absolut numbers till 99,999 (Note: gives false returns for higher numbers). Process is performed in the same order as its arguments.
#' @param x text to process
#' @param exponent Logical. If TRUE values with exponent are converted to a digit representation
#' @param percentage Logical. If TRUE percentages are converted to a digit representation
#' @param fraction Logical. If TRUE fractions are converted to a digit representation
#' @param e Logical. If TRUE values denoted with num e+num (e.g. '2e+2') or num*10^num are converted to a digit representation
#' @param product Logical. If TRUE values products are converted to a digit representation
#' @param words Logical. If TRUE written numbers are converted to a digit representation
#' @export
#' @examples
#' x<-c("numbers with exponent: -2^3, .2^-2, -.3^.2, 49^-.5, 2^10.",
#'      "numbers with percentage: 2%, 15 %, 25 percent.",
#'      "numbers with fractions: 1/100, -2/5, -7/-.1",
#'      "numbers with e: 10e+2, -20e3, .2E-2, 2e4",
#'      "numbers as products: 100*2, -20*.1, 2*10^3",
#'      "written numbers: twenty-two, one hundred fourty five",
#'      "mix: one hundred ten is not 1/10 is not 10^2 nor 10%/5")
#' text2num(x)

text2num<-function(x,exponent=TRUE,percentage=TRUE,fraction=TRUE,e=TRUE,product=TRUE,words=TRUE){
# convert textual representations of numbers
if(exponent==TRUE)   x<-unlist(lapply(x,hight2num))
if(percentage==TRUE)    x<-unlist(lapply(x,percent2number))
if(e==TRUE)          x<-unlist(lapply(x,e2num))    
if(product==TRUE)    x<-unlist(lapply(x,function(y) tryCatch(prod2num(y),error=function(e) return(y))))    
if(words==TRUE)      x<-unlist(lapply(x,text2digit))
if(words==TRUE&percentage==TRUE)    x<-unlist(lapply(x,percent2number))
if(fraction==TRUE)   x<-unlist(lapply(x,function(y) tryCatch(frac2num(y),error=function(e) return(y))))

# output
return(x)
}

text2digit<-function(x){
# correct spelling
text<-gsub("hundret |houndred |houndret |hunderd","hundred ",x)
# remove and
text<-gsub("hundred and ","hundred ",text)
text<-gsub("([a-z])hundred","\\1 hundred ",text)
text<-gsub("thousand and ","thousand ",text)
text<-gsub("([a-z])thousand and ","\\1 thousand ",text)
text<-gsub("fourty","forty",text)
if(length(text)>0&sum(is.na(text)==0)){ 
    one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,six=6, seven=7, eight=8, nine=9)
    teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,sixteen=16, seventeen=17, eighteen=18, nineteen=19)
    ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,sixty=60, seventy=70, eighty=80, ninety=90)
    doubles <- c(teens,ten_digits)

# get word in front of hundred/thousand    
# add space before coma, bracket and point
text<-gsub("[.]"," \\.",gsub("[,]"," ,",text))
text<-gsub("[)]"," \\)",gsub("[(]","\\( ",text))

text<-gsub("  "," ",text)
# remove special dashes
text<-gsub("[-]hund"," hund",text)
text<-gsub("[-]thousa"," thousa",text)
text<-gsub("ty[-]([otfsen])","ty \\1",text)

text<-unlist(strsplit(text," "))
text<-unlist(strsplit2(text,"[\\.,]","before"))
nums<-c(names(unlist(teens)),names(unlist(ten_digits)),names(unlist(one_digits))[-1],"hundred","thousand","million")
e<-(1:length(text))[is.element(tolower(text),nums)]
if(length(e)>0){
# n blocks
nblock<-sum(e[-1]!=(e[-length(e)]+1))+1
# starting at
start<-c(1,which(e[-1]!=(e[-length(e)]+1))+1)
# ending at
end<-c(which(e[-1]!=(e[-length(e)]+1)),length(e))

# first block of numbers
num<-NULL;replace<-NULL;remove<-NULL
for(i in 1:nblock) num[i]<-paste(text[e[start[i]:end[i]]],collapse=" ")
for(i in 1:nblock) replace[i]<-unlist(word2num(num[i])[2])
# replace text to numeric number
for(i in 1:nblock) text[e[start[i]]]<-replace[i]
ind<-(1:nblock)[e[start]<e[end]]
for(i in ind) remove<-c(remove,-((e[start[i]]+1):e[end[i]]))
if(!is.null(remove)) text<-text[remove]
}

text<-paste(text,collapse=" ")
# clean up hundred and thousand
#text<-gsub("([0-9]) hundred ","\\1",text)
#text<-gsub("([0-9]) thousand ","\\1",text)

# remove space before coma, bracket and point
text<-gsub(" [.]([^0-9])","\\.\\1",gsub(" [,]",",",text))
text<-gsub("([0-9]) ([.][0-9])","\\1\\2",text)
text<-gsub(" [)]","\\)",gsub("[(] ","\\(",text))
} else text<-NA
return(text)
}

word2num <- function(word){
    wsplit <- strsplit(tolower(word)," ")[[1]]
    one_digits <- list(zero=0, one=1, two=2, three=3, four=4, five=5,six=6, seven=7, eight=8, nine=9)
    teens <- list(eleven=11, twelve=12, thirteen=13, fourteen=14, fifteen=15,sixteen=16, seventeen=17, eighteen=18, nineteen=19)
    ten_digits <- list(ten=10, twenty=20, thirty=30, forty=40, fifty=50,sixty=60, seventy=70, eighty=80, ninety=90)
    doubles <- c(teens,ten_digits)
    # only go on 
    c(names(unlist(one_digits)),names(unlist(doubles)))
    temp<-NULL
    out <- 0
    i <- 1
    while(i <= length(wsplit)){
        j <- 1
        if(i==1 && wsplit[i]=="hundred")
            temp <- 100
        else if(i==1 && wsplit[i]=="thousand")
            temp2 <- 1000
        else if(wsplit[i] %in% names(one_digits))
            temp <- as.numeric(one_digits[wsplit[i]])
        else if(wsplit[i] %in% names(teens))
            temp <- as.numeric(teens[wsplit[i]])
        else if(wsplit[i] %in% names(ten_digits))
            temp <- (as.numeric(ten_digits[wsplit[i]]))
        if(i < length(wsplit) && wsplit[i+1]=="hundred"){
            if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
                out <- out + 100*temp
            else
                out <- 100*(out + temp)
            j <- 2
        }
        else if(i < length(wsplit) && wsplit[i+1]=="thousand"){
            if(i>1 && wsplit[i-1] %in% c("hundred","thousand"))
                out <- out + 1000*temp
            else
                out <- 1000*(out + temp)
            j <- 2
        }
        else if(i < length(wsplit) && wsplit[i+1] %in% names(doubles)){
            temp <- temp*100
            out <- out + temp
        }
        else{
            out <- out + temp
        }
        i <- i + j
    }
    if(length(out)==0) out<-word
    out<-list(word,out)
    return(out)    
}

# function to convert percent to number   
percent2number<-function(x){
if(length(grep("\\%|[0-9] percent",x))>0){
    x<-gsub("\\\\","",x)
    x<-gsub("([0-9]) percent","\\1%",x)
    x<-gsub("([0-9]) \\%","\\1%",x)
    x<-unlist(strsplit2(x,"[0-9][%]","after"))
    i<-grep("[0-9][%]|[0-9][%]$",x)
    if(length(i)>0){
        stop<-FALSE
        while(stop!=TRUE){
            i<-grep("[0-9][%]|[0-9][%]$",x)[1]
            m <- regexpr("[0-9\\.]*?[%]|[0-9\\.]*?[%]$", x[i])
            remove<-regmatches(x[i], m)
            insert<-as.numeric(gsub("%","",remove))/100
            x[i]<-gsub(remove,insert,x[i])
            if(length(grep("[0-9][%]|[0-9][%]$",x))==0) stop<-TRUE
        }
    }
# clean up
    x<-gsub("  "," ",x<-paste(x,collapse=" "))
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$",".",x)
    }
    return(x)
}

# function to convert ^num
hight2num<-function(x){
if(length(grep("[0-9]\\^[-\\.0-9]",x))>0){
x<-unlist(strsplit2(x,"\\.$","before"))
x<-unlist(strsplit2(x,"[^-\\.0-9][-\\.0-9]*?\\^[-\\.0-9]","before"))
    # add space to end
    x<-paste0(x," ")
    # if has num^num calculate and and replace 
    ind<-grep("[^a-zA-Z][0-9]\\^[-\\.0-9]|^[0-9]\\^[-\\.0-9]",x)
    exponent <- function(a, pow) (abs(a)^pow)*sign(a)
    res<-suppressWarnings(format( exponent(as.numeric(gsub(".*[^-0-9\\.]","\\1",
                                                 gsub("(.*[-0-9\\.]*)\\^[-\\.0-9].*","\\1",x[ind]))),
                                       as.numeric(gsub("[^-0-9\\.].*","\\1",gsub(".*[-0-9\\.]*?\\^([-\\.0-9]*)","\\1",x[ind])))),
                                 scientific=F))

        # clean up white spaces
    res<-gsub("^ *|(?<= ) | *$", "", res, perl = TRUE)
    # remove only zeros at end
    res<-gsub("\\.[0]*$","",res)
    res<-gsub("(\\.[0-9]*?)0*$","\\1",res)
    # insert result
    suppressWarnings(if(length(ind)>0) for(i in 1:length(ind)) x[ind[i]]<-gsub("[-0-9\\.]*\\^[-\\.0-9]*",res[i],x[ind[i]]))
    # collapse
    x<-gsub("  "," ",gsub(" $","",paste(x,collapse=" ")))
    # clean up
    x<-gsub("  "," ",x)
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$|,\\.",".",x)
}
    return(x)
}


# function to convert e num
e2num<-function(x){
    
    # convert "num*10^num"-> "num e num"
    x<-gsub("([0-9]) *?[\\*x] *?10\\^","\\1e",x)
    
if(length(grep("[0-9][Ee][-\\+\\.0-9]",x))>0){
    x<-gsub("([0-9])[Ee]([0-9])","\\1e+\\2",x)
    x<-gsub("([0-9])E([-\\+\\.0-9])","\\1e\\2",x)
    x<-unlist(strsplit2(x," [-\\.0-9]*e[\\-\\+][1-9]","before"))
    # add space to end
    x<-paste0(x,"  ")
    # if has one e[\\-\\+1-9] convert to number and replace with result
    ind<-(1:length(x))[nchar(x)-nchar(gsub("[0-9]e-[\\.0-9]","",x))==4]
    suppressWarnings(if(length(ind)>0) for(i in ind) x[i]<-gsub("([-0-9\\.]*)e(-[0-9\\.]*)",format(round(as.numeric(gsub(".*?([-0-9\\.]*)e(-[0-9\\.]*).*","\\1e\\2",x[i])),10),scientific=F),x[i])
    )
    ind<-(1:length(x))[nchar(x)-nchar(gsub("[0-9]e\\+[\\.0-9]","",x))==4]
    suppressWarnings(if(length(ind)>0) for(i in ind) x[i]<-gsub("([-0-9\\.]*)e(\\+[0-9\\.]*)",format(as.numeric((gsub(".*?([-0-9\\.]*)e(\\+[0-9\\.]*).*","\\1e\\2",x[i]))),scientific=F),x[i]) )
    # collapse and clean up
    x<-gsub("  "," ",gsub("  "," ",gsub(" $","",paste(x,collapse=" "))))
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$",".",x)
}
    return(x)
}

# convert fraction to digit number
frac2num<-function(x){
tryCatch({
if(length(grep("/[-\\.0-9]|/ [-\\.0-9]",x))>0){
    x<-unlist(strsplit2(x,"\\.$","before"))
    x<-gsub("([0-9]) /([-\\.0-9])","\\1/\\2",x)
    x<-gsub("([0-9]) / ([-\\.0-9])","\\1/\\2",x)
    x<-gsub("([0-9])/ ([-\\.0-9])","\\1/\\2",x)
    x<-unlist(strsplit2(x,"[^-\\.0-9][-\\.0-9]*?/[-\\.0-9]","before"))
    # get lines with fraction
    ind<-grep("[0-9]/[-0-9\\.]",x)
    # lines with only one fraction
    ind<-ind[nchar(x[ind])-nchar(gsub("/","",x[ind]))==1]
    if(length(ind)>0){
        # get num/num
        frac<-regmatches(x[ind],regexpr("[-\\.0-9]*/[-\\.0-9]*",x[ind]))
        # recompute num=num/num if num/num
        isfrac<-grep("[0-9]/[0-9]",gsub("[-\\.]","",frac))
        ind<-ind[isfrac]
        if(length(ind)>0){
            # recompute num=num/num
        num<-sapply(frac, function(x) format(eval(parse(text=x)),scientific=FALSE))
#        num<-as.character(round(num,4))
        # insert num
        for(i in 1:length(ind)) x[ind[i]]<-gsub("([-\\.0-9]*/[-\\.0-9]*)",num[i],x[ind[i]])
    }}
    # collapse and clean up
    x<-gsub("  "," ",gsub("  "," ",gsub(" $","",paste(x,collapse=" "))))
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$",".",x)
}
    return(x)
},error=function(e) return(x))
}
# convert product to digit number
prod2num<-function(x){
    x<-gsub("([0-9]) *?[\\*] *?","\\1*",x)
tryCatch({
if(length(grep("\\*[-\\.0-9]|\\* [-\\.0-9]",x))>0){
    x<-unlist(strsplit2(x,"\\.$","before"))
    x<-gsub("([0-9]) \\*([-\\.0-9])","\\1*\\2",x)
    x<-gsub("([0-9]) \\* ([-\\.0-9])","\\1*\\2",x)
    x<-gsub("([0-9])\\* ([-\\.0-9])","\\1*\\2",x)
    x<-unlist(strsplit2(x,"[^-\\.0-9][-\\.0-9]*?\\*[-\\.0-9]","before"))
    # get lines with product
    ind<-grep("[0-9]\\*[-0-9\\.]",x)
    # lines with only one product
    ind<-ind[nchar(x[ind])-nchar(gsub("\\*","",x[ind]))==1]
    if(length(ind)>0){
        # get num/num
        prod<-regmatches(x[ind],regexpr("[-\\.0-9]*\\*[-\\.0-9]*",x[ind]))
        # recompute num=num*num
        num<-sapply(prod, function(x) format(eval(parse(text=x)),scientific=FALSE))
#        num<-as.character(round(num,4))
        # insert num
        for(i in 1:length(ind)) x[ind[i]]<-gsub("([-\\.0-9]*\\*[-\\.0-9]*)",num[i],x[ind[i]])
    }
    # collapse and clean up
    x<-gsub("  "," ",gsub("  "," ",gsub(" $","",paste(x,collapse=" "))))
    x<-gsub(" , ",", ",x)
    x<-gsub(" \\.$",".",x)
}
    return(x)
},error=function(e) return(x))
}
