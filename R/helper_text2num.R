#' text2num
#'
#' Convert written numbers in text to digit number (works up to 99,999) 
#' @param x text to process
#' @export
#' @examples
#' x<-"Over all one thousand two hundred twenty-eight students 
#' participated in our Study, that was administred in thirteen clinics."
#' text2num(x)


text2num<-function(x){

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

# remove dashes - and dubled spaces
text<-gsub("  "," ",gsub("[-]"," ",text))
text<-unlist(strsplit(text," "))

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
text<-gsub(" [.]","\\.",gsub(" [,]",",",text))
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
    return(list(word,out))
}

