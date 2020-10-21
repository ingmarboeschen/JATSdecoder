# JATSdecoder
A meta data and text extraction and manipulation tool set for the statistical programming language [R](www.r-project.org). \n
**JATSdecoder** facilitates text mining projects on scientific reasearch papers by enabeling an individual selection of meta data and text parts. 
Its function **JATSdecoder()** extracts meta data, sectioned text and reference list from NISO-JATS coded XML files. 
Its function **study.character()** uses the **JATSdecoder()** result to perform fine tuned text extraction tasks to identify key study characteristics like statistical methods used, alpha-error, statistical results reported in text. 
**study.character()**'s extraction functions can be applied to any plain textual input. 
**JATSdecoder** supplies some helpfull functions to work with textual input in general. 
Its function **letter.convert()** unifies hexadecimal to unicode characters and if [CERMINE](https://github.com/CeON/CERMINE) generated XML files are processed special error correction, which is extremely relevant to its ability to extract statistical results in text. 
**text2num()** unifies representations of written numbers and special annotations (percent, fraction, e+10) into digit numbers. 
Its function **text2sentences()** is especially design to break floating text with scientific content (referenzes, results) into sentences. 
You can extract adjustable n words around a pattern match in a sentence with **ngramaraoundpattern()**.

- **JATSdecoder::JATSdecoder()** uses functions that can be applied stand alone on NISO-JATS coded XML files or text input:
  - get.title()      # extracts title
  - get.author()     # extracts author/s as vector
  - get.aff()        # extracts involved affiliation/s as vector
  - get.journal()    # extracts journal
  - get.vol()        # extracts journal volume as vector
  - get.doi()        # extracts Digital Object Identifier
  - get.history()    # extracts publishing history as vector with available date stamps
  - get.country()    # extracts country/countries of origin as vector with unique countries
  - get.type()       # extracts document type
  - get.subject()    # extracts subject/s as vector
  - get.keywords()   # extracts keyword/s as vector
  - get.abstract()   # extracts abstract
  - get.text()       # extracts sections and text as list
  - get.references() # extracts reference list as vector


- **JATSdecoder::study.character()** applies several functions on specific parts of the **JATSdecoder()** result and can be used stand alone on any plain text input:
  - get.n.studies()   # extracts number of studies from sections or abstract
  - get.alpha.error()  # extracts alpha error from text 
  - get.method()  # extracts statistical methods from method and result section with *ngramaroundpattern()*
  - get.stats()  # extracts statistical results reported in text (abstract and full text, method and result section, result section only) 
  - get.software()  # extracts software name/s mentioned in method and result section with dictionary search
  - get.R.package()  # extracts mentioned R package name/sin method and result section with dictionary created with *available.packages()*
  - get.power()  # extracts power (1-beta-error) if mentoioned in text
  - get.assumption()  # extracts mentioned assumptions from method and result section with dictionary search
  - get.multiple.comparison()  # extracts correction method for multiple testing from method and result section with dictionary search
  - get.sig.adjectives()  # extracts common inadequate adjectives used before *signignificant* and *not significant* 

- **JATSdecoder helper functions** are helpfull for many text mining project and straight forward to use:
  - text2num() # converts spelled number, fractions, potencies, percentages and numbers denoted with e+num to decimals
  - ngram # creates n gram of words around a pattern match in text 
  - strsplit2() # splits text at pattern match with option "before" or "after" and without removing the pattern match 
  - letter.convert() # converts many hexadecimal and HTML characters to unicode and performs CERMINE specific error correction
  - which.term() # returns hit vector for a set of patterns to search for in text (can be reduced to hits only)

### Built With
* [R Core 3.6](https://www.r-project.org)
* [RKWard](https://rkward.kde.org/)


<!-- GETTING STARTED -->
## Getting Started

To install and use **JATSdecoder** follow these steps:

### Installation
1. Install and load the [devtools](https://github.com/r-lib/devtools) package
``` r
# Install the devtools package
install.packages("devtools")
# Load the devtools package
library(devtools)
``` 
2. Install *JATSdecoder from [github](https://github.com/ingmarboeschen/JATSdecoder)
``` r
# Install JATSdecoder from github
install_github("ingmarboeschen/JATSdecoder",auth_token=" 2d0c4be462585f84b38817a2690e16a699de5dc7")
```

<!-- USAGE EXAMPLES -->
## Usage for a single file
Here a simple download of a NISO-JATS coded XML file is performed with *download.file()*
``` r
library(JATSdecoder)
URL <- "https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
download.file(URL,"file.xml")
# convert full article to list with meta data, sectioned text and referenz list
JATSdecoder("file.xml")
# extract specific content (here: abstract)
JATSdecoder("file.xml",output="abstract")
get.abstract("file.xml")
# extract study characteristics as list
study.character("file.xml")
# extract specific study characteristic (here: statistical results)
study.character("file.xml",output=c("stats","standardStats"),text.mode=3) # with text.mode=3 results from result section are extracted only
```

## Usage for multiple files
The PubMed Central data base offers more than 3 million documents related to the biology and health sciences. The full repository is bulk downloadable as NISO-JATS coded NXML documents here: [PMC bulk download](https://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/). 

1. Get file names from working directory
``` r
setwd("/home/PMC") # May be you would like to choose a certain journal folder instead for testing
files<-list.files(pattern="XML$|xml$",recursive=TRUE)
``` 
2. Apply extraction of article content to all files (replace *lapply()* with *future.apply()* from [future.apply](https://github.com/HenrikBengtsson/future.apply) package for multi core processing)
``` r
library(JATSdecoder)
# extract full article content
JATS<-lapply(files,JATSdecoder)
# extract single article content (here: abstract)
abstract<-lapply(files,get.abstract)
# or
abstract<-lapply(files,JATSdecoder,output="abstract")
```
3. Working with a list of **JATSdecoder()** results
``` r
# first article content as list
JATS[[1]] 
character[[1]] 
# names of all extractable elements
names(JATS[[1]])
names(character[[1]])
# extract one element only (here: title, abstract, history)
lapply(JATS,"[[","title") 
lapply(JATS,"[[","abstract") 
lapply(JATS,"[[","history") 
# extract year of publication from history tag
unlist(lapply(lapply(JATS,"[[","history") ,"[","pubyear"))
``` 
4. Examples for converting, unifying and selecting text with helper functions
``` r
# extract full text from all documents
text<-lapply(JATS,"[[","text") 
# convert floating text to sentences
sentences<-lapply(text,text2sentences)
sentences
# only select sentences with pattern and unlist article wise
pattern<-"significant"
hits<-lapply(sentences,function(x) grep(pattern,x,value=T))
hits<-lapply(hits,unlist)
hits
# number of sentences with pattern
lapply(hits,length)
# unify written numbers, fractions, percentages, potencies and numbers denoted with e+num to digit number
lapply(text,text2num)
``` 


<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements
This software is part of a dissertation project about the evolution of methodological characteristics in psychological research and financed by a grant awarded by the Department of [Psychological Methods and Statistics](https://www.psy.uni-hamburg.de/arbeitsbereiche/psychologische-methoden-und-statistik.html), Institute of Psychology, [University Hamburg](https://www.uni-hamburg.de/), Germany.  
