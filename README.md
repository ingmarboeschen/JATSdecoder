# JATSdecoder
A text extraction and manipulation toolset for the statistical programming language [R](www.r-project.org). 
JATSdecoder facilitates text mining projects on scientific reasearch papers by enabeling an individual selection by meta data and text parts. 
Its function *JATSdecoder()* extracts meta data, sectioned text and reference list from NISO-JATS coded XML files.
Its function *study.character()* uses the JATSdecoder() result to perform fine tuned text extraction tasks to identify key study characteristics like statistical methods used, alpha-error, statistical results reported in text. 
*study.character()*'s extraction functions can be applied to any textual input.
*JATSdecoder* supplies some helpfull functions to work with textual input. 
Its function *letter.convert()* unifies hexadecimal to unicode characters and if [CERMINE](https://github.com/CeON/CERMINE) generated XML files are processed special error correction, which is extremely relevant to its ability to extract statistical results in text. text2num() unifies representations of written numbers and special annotations (percent, fraction, e+10) into digit numbers. Its function *text2sentences()* is especially design to break floating text with scientific content (referenzes, results) into sentences.

- JATSdecoders function **JATSdecoder()** consist of several functions that can also be used stand alone on NISO-JATS doeded XML files or text input:
  - get.title()      # extracts title
  - get.author()     # extracts author/s as vector
  - get.aff()        # extracts involved affiliation as vector
  - get.journal()    # extracts journal
  - get.vol()        # extracts journal volume as vector
  - get.doi()        # extracts Digital Object Identifier
  - get.history()    # extracts publishing history as vector with available date stamps
  - get.country()    # extracts country/countries of origin of affiliation/s as vector
  - get.type()       # extracts document type
  - get.subject()    # extracts subjects as vector
  - get.keywords()   # extracts keywords as vector
  - get.abstract()   # extracts abstract
  - get.text()       # extracts sections and text as list
  - get.references() # extracts reference list as vector


- JATSdecoders function **study.character()** consist of several functions that are applied to specific text parts of the *JATSdecoder()* result but can also be used stand alone on any plain text input:
  - get.n.studies()   # extracts title
  - get.author() # extracts author/s as vector
  - get.journal() # extracts journal
  - get.vol()     # extracts vol as vector
  - get.history() # extracts history as vector
  - get.aff()     # extracts involved affiliation as vector
  - get.country() # extracts country/countries of origin as vector
  - get.doi()     # extracts Digital Object Identifier
  - get.text()    # extracts sections and text as list
  - get.references() # extracts reference list as vector




### Built With
* [R Core 3.6](www.r-project.org)
* [RKWard](https://rkward.kde.org/)


<!-- GETTING STARTED -->
## Getting Started

To get a local copy up and running follow these simple steps.

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
## Usage for single file
Here a simple download of a NISO-JATS coded XML file is performed with *download.file()*
``` r
library(JATSdecoder)
URL <- "https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
download.file(URL,"file.xml")
# convert full article to list with meta data, sectioned text and referenz list
JATSdecoder("file.xml")
# extract specific content (here: abstract)
get.abstract("file.xml")
# extract study characteristics as list
study.character("file.xml")
```

## Usage for multiple files with [future.apply](https://github.com/HenrikBengtsson/future.apply) package
The PubMed Central data base offers more than 3 million documents related to the biology and health sciences. The full repository is bulk downloadable as NISO-JATS coded NXML documents here: [PMC bulk download](https://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/) 

1. Get file names from working directory
``` r
setwd("/home/PMC")
files<-list.files(pattern="XML$|xml$",recursive=TRUE)
``` 
2. Apply extraction of article content (replace lapply() with future.apply() for multi core processing)
``` r
library(JATSdecoder)
# extract full article content
JATS<-lapply(files,JATSdecoder)
# extract single article content (here: abstract)
abstract<-lapply(files,get.abstract)
```
3. Working with the *JATSdecoder results
``` r
# first article
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
4. Convert and unify text with helper functions
``` r
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

# extract study.characteristics
character<-lapply(files,study.character)
