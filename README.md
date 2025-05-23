# JATSdecoder
A metadata and text extraction and text manipulation tool set for the statistical programming language [R](www.r-project.org). 

**JATSdecoder** facilitates text mining projects on scientific articles by enabling an individual selection of metadata and text parts. 
Its function `JATSdecoder()` extracts metadata, sectioned text and reference list from [NISO-JATS](https://jats.nlm.nih.gov/publishing/tag-library/1.1d2/index.html) coded XML files. 
The function `study.character()` uses the `JATSdecoder()` result to perform fine-tuned text extraction tasks to identify key study characteristics like statistical methods used, alpha-error, statistical results reported in text and others. 

Note:  
- PDF article collections can be converted to NISO-JATS coded XML files with the open source software [CERMINE](https://github.com/CeON/CERMINE).
- To extract statistical test results reported in simple/unpublished PDF documents with JATSdecoder::get.stats(), the R package [pdftools](https://cran.r-project.org/web/packages/pdftools/) and its function pdf_text() may help to extract textual content (be aware that tabled content may cause corrupt text).  

Note too:  
- A minimal web app to extract statistical results from textual resources with get.stats() is hosted at:  
[https://get-stats.app](https://get-stats.app)  
- An interactive web application to analyze study characteristics of articles stored in the PubMed Central database and perform an individual article selection by study characteristcs is hosted at:  
[https://scianalyzer.com/](https://scianalyzer.com/)

**JATSdecoder** supplies some convenient functions to work with textual input in general. 
Its function `text2sentences()` is especially designed to break floating text with scientific content (references, results) into sentences. 
`text2num()` unifies representations of written numbers and special annotations (percent, fraction, e+10) into digit numbers. 
You can extract adjustable n words around a pattern match in a sentence with `ngram()`. 
`letter.convert()` unifies hexadecimal to Unicode characters and, if [CERMINE](https://github.com/CeON/CERMINE) generated CERMXML files are processed, special error correction and special letter uniformization is performed, which is extremely relevant for `get.stats()`'s ability to extract and recompute statistical results in text. 

The contained functions are listed below. For a detailed description, see the documentation on [CRAN](https://cran.r-project.org/web/packages/JATSdecoder/index.html).

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


- **JATSdecoder::study.character()** applies several functions on specific elements of the `JATSdecoder()` result. These functions can be used stand alone on any plain textual input:
  - get.n.studies()   # extracts number of studies from sections or abstract
  - get.alpha.error()  # extracts alpha error from text 
  - get.method()  # extracts statistical methods from method and result section with `ngram()`
  - get.stats()  # extracts statistical results reported in text (abstract and full text, method and result section, result section only) and compare extracted recalculated p-values if possible 
  - get.software()  # extracts software name/s mentioned in method and result section with dictionary search
  - get.R.package()  # extracts mentioned R package/s in method and result section with dictionary search on all available R packages created with `available.packages()`
  - get.power()  # extracts power (1-beta-error) if mentioned in text
  - get.assumption()  # extracts mentioned assumptions from method and result section with dictionary search
  - get.multiple.comparison()  # extracts correction method for multiple testing from method and result section with dictionary search
  - get.sig.adjectives()  # extracts common inadequate adjectives used before *significant* and *not significant* 

- **JATSdecoder helper functions** are helpful for many text mining projects and straight forward to use on any textual input:
  - text2sentences() # breaks floating text into sentences
  - text2num() # converts spelled out numbers, fractions, potencies, percentages and numbers denoted with e+num to decimals
  - ngram() # creates &plusmn;n-gram bag of words around a pattern match in text 
  - strsplit2() # splits text at pattern match with option "before" or "after" and without removing the pattern match 
  - grep2() # extension of grep(). Allows connecting multiple search patterns with logical AND operator
  - letter.convert() # unifies many and converts most hexadecimal and HTML characters to Unicode and performs CERMINE specific error correction
  - which.term() # returns hit vector for a set of patterns to search for in text (can be reduced to hits only)

### Built With
* [R Core 3.6](https://www.r-project.org)
* [RKWard](https://rkward.kde.org/)
* [devtools](https://github.com/r-lib/devtools) package


### How to cite JATSdecoder
```
JATSdecoder: A Metadata and Text Extraction and Manipulation Tool Set. Ingmar Böschen (2023). R package version 1.2.0
```
### Resources
**Articles:**

- Böschen, I. (2021). Software review: The JATSdecoder package—extract metadata, abstract and sectioned text from NISO-JATS coded XML documents; Insights to PubMed central’s open access database. Scientometrics. https://doi.org/10.1007/s11192-021-04162-z. [link to [repo](https://github.com/ingmarboeschen/JATSdecoderEvaluation/tree/main/Evaluation_PubMedCentral_data)]

- Böschen, I. (2021). Evaluation of JATSdecoder as an automated text extraction tool for statistical results in scientific reports. Scientific Reports 11, 19525. https://doi.org/10.1038/s41598-021-98782-3. [link to [repo](https://github.com/ingmarboeschen/JATSdecoderEvaluation/tree/main/Evaluation_get.stats_data)]

- Böschen, I. (2023). Evaluation of the extraction of methodological study characteristics with JATSdecoder. Scientific Reports 13, 139. https://doi.org/10.1038/s41598-022-27085-y. [link to [repo](https://github.com/ingmarboeschen/JATSdecoderEvaluation/tree/main/Evaluation_study.character_data)]

- Böschen, I. (2023). Changes in methodological study characteristics in psychology between 2010-2021. PLOS ONE 18(5). https://doi.org/10.1371/journal.pone.0283353. [link to [repo](https://github.com/ingmarboeschen/JATSdecoderEvaluation/tree/main/Study_ChangesInMethodologyInPsychology)]

- Böschen, I. (2024). statcheck is flawed by design and no valid spell checker for statistical results. [https://arxiv.org/abs/2408.07948](https://arxiv.org/abs/2408.07948). [link to [repo](https://github.com/ingmarboeschen/JATSdecoderEvaluation/tree/main/Check_statcheck)]

**Evaluation data and code:** 

[https://github.com/ingmarboeschen/JATSdecoderEvaluation/](https://github.com/ingmarboeschen/JATSdecoderEvaluation/)

**JATSdecoder on CRAN:**

[https://CRAN.R-project.org/package=JATSdecoder/](https://CRAN.R-project.org/package=JATSdecoder/)


<!-- GETTING STARTED -->
## Getting Started

To install **JATSdecoder** run the following steps:

### Installation
Option 1: Install **JATSdecoder** from [CRAN](https://cran.r-project.org/)
``` r
install.packages("JATSdecoder")
``` 
Option 2: Install **JATSdecoder** from [github](https://github.com/ingmarboeschen/JATSdecoder) with the [devtools](https://cran.r-project.org/web/packages/devtools/index.html) package
``` r
if(require(devtools)!=TRUE) install.packages("devtools")
devtools::install_github("ingmarboeschen/JATSdecoder")
```


<!-- USAGE EXAMPLES -->
## Usage for a single XML file
Here, a simple download of a NISO-JATS coded XML file is performed with `download.file()`:
``` r
# load package
library(JATSdecoder)
# download example XML file via URL
URL <- "https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
download.file(URL,"file.xml")
# convert full article to list with metadata, sectioned text and reference list
JATSdecoder("file.xml")
# extract specific content (here: abstract)
JATSdecoder("file.xml",output="abstract")
get.abstract("file.xml")
# extract study characteristics as list
study.character("file.xml")
# extract specific study characteristic (here: statistical results)
study.character("file.xml",output=c("stats","standardStats")) 
# reduce to checkable results only
study.character("file.xml",output="standardStats",stats.mode="checkable")
# compare with result of statcheck's function checkHTML() (Epskamp & Nuijten, 2018)
install.packages("statcheck")
library(statcheck)
checkHTML("file.xml")

# extract results with get.stats() from simple/unpublished manuscripts with pdftools::pdf_text()
x<-pdftools::pdf_text("path2file.pdf")
x<-unlist(strsplit(x,"\\n"))
JATSdecoder::get.stats(x)

```

## Usage for a collection of XML files
The [PubMed Central](https://www.ncbi.nlm.nih.gov/pmc/) database offers more than 5.4 million documents related to the biology and health sciences. The full repository is bulk downloadable as NISO-JATS coded NXML documents here: [PMC bulk download](https://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/). 

1. Get XML file names from working directory
``` r
setwd("/home/PMC") # choose a specific folder with NISO-JATS coded articles in XML files on your device
files<-list.files(pattern="XML$|xml$",recursive=TRUE)
``` 
2. Apply the extraction of article content to all files (replace `lapply()` with `future.apply()` from [future.apply](https://github.com/HenrikBengtsson/future.apply) package for multicore processing)
``` r
library(JATSdecoder)
# extract full article content
JATS<-lapply(files,JATSdecoder)
# extract single article content (here: abstract)
abstract<-lapply(files,JATSdecoder,output="abstract")
# or
abstract<-lapply(files,get.abstract)
# extract study characteristics
character<-lapply(files,study.character)
```
3. Working with a list of `JATSdecoder()` results
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
unlist(lapply(JATS,"[[","history") ,"[","pubyear")
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

## Exemplary analysis of some NISO-JATS tags
Next, some example analysis are performed on the full PMC article collection. As each variable is very memory consuming, you might want to reduce your analysis to a smaller amount of articles. 

1. Extract JATS for article collection (replace `lapply()` with `future.apply()` from [future.apply](https://github.com/HenrikBengtsson/future.apply) package for multicore processing)
```r
# load package
library(JATSdecoder)
# set working directory
setwd("/home/foldername")
# get XML file names
files<-list.files(patt="xml$|XML$")
# extract JATS
JATS<-lapply(files,JATSdecoder)
```

2. Analyze distribution of publishing year
```r
# extract and numerize year of publication from history tag
year<-unlist(lapply(lapply(JATS,"[[","history") ,"[","pubyear"))
year<-as.numeric(year)
# frequency table
table(year)
# display absolute number of published documents per year in barplot
# with factorized year
year<-factor(year,min(year,na.rm=TRUE):max(year,na.rm=TRUE))
barplot(table(year),las=1,xlab="year",main="absolute number of published PMC documents per year")
# display cummulative number of published documents in barplot
barplot(cumsum(table(year)),las=1,xlab="year",main="cummulative number of published PMC documents")
``` 
![](articlesperyear.png)

3. Analyze distribution of document type
```r
# extract document type
type<-unlist(lapply(JATS ,"[","type"))
# increase left margin of grafik output
par(mar=c(5,12,4,2)+.1)
# display in barplot
barplot(sort(table(type)),horiz=TRUE,las=1)             
# set margins back to normal
par(mar=c(5,4,4,2)+.1)
``` 
![](type.png)

4. Find most frequent authors

NOTE: author names are not stored fully consistent. Some first and middle names are abbreviated, first names are followed by last names and vice versa!

```r
# extract author
author<-lapply(JATS ,"[","author")
# top 100 most present author names 
tab<-sort(table(unlist(author)),dec=T)[1:100]
# frequency table
tab
# display in barplot
# increase left margin of grafik output
par(mar=c(5,12,4,2)+.1)
barplot(tab,horiz=TRUE,las=1)             
# set margins back to normal
par(mar=c(5,4,4,2)+.1)
# display in wordcloud with wordcloud package
library(wordcloud)
wordcloud(names(tab),tab)
``` 
![](author.png)

## References
<div id="refs" class="references">
<div id="CERMINE">
- National Center for Biotechnology Information (NCBI), National Library of Medicine (NLM). 2014. Journal Publishing Tag Library - NISO JATS Draft Version 1.1d2. 
[https://jats.nlm.nih.gov/publishing/tag-library/1.1d2/index.html].
</div>

<div id="JATS">
- Dominika Tkaczyk, Pawel Szostek, Mateusz Fedoryszak, Piotr Jan Dendek and Lukasz Bolikowski. 
CERMINE: automatic extraction of structured metadata from scientific literature. 
In International Journal on Document Analysis and Recognition (IJDAR), 2015, 
vol. 18, no. 4, pp. 317-335, doi: 10.1007/s10032-015-0249-8. 
[https://github.com/CeON/CERMINE/].
</div>


<div id="JATSdecoder">
- Böschen, I. (2021) Software review: The JATSdecoder package—extract metadata, abstract and sectioned text from NISO-JATS coded XML documents; Insights to PubMed central’s open access database. Scientometrics. https://doi.org/10.1007/s11192-021-04162-z
</div>
  
<div id="get.stats">
- Böschen, I. (2021). Evaluation of JATSdecoder as an automated text extraction tool for statistical results in scientific reports. Scientific Reports. 11, 19525. https://doi.org/10.1038/s41598-021-98782-3
</div>
  
<div id="study.character">
- Böschen, I. (2023). Evaluation of the extraction of methodological study characteristics with JATSdecoder. Scientific Reports. 13, 139. https://doi.org/10.1038/s41598-022-27085-y
</div>

</div>

<!-- ACKNOWLEDGEMENTS -->
## Acknowledgements
This software is part of a dissertation project about the evolution of methodological characteristics in psychological research and financed by a grant awarded by the Department of [Research Methods and Statistics](https://www.psy.uni-hamburg.de/arbeitsbereiche/forschungsmethoden-und-statistik.html), [Institute of Psychology](https://www.psy.uni-hamburg.de/), [University Hamburg](https://www.uni-hamburg.de/), Germany.  
