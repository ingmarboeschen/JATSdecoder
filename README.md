# JATSdecoder
A text extraction and manipulation toolset for NISO-JATS coded XML files 


<!-- TABLE OF CONTENTS -->
## Table of Contents

* [About the Project](#about-the-project)
  * [Built With](#built-with)
* [Getting Started](#getting-started)
  * [Installation](#installation)
* [Usage](#usage)
* [Roadmap](#roadmap)
* [Contributing](#contributing)
* [License](#license)
* [Contact](#contact)



<!-- ABOUT THE PROJECT -->
## About The Project
JATSdecoder is a function collection to extract meta data and textual content from NISO-JATS coded documents in XML format. It facilitates text mining projects on scientific reasearch papers by enabeling an individual selection of text parts.
JATSdecoder supplies some convenient helper functions that can be applied to any textual input.

[![JATSdecoder][product-screenshot]](https://example.com)

Here's a blank template to get started:
**To avoid retyping too much info. Do a search and replace with your text editor for the following:**
`github_username`, `repo_name`, `twitter_handle`, `email`


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
Here a simple download of a NISO-JATS coded article is performed with *download.file()
``` r
library(JATSdecoder)
URL <- "https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
download.file(URL,"file.xml")
# convert full article to list with meta data, sectioned text and referenz list
JATSdecoder("file.xml")
# extract study characteristics as list
study.character("file.xml")
```

## Usage for multiple files with [future.apply](https://github.com/HenrikBengtsson/future.apply) package
The PubMed Central data base offers more than 3 million documents related to the biology and health sciences. The full repository is bulk downloadable as NISO-JATS coded NXML documents here: [PMC bulk download](ftp://ftp.ncbi.nlm.nih.gov/pub/pmc/oa_bulk/) 

1. Get file names from working directory
``` r
setwd("/home/PMC")
files<-list.files(pattern="XML$|xml$",recursive=TRUE)
``` 
2. Apply multi core extraction of article content and study characteristics
``` r
library(JATSdecoder)
library(future.apply)
plan(multisession,workers=12)
JATS<-future_apply(files,JATSdecoder)
character<-future_apply(files,study.character)

```
3. Working with the results
``` r
JATS[1] # first article
lapply(JATS,"[","abstract") # all abstracts in JATS

