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
* [Acknowledgements](#acknowledgements)



<!-- ABOUT THE PROJECT -->
## About The Project

[![Product Name Screen Shot][product-screenshot]](https://example.com)

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
``` r
# Install the devtools package
install.packages("devtools")
# Load the devtools package
library(devtools)
# Install JATSdecoder from github
install_github("ingmarboeschen/JATSdecoder",auth_token=" 2d0c4be462585f84b38817a2690e16a699de5dc7")
```

<!-- USAGE EXAMPLES -->
## Usage
``` r
#' URL <- "https://journals.plos.org/plosone/article/file?id=10.1371/journal.pone.0114876&type=manuscript"
#' download.file(URL,"file.xml")
#' JATSdecoder("file.xml")
#' study.character("file.xml")
```

Use this space to show useful examples of how a project can be used. Additional screenshots, code examples and demos work well in this space. You may also link to more resources.

_For more examples, please refer to the [Documentation](https://example.com)_

