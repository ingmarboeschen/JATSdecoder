#' get.software
#'
#' Extracts mentioned software from text by dictionary search for 63 software names (object: .software_names).
#' @param x text string to process.
#' @param add.software a text vector with additional software name patterns to search for.
#' @seealso \code{\link[JATSdecoder]{study.character}} for extracting multiple study characteristics at once.
#' @return Character. Vector with identified statistical software/s.
#' @export

#' @examples
#' get.software("We used the R Software and Excel 4.0 to analyse our data.")

get.software<-function(x,add.software=NULL){
# convert / * and - to space
x<-gsub("[/\\*]|-"," ",unlist(x))
# remove html
x<-gsub("<.*?.*>"," ",x)
# convert double spaces to single space
x<-gsub("^ *|(?<= ) | *$","",x,perl=TRUE)

## create list with used software
if(!is.null(add.software)) .software_names<-c(.software_names,add.software)
x<-which.term(x,.software_names,tolower=FALSE,hits_only=TRUE)
# remove second and further software name definitions after "|"
x<-gsub("\\|.*","",x)
# convert specific names
x[grep("\\[\\^A-Za-z\\]\\[Mm\\]\\[P",x)]<-"Mplus"
x[grep("\\E \\[pP\\]rime",x)]<-"E-Prime"
x<-gsub(".* *G \\*Powe.*","G*Power",x)
# remove or convert [] in software names
x<-gsub("\\[\\^A-Z\\]|\\[\\^a-z\\]| $","",x)
x<-gsub("\\[DA\\]\\[AD\\]","DA",gsub("\\[Kk\\]","K",x))
x<-gsub("\\[Tt\\]","T",gsub("\\[Ss\\]","S",gsub("\\[Ff\\]","F",x)))
x<-gsub("\\[Nn\\]","n",gsub("\\[Cc\\]","C",x))
x<-gsub("\\[Vv\\]","V",gsub("\\[Ll\\]","L",x))
x<-gsub("\\[\\^[A-Z][a-z]\\]","",gsub("\\[Pp\\]|\\[pP\\]","P",x))
x<-gsub("\\[\\+\\]","+",x)
x<-gsub(" \\[1-9\\]","",x)
x<-gsub(" Core| Institute","",x)
x<-gsub(" \\[1\\-9vV\\]","",x)
x<-gsub("\\[- \\]\\*","-",x)
x<-gsub("\\[1-9\\]","",x)

return(unique(unlist(x)))

}


.software_names<-c(
# software to process and analyse data
"SPSS|Statistical Package for the Social Sciences|PASW|Predictive Analytics Soft[Ww]are",
"R Core|using R[,\\.]| in R[,\\.]|[^a-zA-Z][Rr] [Pp]roject|oftware '*R'*[^a-zA-Z]|[A-Za-z]R Foundation| with R[,\\.]|R Development Core|[^A-Z0-9] R \\([Vv1-4]| R [0-9]\\.[0-9]| R[0-9]\\.[0-9]|[Ss]oftware R[^a-zA-z]|[^a-zA-Z]R [Ss]oftware|anguage R|R [Ff]oundation|in R [Vv]ersion|with R [Vv]ersion| in R with ",
"[^A-Z]SPM[1-9]|[^A-Z]SPM.[0-9]|^SPM[1-9]|^SPM.[0-9]|[sS]tatistical [pP]arametric [mM]apping",
"Stata[^a-z]|STATA[^A-Z]",
"SAS Institute|Statistical Analysis Software|Statistical Analysis System|[\\( ]SAS [Vv]ersion [1-9]|with SAS [Vv]ersion| in SAS [1-9]|with SAS [1-9]|SAS PROC",
"Statistica |STATISTICA|StatSoft",
"MATLAB|Matlab",
"N[Vv]ivo|N Vivo",
"MAXQ[DA][AD]|MAXqda",
"Acq[Kk]nowledge",
"AFNI",
"Free[Ss]urfer|Free Surfer",
"Graph[Pp]ad|Graph Pad",
"[^A-Z]REST[^A-Z]",
"ImageJ|Image J[^a-z]",
"JMP[^a-z]",
"Excel[^a-z]|[^A-Z]EXCEL|XLSTAT",
"[Ff]MRIB|[^A-Z]FSL[^A-Z]",
"[^A-Za-z]G *Power|^G *Power",
"[^A-Z]PASS[^A-Z]",
"[^A-Z]NCSS[^A-Z]",
"Praat|PRAAT",
"AMOS|Amos[^a-z]",
"LISREL|Lisrel[^a-z]",
"[^A-Za-z][Mm][Pp][Ll][Uu][Ss]|[^A-Za-z][Mm] [Pp][Ll][Uu][Ss]",
"JASP",
"Kubios", # HRV,
"Neuroscan",
"DMDX",
"Python|PYTHON",
"EEGLAB",
"MRI[Cc]ro[Nn]",
"MRI[Cc]ro[^Nn]",
"StudSize",
"Psychtoolbox|Psych [Tt]oolbox|Psych[tT]oolbox",
"Psy[Ss]cope|Psy [Ss]cope",
"Systat|SigmaPlot|SigmaStat|SYSTAT",
"MLwiN|ML[wW][Ii][nN]",
"WINanalyze|Win[- ]*Analyze",
"C[+][+]",
"Tanagra",
"OptoGait",
"Statgraphics|StatGraphics",
"EQS[^A-Z]",
"Smart PLS|SmartPLS",
"Warp PLS|WarpPLS",
"Winsteps|WinSteps|WINSTEPS",
"MindWare|Mindware|MINDWARE",
"Cartool",
"RDSAT",
"PROCESS MACRO|PROCESS *[Mm]acro|PROCESS [Mm]odul|PROCESS .*Hayes|Hayes.*PROCESS|Process *[Mm]acro|PROCESS *[Tt]oolbox|PROCESS [a-z]* *tool",
"FACTOR|oftware Factor|Factor [Ss]oftware|Factor [1-9]\\.[0-9]|Factor [1-9][0-9]\\.[0-9]",
"METAWIN|META[- ]*WIN",
"Review Manager|REVIEW MANAGER",
"HLM [1-9vV]|HLM[Pp]rogram|HLM [Ss]oft|[Hh]ierarchical [Ll]inear [Mm]odeling [Pp]rogram|[Hh]ierarchical [Ll]inear [Mm]odeling [Ss]oft",
"QtiPlot",
"Grapher[^a-z]",
"Open[- ]*MX"

## software to present experiments/gather data
#"Presentation",
#"Tobii|TOBII",
#"E [pP]rime|E[- ]*[pP]rime",
#"LimeSurvey|Lime[- ]*[Ss]urvey",
#"SurveyMonkey|[Ss]urvey[- ]*[Mm]onkey",
#"Qualtrics|QUALTRICS",
#"Super[Ll]ab|SUPERLAP",
#"Amazon Mechanical Turk|[mM]echanical [tT]urk|Amazon MT",

)

