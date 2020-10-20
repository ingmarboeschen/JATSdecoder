#' allStats
#'
#' Extract statistical results from text with some uniformisation
#' @param x text to extract statistical results from
#' @export
#' @examples
#' x<-c("The mean difference of scale A was significant (beta=12.9, t(18)=2.5, p<.05)",
#' "The ANOVA yielded significant results on 
#'  faktor A (F(2,18)=6, p<.05, eta(g)2<-.22)",
#' "the correlation of x and y was r=.37.")
#' allStats(x)
allStats<-function(x){
if(length(x)>0) x[is.na(x)]<-""
# escape 1:
if(sum(length(x)>0&nchar(x)>2)>0){
# letter convert x
x<-letter.convert(x,greek2text=T)
# convert space before/after bracket
x<-gsub("[\\(\\[] ","(",x)
x<-gsub(" [\\)\\]]",")",x)
# unify "[]" -> "()" ??
#x<-gsub("\\[","(", x)
#x<-gsub("\\]",")", x)

x<-gsub(" to ","-", x)

# unify "-"
x<-gsub("\u2012","-", x)
# approximate to equal
x<-gsub("\u2243","=", x)
# precides to < succeeds to >
x<-gsub("\u227a|\u27e8","<", x)
x<-gsub("\u227b",">", x)

# convert [df] to (df)
x<-gsub("([a-zA-Z]) \\[([-\\.,;0-9]*)\\]","\\1 (\\2)",x)
x<-gsub("([a-zA-Z])\\[([-\\.,;0-9]*)\\]","\\1(\\2)",x)

# convert text to sentences if has ". [A-Z]"
if(length(grep("\\. [A-Z]",x))==1) x<-text2sentences(x)
# covert <sub> to "()" 
stats<-gsub(" <sub>|<sub>|<sub> | <sub> ","(",gsub("</sub>| </sub>|</sub> | </sub> ",")",x))
# remove e.g.
stats<-gsub("e\\.g\\. |e\\.g\\.","",stats)
# double bracket to single bracket
stats<-gsub("[(][(]","(",gsub("[)][)]",")",stats))
stats<-gsub("[\\[][\\[]","[",gsub("[\\][\\]","]",stats))
# remove html in inline formula
stats<-lapply(strsplit(stats,"<inline-formula>|</inline-formula>"),function(x) paste(gsub("<mml.*?>|</mml.*?>","",x),collapse=" "))
# remove further html
stats<-gsub("<[a-z/]*?>","",stats)
# correct "- 1.2"-> "-1.2"
stats<-gsub(" - ([0-9\\.])"," -\\1",stats)
# correct bracket and coma use: "( 1 , 305 )" -> "(1, 305)"
stats<-gsub("([\\(]) ([0-9])","\\1\\2",stats)
stats<-gsub("([0-9a-zA-Z]) ,","\\1,",stats)
stats<-gsub(" [\\)]",")",stats)
# remove_
stats<-gsub("_","",stats)
# convert of/was/is + num -> =number
stats<-gsub(" was ([0-9\\.-])","=\\1",stats)
stats<-gsub(" is ([0-9\\.-])","=\\1",stats)
stats<-gsub(" of ([0-9\\.-])","=\\1",stats)
# unify CI use
stats<-gsub("\\[CI: ([0-9\\.])","CI=\\1",stats)
stats<-gsub("([^a-zA-Z])CI:","\\1CI=",stats)

# merge stat and label for rtFQH letter/num (
  stats<-gsub("([^a-zA-Z][rtFQH]) ([a-z0-9] \\([0-9])","\\1\\2",stats)

# remove () around only letters
stats<-gsub("\\(([- A-Za-z]*?)\\)","\\1",stats)
stats<-gsub("\\[([- A-Za-z]*?)\\]","\\1",stats)
# remove "[" in front of t, F, r chi
stats<-gsub("\\[([tFrcZ])","\\1",stats)

# remove "[(" in front of 3 words text
stats<-gsub(" [\\[\\(]([A-Za-z]*? [A-Za-z]*? [A-Za-z]*?)"," \\1",stats)
  # remove [ in front of 2 words
  stats<-gsub("\\[([a-zA-z]*? [a-zA-z]*? )","\\1",stats)
  # remove ; in front of words
  stats<-gsub("\\; ([a-zA-z]*? [a-zA-z]*? )"," \\1",stats)

# convert "num to num" from CI to "num; num"
stats<-gsub("([0-9]) to ([0-9\\.\\-])","\\1; \\2",stats)
# unify chi2
stats<-gsub("chi2 \\(","chi2(",stats)
stats<-gsub("chi \\(","chi(",stats)
stats<-gsub("[Xx]2 \\(","chi2(",stats)
stats<-gsub("[Xx]2\\(","chi2(",stats)
stats<-gsub("[Xx]\\^2\\(|[Xx] \\^2 \\(|[Xx] \\^2\\(|[Xx]\\^2 \\(","chi2(",stats)

# remove '-value'
stats<-gsub("[ \\-]value","",stats) 
# remove '-test'
stats<-gsub("[ \\-]test","",stats) 
# remove '-Letter'
stats<-gsub("-([A-Za-z])"," \\1",stats) 

# sz->beta
stats<-gsub("\u00DF","beta",stats) 

# unify eta and partial eta
stats<-gsub("epsilon","eta",stats)
stats<-gsub("\\(partial\\)","(p)",stats)
stats<-gsub("eta[\\^]2 p|eta[\\^]2p|etap[\\^]2|eta\\(p\\)[\\^]2","eta2(p)",stats)
stats<-gsub("eta\\(p\\)2|eta\\(p\\) 2|eta\\(p2\\)","eta2(p)",stats)
stats<-gsub("eta[\\^]2|eta 2","eta2",stats)

# remove space around between/within partial/change
stats<-gsub(" [Bb]etween | [Bb]etween","between",stats)
stats<-gsub(" [Ww]ithin | [Ww]ithin","within",stats)
stats<-gsub(" [Pp]artial | [Pp]artial","partial",stats)
stats<-gsub(" [Cc]hange | [Cc]hange","change",stats)

# unify coefficients
stats<-gsub("correlation coefficient","r",stats)
stats<-gsub("regression coefficient","beta",stats)
# convert "(p" to ", p"
stats<-gsub(" \\(p([<=>\\.0-9 ]*)\\)",", p\\1",stats)
stats<-gsub(" \\( p([<=>\\.0-9 ]*)\\)",", p\\1",stats)
stats<-gsub(" \\[p([<=>\\.0-9 ]*)\\]",", p\\1",stats)
stats<-gsub(" \\[ p([<=>\\.0-9 ]*)\\]",", p\\1",stats)
stats<-gsub(" \\[p([<=>\\.0-9 ]*)",", p\\1",stats)
stats<-gsub(" \\[ p([<=>\\.0-9 ]*)",", p\\1",stats)

# split before "(p<=" or "(p <="
stats<-unlist(strsplit2(stats,"[(][Pp][=<>]|[(][Pp] [=<>]","before"))
# split after p-value ending with ")"
stats<-unlist(strsplit2(stats,"[Pp][=<>]0[\\.][0-9][0-9][0-9][)]|[Pp][=<>]0[\\.][0-9][0-9][)]|[Pp][=<> ][ =<>]0[\\.][0-9][0-9][0-9][)]|[Pp][=<> ][ =<>]0[\\.][0-9][0-9][)]|[Pp] [=<>] 0[\\.][0-9][0-9][0-9][)]|[Pp] [=<>] 0[\\.][0-9][0-9][)]|[Pp][=<>][\\.][0-9][0-9]\\)|[Pp][=<>][\\.][0-9][0-9][0-9]\\)|[Pp][=<>][\\.][0-9]\\)|[Pp] [=<>] [\\.][0-9][0-9]\\)|[Pp] [=<>] [\\.][0-9][0-9][0-9]\\)|[Pp] [=<>] [\\.][0-9]\\)","after"))
## new # split after "[0-9]); "
stats<-unlist(strsplit2(stats,"[0-9]\\);","after"))
# split at ": "
stats<-unlist(strsplit(stats,"[\\:] "))
# split at "[no letter]see "
stats<-unlist(strsplit(stats,"[^a-zA-Z]see "))

# remove mathjax fomula by splitting, cleaning and collapsing lines with documentclass
stats<-unlist(lapply(lapply(strsplit(stats,"\\\\documentclass|documentclass"),function(x) gsub(".*?\\\\end\\{document\\}","",x)),paste,collapse=" "))

# get lines with stats
stats<-get.sentence.with.pattern(stats,"[=<>]",tolower=FALSE)
# convert " ^" -> "^" 
stats<-gsub(" [\\^]","^",stats)
# space clean up
stats<-gsub("^ *|(?<= ) | *$", "", stats, perl = TRUE)
# unify " , "
stats<-gsub(" , ",", ",gsub(",  ",", ",stats))
# unify "( "
stats<-gsub(" [)]",")",gsub("[(] ","(",stats))
stats<-gsub(" [)]",")",gsub("[(] ","(",stats))


## NEW: 2 and results
# convert special seperator "and" to "&"
if(length(grep("[<=>][ -\\.0-9]*? and [-\\.0-9]*?[,; ]|[<=>][ -\\.0-9]*? and [-\\.0-9]*?$",stats))>0){
  ind<-grep("[<=>][ -\\.0-9]*? and [-\\.0-9]*?[,; ]|[<=>][ -\\.0-9]*? and [-\\.0-9]*?$",stats)
  stats[ind]<-gsub("([<=>][ -\\.0-9]*?) and ([-\\.0-9]*?)","\\1 & \\4",stats[ind])
}

# strsplit at "word, " 
stats<-unlist(strsplit(stats,"[a-z]{5}, "))
# escape 2: go on if something left else go return character(0)
if(length(stats)>0){
stats<-unlist(strsplit(stats," a | as | also | but | in | so | for | were | was | all other | and | or | nor | of | with | respectively| that |versus|which | not | where | than |whereas| factors| condition| [0-9] [0-9] | [0-9]\\. [0-9]\\. | [A-Za-z] [A-Za-z] [A-Za-z] "))
# if line starts with eta paste to line in front
if(length(grep("^eta",stats))>0){
 i<-grep("^eta",stats)
 # remove i=1
 i<-i[i!=1]
 if(length(i)>0){
  stats[i-1]<-paste0(stats[i-1],", ",stats[i])
  stats<-stats[-i]
 }

}


# escape 3: go on if something left else go return character(0)
if(length(stats)>0){
stats<-unlist(strsplit(stats," on | at | the |^the | to | their | than |vs[\\.]|VS[\\.]|signific|significant|xperiment|ypothesis|[Ff]igure [1-9]|id=\\\"[sS]ec[0-9]*{3}"))
# only select lines with number and "=<>"
stats<-get.sentence.with.pattern(stats,"[0-9]")
stats<-get.sentence.with.pattern(stats,"[=<>]")
# escape 4: go on if something left else go return character(0)
if(length(stats)>0){
# spaces before and after "=" and "<>"
stats<-gsub(" [=] | [=]|[=] ","=",stats)
stats<-gsub(" [<] | [<]|[<] ","<",stats)
stats<-gsub(" [>] | [>]|[>] ",">",stats)
# unify "p"
stats<-gsub("^ ","",gsub(" P[<]|^P[<]"," p<",stats))
stats<-gsub("^ ","",gsub(" P[=]|^P[=]"," p=",stats))
stats<-gsub("^ ","",gsub(" P[>]|^P[>]"," p>",stats))
# unify degrees of freedom
#stats<-lapply(stats,function(x) gsub(" [(]","\\(",x))
# unify F
stats<-gsub("^F [(]","F(",stats)
stats<-gsub(" F [(]"," F(",stats)
stats<-gsub("[(]F [(]","F(",stats)
# unify t
stats<-gsub("^t [(]|([^a-z ])t [(]","\\1t(",stats)
stats<-gsub(" t [(]|\\(t [(]"," t(",stats)
# unify R2 and r
stats<-gsub("R 2([<=])|R 2 ([<=])|R \\^2([<=])","R2\\1\\2\\3",stats)
stats<-gsub(" r 2|R 2 |R 2|\\(r 2","R2",stats)
stats<-gsub("^r [(]|( )r [(]","\\1r(",stats)
stats<-gsub("R[\\^ ]2([<=>])|R[\\^ ]2 ([<=>])|R \\^2([<=>])","R2\\1\\2\\3",stats)

# unify chi
stats<-gsub("[Cc]hi \\^2|[Cc]hi 2|[Cc]hi2|[Cc]hi-square|[Cc]hi-square value","chi2",stats)
stats<-gsub("x 2([<>= ])","chi2\\1",stats)
stats<-gsub("X \\^2([=<>])","chi2\\1",stats)
stats<-gsub("chi [(]","chi2(",stats)

# unify eta2
stats<-gsub("eta 2|eta\\^2|eta \\^2","eta2",stats)
stats<-gsub("eta p 2|eta2 p|etap2|eta[(]p[)]2|eta p \\^2|partial eta[ -]squared","eta2(p)",stats)
stats<-gsub("eta G 2|eta[(]G[)]2","eta2(G)",stats)

# correct "=- " 
stats<-gsub("[=][-] ","=-",stats)
stats<-gsub(" [=]","=",stats)

## exclude R 2 (adj)->
# split behind "%)[;,]"
stats<-unlist(strsplit2(stats,"[[:punct:]]\\), ","after"))
# split before bracket with two letters at start 
stats<-unlist(strsplit2(stats," [(][a-zA-Z\u00DF][a-zA-Z0-9\\=]","before"))
# only select lines with number and "="
stats<-get.sentence.with.pattern(stats,"[0-9]")
stats<-get.sentence.with.pattern(stats,"[=<>]")

## split at " t([1-9]"if no "beta" "CI" in line or "^.=" at beginning
index1<-grep(" t\\([1-9]",stats)
index2<-grep("[Bb]eta| CI|confidence|^.=|^..=",stats,invert=TRUE)
index<-index1[is.element(index1,index2)]
# split selected lines
temp<-strsplit2(stats[index]," t\\([1-9]","before",)
# select results only
temp<-lapply(lapply(temp,get.sentence.with.pattern,"[0-9]"),get.sentence.with.pattern,"[=<>]")
# paste with "SPLIT"
temp<-unlist(lapply(temp,paste,collapse="SPLIT"))
# replace results
stats[index]<-temp
# split at "SPLIT"
stats<-unlist(strsplit(stats,"SPLIT"))

# convert =1,000 -> =1000 in lines without "F([1-9]"
Fpattern<-"^[fF][\\[\\(][1-9]|[^a-zA-Z][fF][\\[\\(][1-9]"
if(length(grep(Fpattern,stats,invert=TRUE))>0){
  ind<-grep(Fpattern,stats,invert=TRUE)
  stats[ind]<-gsub("([0-9]),([0-9]{3})","\\1\\2",stats[ind])
  stats[ind]<-gsub("([0-9]),([0-9]{3})","\\1\\2",stats[ind])
 }

# convert =1,000 -> =1000 after "="
stats<-gsub("([<=>][0-9]*?),([0-9]{3})","\\1\\2",stats)
stats<-gsub("([<=>] [0-9]*?),([0-9]{3})","\\1\\2",stats)

# convert df=1,000 -> df=1000 in lines with high degrees of freedom in F stats
i<-grep("[Ff][(\\[][0-9]*?;[ 0-9]*{3}|[Ff][(\\[][0-9]*?,[0-9]*{3};[ 0-9]",stats)
if(length(i>1)){
  stats[i]<-gsub("([0-9]; [0-9]*?),([0-9]*{3})","\\1\\2",stats[i])
  stats[i]<-gsub("([0-9]; [0-9]*?),([0-9]*{3})","\\1\\2",stats[i])
  stats[i]<-gsub("([0-9]*?),([0-9]*{3};[ 0-9])","\\1\\2",stats[i])
} 
 
# split at ";" if line has two p-values and only one ";"
while(sum(nchar(stats)-nchar(gsub("p[<>=]","",stats))==4&nchar(stats)-nchar(gsub("[;]","",stats))==1)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub("p[<>=]","",stats))==4&nchar(stats)-nchar(gsub("[;]","",stats))==1][1]
  stats<-c(stats[(1:length(stats))<ind],unlist(strsplit(stats[ind],";")),stats[(1:length(stats))>ind])
  }

# split at ";" if line has three p-values and only two ";"
while(sum(nchar(stats)-nchar(gsub("p[<>=]","",stats))==6&nchar(stats)-nchar(gsub("[;]","",stats))==2)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub("p[<>=]","",stats))==6&nchar(stats)-nchar(gsub("[;]","",stats))==2][1]
  stats<-c(stats[(1:length(stats))<ind],unlist(strsplit(stats[ind],";")),stats[(1:length(stats))>ind])
}
# split at ";" if line has four p-values and only three ";"
while(sum(nchar(stats)-nchar(gsub("p[<>=]","",stats))==8&nchar(stats)-nchar(gsub("[;]","",stats))==3)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub("p[<>=]","",stats))==8&nchar(stats)-nchar(gsub("[;]","",stats))==3][1]
  stats<-c(stats[(1:length(stats))<ind],unlist(strsplit(stats[ind],";")),stats[(1:length(stats))>ind])
}

# if line has multiple F([1-9] values split before F([1-9]
while(sum(nchar(stats)-nchar(gsub("[( ]F[(][1-9]|^F[(][1-9]","",stats))>4)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub("[( ]F[(][1-9]|^F[(][1-9]","",stats))>4][1]
  stats<-  c(stats[(1:length(stats))<ind],unlist(strsplit2(stats[ind],"[( ]F[(][1-9]|^F[(][1-9]","before")),stats[(1:length(stats))>ind])
}

# if line has multiple F[0-9a-z]([1-9] values split before F[0-9a-z]([1-9]
while(sum(nchar(stats)-nchar(gsub("[( ]F[0-9a-z][(][1-9]|^F[0-9a-z][(][1-9]","",stats))>5)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub("[( ]F[0-9a-z][(][1-9]|^F[0-9a-z][(][1-9]","",stats))>5][1]
  stats<-  c(stats[(1:length(stats))<ind],unlist(strsplit2(stats[ind],"[( ]F[0-9a-z][(][1-9]|^F[0-9a-z][(][1-9]","before")),stats[(1:length(stats))>ind])
}

# if line has multiple chi2([1-9] values split before chi2([1-9]
while(sum(nchar(stats)-nchar(gsub("[( ]chi2[(][1-9]|^chi2[(][1-9]","",stats))>8)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub("[( ]chi2[(][1-9]|^chi2[(][1-9]","",stats))>8][1]
  stats<-  c(stats[(1:length(stats))<ind],unlist(strsplit2(stats[ind],"[( ]chi2[(][1-9]|^chi2[(][1-9]","before")),stats[(1:length(stats))>ind])
}

# if line has multiple chi([1-9] values split before chi([1-9]
while(sum(nchar(stats)-nchar(gsub("[( ]chi[(][1-9]|^chi[(][1-9]","",stats))>8)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub("[( ]chi[(][1-9]|^chi[(][1-9]","",stats))>8][1]
  stats<-  c(stats[(1:length(stats))<ind],unlist(strsplit2(stats[ind],"[( ]chi[(][1-9]|^chi[(][1-9]","before")),stats[(1:length(stats))>ind])
}
# if line has multiple r([1-9] values split before r([1-9]
while(sum(nchar(stats)-nchar(gsub("[( ]r[(][1-9]|^r[(][1-9]","",stats))>5)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub("[( ]r[(][1-9]|^r[(][1-9]","",stats))>5][1]
  stats<-  c(stats[(1:length(stats))<ind],unlist(strsplit2(stats[ind],"[( ]r[(][1-9]|^r[(][1-9]","before")),stats[(1:length(stats))>ind])
}

# if line has multiple p values with 2 and 3 digits split after "p=.num" or p=0.num
while(sum(nchar(stats)-nchar(gsub(" p[<=>][.][0-9]| p[<=>]0[.][0-9]","",stats))>8)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub(" p[<=>][.][0-9]| p[<=>]0[.][0-9]","",stats))>8][1]
  stats<-  c(stats[(1:length(stats))<ind],unlist(strsplit(gsub("( p[<=>][.][0-9]*?)([^0-9])|( p[<=>]0[.][0-9]*?)([^0-9])", "\\1\\3 SPLITs \\2\\4",stats[ind]),"SPLITs")),stats[(1:length(stats))>ind])
}


# if line has multiple p values with 2 and 3 digits split after "p<=>.num" or p<=>0.num
while(sum(nchar(stats)-nchar(gsub(" p<=>[.][0-9]| p<=>0[.][0-9]","",stats))>10)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub(" p<=>[.][0-9]| p<=>0[.][0-9]","",stats))>10][1]
  stats<-  c(stats[(1:length(stats))<ind],unlist(strsplit(gsub("( p<=>[.][0-9]*?)([^0-9])|( p<=>0[.][0-9]*?)([^0-9])", "\\1\\3 SPLITs \\2\\4", stats[ind]),"SPLITs")),stats[(1:length(stats))>ind])
}

# if line has multiple p values with 2 and 3 digits split after "p<=.num" or p=>0.num
while(sum(nchar(stats)-nchar(gsub(" p[<=>]{2}[.][0-9]| p[<=>]{2}0[.][0-9]","",stats))>9)>0){
  ind<-(1:length(stats))[nchar(stats)-nchar(gsub(" p[<=>]{2}[.][0-9]| p[<=>]{2}0[.][0-9]","",stats))>9][1]
  stats<-  c(stats[(1:length(stats))<ind],unlist(strsplit(gsub("( p[<=>]{2}[.][0-9]*?)([^0-9])|( p[<=>]{2}0[.][0-9]*?)([^0-9])", "\\1\\3 SPLITs \\2\\4", stats[ind]),"SPLITs")),stats[(1:length(stats))>ind])
}

# space clean up
stats<-gsub("^ *|(?<= ) | *$", "", stats, perl = TRUE)
# delete "(" at start of line
stats<-gsub("^ |^[(]|^ [(]","",stats)
# remove till (F( or (t(
stats<-gsub(".*[(]F[(]","F(",stats)
stats<-gsub(".*[(]t[(]|.*[(]t [(]","t(",stats)
stats<-gsub(".*[(]T[(]|.*[(]T [(]","T(",stats)
# remove numbers in front of line
stats<-gsub("^[0-9]*","",stats)

# only select lines with number and "="
stats<-get.sentence.with.pattern(stats,"[0-9]")
stats<-get.sentence.with.pattern(stats,"[=<>]")

# escape 5
if(length(stats)>0){
# remove front words
stats<-remove.front(stats)
# remove text at end until last found number or % in line
stats<-lapply(stats,function(x){a<-x; while(length(grep("[^0-9\u0025\\]]$",a))>0) a<-gsub("[^0-9\u0025\\]]$","",a); return(a)})
# remove text til "(" in lines with no ")"
stats<-lapply(stats,function(x){ if(length(grep("[)]",x,invert=TRUE))>0&length(grep("[(]",x))>0) {
i<-(1:length(x))[grep("[(]",x)]; i<-i[grep("[)]",x[i],invert=TRUE)]
x[i]<-gsub(".*[(]","",x[i])
}
return(x)
})

# remove text behind ")" in lines with no "("
stats<-lapply(stats,function(x){ if(length(grep("[(]",x,invert=TRUE))>0&length(grep("[)]",x))>0) {
i<-(1:length(x))[grep("[)]",x)]; i<-i[grep("[(]",x[i],invert=TRUE)]
x[i]<-gsub("[)].*","",x[i])
}
return(x)
})

# remove text til "[" in lines with no "]"
stats<-lapply(stats,function(x){ if(length(grep("\\]",x,invert=TRUE))>0&length(grep("\\[",x))>0) {
i<-(1:length(x))[grep("\\[",x)]; i<-i[grep("\\]",x[i],invert=TRUE)]
x[i]<-gsub(".*\\[","",x[i])
}
return(x)
})

# remove non letters in front
stats<-unlist(lapply(stats,function(x){while(length(grep("^[^a-zA-Z\u00DF\\|]",unlist(x))>0)) x<-gsub("^[^a-zA-Z\u00DF]","",unlist(x)); return(x)}))

# split at seperator follwed by long word
stats<-unlist(strsplit(stats,"[,;] [a-z]{7}"))

# only select lines with number after "=<>" 
stats<-get.sentence.with.pattern(stats,"[=<>][-\\.0-9]")

# remove emty lines
stats<-stats[nchar(unlist(stats))>2]

# unify F(num, num) ->F(num,num) 
stats<-gsub("(F\\([0-9\\.]*?,) ([0-9\\.]*?\\))","\\1\\2",stats)

# unify chi ( again
stats<-gsub("chi [(]|X2 [(]","chi(",stats)
# remove space in front of number ??
#stats<-gsub("[^&] ([0-9])","\\1",stats)

# unify p-values
stats<-gsub("[Pp][- ][- ][Vv]alue|[Pp][- ][Vv]alue","p",stats)
stats<-gsub("^P[=]| P[=]","p=",stats)
stats<-gsub("^P[<]| P[<]","p<",stats)
stats<-gsub("^P[>]| P[>]","p>",stats)
stats<-gsub("^p s([<=>])","ps\\1",stats)
stats<-gsub(" p s([<=>])"," ps\\1",stats)
# unify F s values
stats<-gsub("^F s([<=>])","Fs\\1",stats)
stats<-gsub(" F s([<=>])"," Fs\\1",stats)

# unify eta(p)
stats<-gsub("partial eta2|eta[(]p[)]2|eta p2|etarho \\^2|eta p\\^2|etarho2","eta2(p)",stats)

# unify BayesFactor
stats<-gsub(" BF01| BF0[\\+]"," BF(01)",gsub("^BF01|^BF0[\\+]","BF(01)",stats))
stats<-gsub(" BF10"," BF(10)",gsub("^BF10","BF(10)",stats))

# remove figure at line start
stats<-gsub("^[Ff]igure","",stats)

# find lines that have a "[" after removal till "]"
i1<-grep("\\][,;]",stats)
i2<-grep("\\[",gsub(".*\\]","",stats))
i<-i1[is.element(i1,i2)]
# split at ]; or ], for identified lines
stats[i]<-lapply(stats[i],function(x) unlist(strsplit(x,"\\][,;]")))

# split before ": r="
stats<-unlist(lapply(stats,strsplit2,"[:] r[=]","before"))

# remove non letters in front again
stats<-unlist(lapply(stats,function(x){while(length(grep("^[^a-zA-Z\u00DF\\|]",unlist(x))>0)) x<-gsub("^[^a-zA-Z\u00DF\\|]","",unlist(x)); return(x)}))
# remove text at end until last found number or % in line
stats<-unlist(lapply(stats,function(x){a<-x; while(length(grep("[^0-9\u0025]$",a))>0) a<-gsub("[^0-9\u0025]$","",a); return(a)}))

# unify chisq to 'statcheck' readable notation
stats<-lapply(stats,function(x) gsub("chi\\(|chi \\^2 \\(|chi\\^2\\(|[Xx]\\^2\\(","chi2(",x))
stats<-lapply(stats,function(x) gsub("chi=|[Xx]2=","chi2=",x))
stats<-lapply(stats,function(x) gsub("chi<|[Xx]2<","chi2<",x))
stats<-lapply(stats,function(x) gsub("chi>|[Xx]2>","chi2>",x))
stats<-lapply(stats,function(x) gsub("[Xx]2 [(]|[Xx]2[(]","chi2(",x))
# unify R2
stats<-lapply(stats,function(x) gsub("[Rr][ -][Ss]quare[=]|r \\^2[=]|r\\^2[=]|[Rr][ -][Ss]quared[=]","R2=",x))
stats<-unlist(lapply(stats,function(x) gsub("R adj2|R[(]adj[)]|R[(]adj[)]2|[Aa]djusted R2","R2(adj)",x)))

# only select lines with number and "="
stats<-get.sentence.with.pattern(stats,"[0-9]")
stats<-get.sentence.with.pattern(stats,"[=<>]")

# add space to ",95%"
stats<-gsub("([,;])([0-9\\.]*?\u0025)","\\1 \\2",stats)

# split line at "; " if has only 1 "; [^-0-9\\.]" and at least 2 ", "??
Nsemi<-nchar(stats)-nchar(gsub("; [^-0-9\\.]","",stats))
Ncomma<-nchar(stats)-nchar(gsub(", [^-0-9\\.]","",stats))
ind<-which(Nsemi==2&Ncomma>=4)
stats[ind]<-gsub("; ","SPLIT",stats[ind])
stats<-unlist(strsplit(stats,"SPLIT"))


## clean up
# remove bad captured lines: letter-space-letter-space-letter
stats<-grep("[a-zA-Z] [a-zA-Z] [a-zA-Z] [a-zA-Z]",stats,invert=TRUE,value=TRUE)
# remove bad captured lines: num space num space num  from tables in pdf documents
stats<-grep("\\.[0-9][0-9] \\.[0-9][0-9] \\.[0-9][0-9] |\\.[0-9] \\.[0-9] \\.[0-9] ",stats,invert=TRUE,value=TRUE)
stats<-grep("\\.[0-9][0-9][0-9] \\(\\.[0-9][0-9][0-9]\\) ",stats,invert=TRUE,value=TRUE)
# remove potential lines from tables
stats<-grep(" [a-zA-Z] \\.[0-9] \\.[0-9] ",stats,invert=TRUE,value=TRUE)
stats<-grep(" [a-zA-Z] [a-zA-Z] [a-zA-Z] ",stats,invert=TRUE,value=TRUE)

# remove lines with "at p<"
stats<-grep("^at p[<]",stats,invert=TRUE,value=TRUE)
# remove text till [:,;] if no number exists in front of [:,;]
i<-grep("[0-9]",gsub("[:,;].*","",stats),invert=TRUE)
stats[i]<-sub(".*?[:,;]","",stats[i])
# remove lines with stars
stats<-grep("\\*",stats,invert=TRUE,value=TRUE)
# remove space in front
stats<-gsub("^ ","",stats)
# remove from "; [Ss]ee"
stats<-gsub("[;,] [Ss]ee.*","",stats)

# add "]" in lines with "CI" and "[" but no "]"
stats<-gsub("(\\[[0-9\\. ,;\\-]*?)( [a-z])","\\1]\\2",stats)
stats<-gsub("(\\[[0-9\\. ,;\\-]*?)$","\\1]\\2",stats)

# add space to ",95%"
stats<-gsub("([,;])([0-9\\.]*?\u0025)","\\1 \\2",stats)

# add ")" in CI=( without (
stats<-gsub("(CI[:= ][(][0-9, ;\\.\\-]*?$)","\\1)",stats)
stats<-gsub("(CI[:= ][(][0-9, ;\\.\\-]*?), ([^0-9\\.\\-])","\\1), \\2",stats)
stats<-gsub("(CI[(][0-9, ;\\.\\-]*?$)","\\1)",stats)
stats<-gsub("(CI[(][0-9, ;\\.\\-]*?), ([^0-9\\.\\-])","\\1), \\2",stats)

# remove till "(" if has no ")"
i<-grep("[\\)]",stats,invert=T)
stats[i]<-gsub(".*[\\(]","",stats[i])

# remove till [(F( and [(t(, r, M, chi2, chi  
stats<-gsub(".*[\\(\\[]F\\(","F(",stats)
stats<-gsub(".*[\\(\\[]t\\(","t(",stats)
stats<-gsub(".*[\\(\\[]T\\(","T(",stats)
stats<-gsub(".*[\\(\\[]r\\(","r(",stats)
stats<-gsub(".*[\\(\\[]B\\(","B(",stats)
stats<-gsub(".*[\\(\\[]BF\\(","BF(",stats)
stats<-gsub(".*[\\(\\[]chi2\\(","chi2(",stats)
stats<-gsub(".*[\\(\\[]chi\\(","chi(",stats)
stats<-gsub("^[^<=>]*?[\\(\\[]M([<=>])","M\\1",stats)
# remove till [(F[a-zA-Z0-9]( and [(t[a-zA-Z0-9]( 
stats<-gsub(".*[\\(\\[]F([a-zA-Z0-9]*?\\([1-9])","F\\1",stats)
stats<-gsub(".*[\\(\\[]t([a-zA-Z0-9]*?\\([1-9])","t\\1",stats)
stats<-gsub("[^0-9*?]*[\\(\\[]r([\\(<=>])","r\\1",stats)
stats<-gsub("[^0-9*?]*[\\(\\[]([Bb][\\(<=>])","\\1",stats)
# remove til stat if has no number in front
stats<-gsub("^[^0-9]* ([tQcrRFZzbB\u0392])","\\1",stats)
stats<-gsub("^[^0-9]* \\(([tQcrRFZzbB\u0392])","\\1",stats)

# clean up/unify chi2
stats<-gsub("chi2\\(([0-9\\.]*, df)","chi2=\\1",stats)
stats<-gsub("chi\\(([0-9\\.]*, df)","chi2=\\1",stats)
stats<-gsub("chi2([<=>]*[0-9\\.]*) (\\([0-9]*\\))","chi\\2\\1",stats)
stats<-gsub("chi([<=>]*[0-9\\.]*) (\\([0-9]*\\))","chi\\2\\1",stats)

# remove F values with only one df
stats<-gsub("F\\([0-9]*?,[^0-9]*?\\)","",stats)
stats<-gsub(",F",", F",stats)
# remove empty brackets
stats<-gsub("\\(\\)","",stats)
stats<-gsub("  "," ",stats)
# remove lines with web content
stats<-grep("www\\.|http",stats,invert=TRUE,value=TRUE)

# select lines with "letter[<=>]" or ")[<=>]"  or "2[<=>]" only
stats<-grep("[2a-zA-Z\\)][<=>]",stats,value=TRUE)
# correct comma use
stats<-gsub("([0-9]),([a-zA-Z])","\\1, \\2",stats)
# remove [ in front of CI
stats<-gsub("\\[CI","CI",stats)
stats<-gsub("\\[ci","ci",stats)
stats<-gsub("\\[Ci","Ci",stats)

# add space between CI(num) 
stats<-gsub("CI([0-9\\.])","CI \\1",stats)
# unify CI annotation
#stats<-gsub("(CI[= :(;0-9\\.\\-\\+]+[^)])+, ","\\1), ",stats)
stats<-gsub("(CI[=:])([^(][0-9\\.\\-\\+]+\\))","\\1(\\2, ",stats)
stats<-gsub("(CI[=:][(][-0-9\\.\\+,; ].*)(, [a-zA-Z])","\\1)\\2",stats)
stats<-gsub("(CI[=:][\\[][-0-9\\.\\+,; ].*)(, [a-zA-Z])","\\1]\\2",stats)

# remove \n
stats<-gsub("\\n","",stats)

#  remove only text in front of [ if has no ]
stats<-gsub("[a-zA-Z ].*\\[([a-ce-mo-zA-CE-MO-Z])","\\1",stats)

# remove space after "^2 ("
stats<-gsub("[\\^]2 ([\\[\\(\\[])","^2\\1",stats)

# correct "--" -> "-"
stats<-gsub("--","-",stats)
# reconvert [0-9] & [-\\.0-9] to "and 
stats<-gsub("([0-9]) & ([-\\.0-9])","\\1 and \\2",stats)
# double bracket to single bracket
stats<-gsub("[(][(]","(",gsub("[)][)]",")",stats))
stats<-  gsub("\\[\\[","[",gsub("\\]\\]","]",stats))

# output
return(unlist(stats))

} else return(character(0)) # escape 5
} else return(character(0)) # escape 4
} else return(character(0)) # escape 3
} else return(character(0)) # escape 2
} else return(character(0)) # escape 1
  
}

## included function: remove.front()
# function to remove words before stats
remove.front<-function(x,n=2){
if(length(x)>0){
if(sum(!is.na(x))>0){
x<-x[!is.na(x)]
# select text before and behind [=<>]
y<-unlist(lapply(strsplit(x,"[=<>0-9(]"),"[",1))
z<-unlist(lapply(lapply(strsplit2(x,"[=<>0-9(]","before"),"[",-1),paste,collapse=""))
# remove first word until n left and than till ,
# if y is not NA
if(sum(!is.na(y))>0){
y[is.na(y)]<-""
while(sum(nchar(y)-nchar(gsub(" ","",y))>n)>0) {
 i<-(1:length(y))[nchar(y)-nchar(gsub(" ","",y))>=n]
 y[i]<-sub(".*? ","",y[i])
}
y<-gsub(".*[,] ","",y)
return(paste(y,z,sep=""))
}else return(NA)
}else return(NA)
}else return(NA)
}

