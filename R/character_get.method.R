#' get.method
#'
#' Extracts statistical methods mentioned in text.
#' @param x text to extract statistical methods from.
#' @param add possible new end words of method as vector.
#' @param cermine Logical. If TRUE CERMINE specific letter conversion will be performed.
#' @importFrom stats na.omit
#' @export
#' @examples
#' x<-"We used multiple regression analysis and 
#' two sample t tests to evaluate our results."
#' get.method(x)

get.method<-function(x,add=NULL,cermine=FALSE){
# unlist
if(is.list(x)) x<-unlist(x)
# remove figs
method<-gsub("<fig.*?.*/fig>","",x)
# lowerize
method<-tolower(method)
#remove html
method<-gsub("<.*?.*>]","",method)

method<-gsub(" indices"," index",method)

# split lines
method<- unlist(strsplit(method,"\\, |; | or |\\: | and | as | by | to | is | are | have | has | was | were | be | in | on | also | with |/| [(\\[]| based on | beside"))
  # set "-" to space
  method<- gsub("-"," ",gsub("[\u2013]"," ",method))
  method<- gsub("[\U2015]"," ",gsub("[\U2014]"," ",method))

# convert "bayes factor" to "bayesfactor"
  method<- gsub("information criterion","informationcriterion",gsub("bayes factor","bayesfactor",method))
# convert "area under the curve" 
  method<- gsub("area under the curve","areaunderthe curve",method)

  
## Extract potential lines with "method" used
# define methods to look out for
patterns<-"regression|anova|ancova|correlation|model|analys[ie]s|test|estimator|estimates|estimation|coefficient|algorithm|simulation|statistic[^a]|statistic$|index|imputation|bootstrap|interval|method|theorem|review|informationcriterion|bayes|curve|matrix"
# add new patterns
if(!is.null(add)) patterns<-paste(c(patterns,add),collapse="|")
# reduce to sentences with patterns
method<-unlist(lapply(method,get.sentence.with.pattern,patterns,tolower=T))

  
# only go on if pattern match in sentence
if(sum(nchar(method))>4){
  ## unifying and clean up
  # remove "'s" or "'"
  method<- gsub("['\u2019\u00B4]s|['\u2019\u00B4]","",method)
  # set "*" to "times"
  method<- gsub("\\*"," times ",method)
  # remove html
  method<- gsub("<.*?.*>"," ",method)
  # remove text within brackets
  method<- gsub("[(].*?.*[)]"," ",method)
  # convert hex letters 
  method<-unlist(lapply(method,letter.convert,cermine=cermine))
  # remove some punctuations
  method<- gsub("[,;:_\\'\"-]|\\)|\\(|\\{|\\}|\\]|\\[|\\+|\\.$"," ",method)
  # space clean up
  method<-gsub("^ *|(?<= ) | *$", "", method, perl = TRUE)
  # some plural to singular
  method<- gsub("analyses","analysis",method)
  method<- gsub("anovas","anova",method)
  method<- gsub("indexes","index",method)
  method<- gsub("correlations","correlation",method)
  method<- gsub("models","model",method)
  method<- gsub("curves","curve",method)
  method<- gsub("algrorithms","algrorithm",method)
  method<- gsub("regressions","regression",method)
  method<- gsub("methods|methodology","method",method)
  method<- gsub("coefficients","coefficient",method)
  method<- gsub("imputations","imputation",method)
  method<- gsub("bayes factors","bayes factor",method)
  
  method<- gsub("tests","test",method)
  # unify anova/ancova
  method<- gsub("analysis of variances|analysis of variance|variance analysis","anova",method)
  method<- gsub("analysis of covariances|ancovas","ancova",method)
  # add space for two word methods
  method<- gsub("clusteranalysis","cluster analysis",method)
  method<- gsub("metaanalysis","meta analysis",method)
  method<- gsub("survivalanalysis","survival analysis",method)
  method<- gsub("clusteranalysis","cluster analysis",method)
  method<- gsub("poweranalysis","power analysis",method)
  method<- gsub("ldamodel","lda model",method)
  method<- gsub("randomeffect","random effect",method)
  # uniformisation
  method<- gsub("goodness of fit|goodness fit","gof",method)
  method<- gsub("a priori","a-priori",method)
  method<- gsub("a posteri","a-posteri",method)
  method<- gsub("posthoc","post hoc",method)

  # space clean up
  method<-gsub("^ *|(?<= ) | *$", "", method, perl = TRUE)
  
  ## special cleanup for cermine converted files
  if(cermine==TRUE){
    # correct wrong capture "3 9 2" -> "3*2" 
    while(length(grep("([0-9]) 9 ([0-9])",method))>0)  method<-gsub("([0-9]) 9 ([0-9])","\\1\\*\\2",method)
    
  }
  ## get ngram around patterns
  ngrams<-method
  ngram<-unlist(lapply(method,ngram,patterns,c(-7,0),tolower=TRUE))
  # clean up
  # remove till prewords and output unique methods
  rm.prewords<-function(x,prewords){ 
  x<-unlist(x)
  # for patterns inside line
  x<-gsub(gsub("|$","",paste(paste(".* ",prewords," |",sep=""),collapse="")),"",x)
  # for patterns at beginning of line
  x<-gsub(gsub("|$","",paste(paste("^",prewords," |",sep=""),collapse="")),"",x)
  # for patterns at end of line
  x<-gsub(gsub("|$","",paste(paste(" ",prewords,"$|",sep=""),collapse="")),"",x)
  return(unique(x))
  }
  
  ## further clean up
  # remove text until potential preverbs
  preverbs1<-c("computed","computing","compute","conduct","conducting","conducted","using","performed","analyzed","perform","used","resulting","estimate","underwent","entering","requiring","carried out","preferred","estimated","fitted","ran","run","did","optain","optained", "use","uses","used","utilised","yielding","undertaking","underlying","regarding","reviewed")
  preverbs2<-c("be","were","following","utilizing","fitting","apply","applied","applying","performing","include","including","included","running","present","report","provide", "provides","examine","examined","assessed","calculating","calculated","describe", "providing","proposed","modelled","assuming","will","corresponding","select","selected","separate")
  preverbs3<-c("have","had","has","considered","contains","showed","tested","was","aided","assist","tested","testing","expected","giving","consider","gave","give","given","gives","revealed","reveales","indicated","hypothesized","reran","utilized","reported","reports","reporting","planned","comprised","related","displays","show")
  preverbs4<-c("developed","entering","mentioned","compare","compared","contrasted","acceptable","completed","shows","containing","analysed","anote","represents","employed","return","specified","created","referred","breaking","could","might","presented","producing","associated","examines")
  ngram<-lapply(ngram,function(x) rm.prewords(x,preverbs1))
  ngram<-lapply(ngram,function(x) rm.prewords(x,preverbs2))
  ngram<-lapply(ngram,function(x) rm.prewords(x,preverbs3))
  ngram<-lapply(ngram,function(x) rm.prewords(x,preverbs4))
  # remove text until potential prewords
  prewords1<-c("a","an","as","also","and","are","s","its","to","the","these","those","this","that","which","thus","at","but","than","then","through","no","were","by","on","in","all","so","for","from","each","we","our","their","with","or","normal","simple","new","next","via", "whether","unknown","another", "such","strongest")
  prewords2<-c("previous","preliminary","relevant","possible","potential","whenever","whereas","whoose","average", "initial","because","other","into","similar","several","common","moderate","strong","high","low","higher","lower","positive", "pre","not","excellent", "pilot","about","after","institutional")
  #,"first","second","third","fourth","fifth","lowest"
  prewords3<-c("final","while","when","where","weak","weaker","weakest","various","further","true","traditional","any","accordingly", "nvivo", "conventional","main","primary","later","until","recent","detailed","either","particular","subsequent","trails","how", "other","additional","although")
  prewords4<-c("collection","before","statistical","statistically","greatest","latest","pre","same","simple","both","would", "appropriate","some","original","hypothesis","either","current","accross","stranger","every","see","significant","insignificant", "nonsignificant","significantly","consequently") 
  ngram<-lapply(ngram,function(x) rm.prewords(x,prewords1))
  ngram<-lapply(ngram,function(x) rm.prewords(x,prewords2))
  ngram<-lapply(ngram,function(x) rm.prewords(x,prewords3))
  ngram<-lapply(ngram,function(x) rm.prewords(x,prewords4))
  # remove if not a priori
# ngram<-lapply(ngram,function(x) gsub("a []","",x))
  # remove words with " of " behind
  ofwords1<-paste(c("magnitude","step","lack","evaluation","time","total","presence","case","choice","significance","kind", "measure","range","goal","form","application","assessment","interpretation","phase","distribution","mean","means","details","consisted","consequence","set"), "of")
  ofwords2<-paste(c("problem","end","types","purposes","series","instead","results","set","type","method","order","purpose","unit","number","stage","level","pattern","degree","strength","result","regardless","selection","advantage","assumptions","assumption","examinations","examination","product","variety"),"of")
  ngram<-lapply(ngram,function(x) rm.prewords(x,ofwords1))
  ngram<-lapply(ngram,function(x) rm.prewords(x,ofwords2))
  
  # remove text until potential presigns und punctuations
  presigns<-c(".*[<>=]","")
  ngram<-lapply(ngram,function(x) rm.prewords(x,presigns))
  # remove until number with "." or ": "
  ngram<-lapply(ngram,function(x) gsub(".*\\.[0-9]|.*\\.[0-9][0-9]|.*\\: ","",x))
  # remove until punctuation
  ngram<-lapply(ngram,function(x) gsub(".*[\\.,]","",x))
  # remove lines with "significantly"
  ngram<-lapply(ngram,function(x) grep("significantly",x,invert=TRUE,value=TRUE))
  ## further unifying
  # specific unifying after inspection
  ngram<-lapply(ngram,function(x) gsub("-"," ",x))
  ngram<-lapply(ngram,function(x) gsub("^rm | rm "," repeated measures ",x))
  ngram<-lapply(ngram,function(x) gsub("repeated measure |repeated measurement|repeated measures[^ ]","repeated measures ",x))
  ngram<-lapply(ngram,function(x) gsub("repeatedmeas","repeated meas",x))
  ngram<-lapply(ngram,function(x) gsub("rmanova","repeated measures anova",x))
  ngram<-lapply(ngram,function(x) gsub(" way "," factorial ",x))
  ngram<-lapply(ngram,function(x) gsub("metaanalysis|meta-analysis","meta analysis",x))
  ngram<-lapply(ngram,function(x) gsub("multi level|multi-level","multilevel",x))
  ngram<-lapply(ngram,function(x) gsub("pathmodel","path model",x))
  ngram<-lapply(ngram,function(x) gsub("multi varia","multivaria",x))
  ngram<-lapply(ngram,function(x) gsub("multivariable","multivariate",x))
  ngram<-lapply(ngram,function(x) gsub("univariable","univariate",x))
  ngram<-lapply(ngram,function(x) gsub("subjects","subject",x))
  ngram<-lapply(ngram,function(x) gsub("fidence intervals","fidence interval",x))
  ngram<-lapply(ngram,function(x) gsub("modeling","model",x))
  ngram<-lapply(ngram,function(x) gsub("lcmodel","lc model",x))
  ngram<-lapply(ngram,function(x) gsub("models|modeling","model",x))
  ngram<-lapply(ngram,function(x) gsub("tests","test",x))
  ngram<-lapply(ngram,function(x) gsub("fishers","fisher",x))
  ngram<-lapply(ngram,function(x) gsub("effects","effect ",x))
  ngram<-lapply(ngram,function(x) gsub("x2 test| x 2 test|^x 2 test","chi 2 test",x))
  ngram<-lapply(ngram,function(x) gsub("x\\^2|chi \\^2|chi \\^ 2|chi 2","chi square",x))
  ngram<-lapply(ngram,function(x) gsub("chi squared","chi square",x))

  ngram<-lapply(ngram,function(x) gsub("generalized","generalised",x))

  # deleting some uninformative results
  ngram<-lapply(ngram,function(x) grep("^model$|^modeled$|^modelled$|^modelling$",unlist(x),invert=TRUE,value=TRUE))
  ngram<-lapply(ngram,function(x) grep("^test$|^tested$|^testing$",unlist(x),invert=TRUE,value=TRUE))
  ngram<-lapply(ngram,function(x) grep("^analysis$|^analyses$|^data analysis$",unlist(x),invert=TRUE,value=TRUE))
  ngram<-lapply(ngram,function(x) grep("^algorithm$|^algorithms$|^matrix$",unlist(x),invert=TRUE,value=TRUE))
  ngram<-lapply(ngram,function(x) grep("^estimates$|^coefficient$|^intervals$",unlist(x),invert=TRUE,value=TRUE))
  ngram<-lapply(ngram,function(x) gsub("^of ","",x))
  ngram<-lapply(ngram,function(x) gsub(".*article title source ","",x))
  ngram<-lapply(ngram,function(x) gsub("^statistical analysis|statistical method|statistical model","",x))
  ngram<-lapply(ngram,function(x) gsub("regression analys[ei]s|regression model|regression modelling|regression method","regression",x))
  ngram<-lapply(ngram,function(x) gsub("(anc*?ova) analys[ei]s|(anc*?ova) model","\\1\\2",x))
  
  # remove some bad captured and empty cells
  ngram<-lapply(ngram,function(x) grep("[a-z_]test$|[a-z_]test[a-z1-9_]|^test[a-z_]|\\|^[0-9] test$|^[0-9][0-9] test$",unlist(x),invert=TRUE,value=TRUE))
  ngram<-lapply(ngram,function(x) grep("^na na|vilanova|villanova|velanova|^$",x,invert=TRUE,value=TRUE))
  ngram<-lapply(ngram,function(x) grep("secsec|[a-z][a-z][a-z]anova$|tanova$",x,invert=TRUE,value=TRUE))
  # remove space in front of line and double space to single space
  ngram<-lapply(ngram,function(x) gsub("  "," ",gsub("  "," ",x)))
  ngram<-lapply(ngram,function(x) gsub("^ |^  ","",x))
  
  ## remove first of doubled numbers and convert textual number to digit
  nums<-c("one","two","three","four","five","six","seven","eight","nine")
  g<-expand.grid(nums,nums)
  # all doubled text nums 1:9
  doublenum<-paste(g[,1]," ",g[,2]," ",sep="")
  # either in line or start of line
  search<-c(paste(".* ",doublenum,sep=""),paste("^",doublenum,sep=""))
  replace<-paste(" ",rep(as.character(g[,2]),2)," ",sep="")
  # search and replace number in front of number
  for(i in 1:length(search)){
   ngram<-lapply(ngram,function(x) gsub(search[i],replace[i],unlist(x)))
  }
  # convert "one - nine" in text to "1-9"
  search<-c(paste(" ",nums," ",sep=""),paste("^",nums," measure of",sep=""))
  replace<-c(paste0(" ",1:9," "),paste0(1:9," "))
  for(i in 1:length(search)){
   ngram<-lapply(ngram,function(x) gsub(search[i],replace[i],unlist(x)))
  }
  ngram<-unlist(ngram)
  # remove brackets and plus
  ngram<-gsub("[\\+\\[\\(\\]\\)\\{\\}\\=]", "", ngram, perl = TRUE)
  # space clean up
  ngram<-gsub("^ *|(?<= ) | *$", "", ngram, perl = TRUE)
## remove one word captures with keeping those methods with matching of patterns
  tab<-(sort(table(unlist(ngram)),decreasing=T))
  n<-names(tab)
  # select and remove those single words that have no matching with patterns
  p<-grep(paste(patterns,sep="|"),grep(" ",n,invert=TRUE,value=TRUE),value=TRUE,invert=TRUE);length(p)
  ngram<- ngram[!is.element(ngram,p)]
  # remove analyse in fron of line
  ngram<-gsub("^analyse ","",ngram)
  # remove further single detection with certain uninfomative patterns
  p<-grep("model|test|analysis|algorithm|index|statistic|method|estimation|interval|intervals|review|reviews|curve|curves",grep(" ",n,invert=TRUE,value=TRUE),value=TRUE);p
  # only pattern
  p1<-gsub("\\|$","",paste0("^",p,"$|",collapse=""));p1
  # number and pattern only
  p2<-gsub("\\|$","",paste0("^[0-9] ",p,"$|",collapse=""));p2
  # remove [ ] from p1 and p2
#  p1<-gsub("\\[|\\]","",p1)
#  p2<-gsub("\\[|\\]","",p2)
  ngram<-grep(p1,ngram,invert=TRUE,value=TRUE)
  ngram<-grep(p2,ngram,invert=TRUE,value=TRUE)
  # remove further lines containing patterns
  ngram<-grep("spss|^sas |^r ",ngram,invert=TRUE,value=TRUE) # software
  ngram<-grep("^two test$|^two model$|^first model$",ngram,invert=TRUE,value=TRUE)
  ngram<-grep("^every test$|^total test$|^six model$|^four test$|^eight test$|^[1-9] test$",ngram,invert=TRUE,value=TRUE)
  ngram<-grep("^most test$|^one test$|^above model$|^three method$|^latter model$",ngram,invert=TRUE,value=TRUE)
  ngram<-grep("^statistical test$|^statistical model$|^statistical analysis$|^statistical method|^statistical model$",ngram,invert=TRUE,value=TRUE)
  # remove method specific pattern
  ngram<-grep("^different method$",ngram,invert=TRUE,value=TRUE)
  ngram<-grep(  paste0("^",c("second","full","best","one","overall")," model$",collapse="|"),ngram,invert=TRUE,value=TRUE)
  ngram<-gsub(  paste0("^",c("highest","lowest","best","negative","positive","good","low","high")," correlation$",collapse="|"),"correlation",ngram)
  ngram<-gsub(  paste0("^",c("single","within","above","if","third","gender")," anova$",collapse="|"),"anova",ngram)
  nums<-c("one","two","three","four","five","six","seven","eight","nine")
  ngram<-gsub(  paste0("^",nums," anova$",collapse="|"),"anova",ngram)
  ngram<-gsub(  paste0("^",nums," manova$",collapse="|"),"manova",ngram)
  ngram<-gsub(  paste0("^",nums," correlation analysis$",collapse="|"),"correlation",ngram)
  ngram<-gsub(  paste0("^",nums," ancova$",collapse="|"),"ancova",ngram)
  ngram<-gsub(  paste0("^",nums," coefficient$",collapse="|"),"coefficient",ngram)
  ngram<-gsub(  paste0("^",nums," algorithm$",collapse="|"),"algorithm",ngram)
  ngram<-gsub(  paste0("^",nums," algorithms$",collapse="|"),"algorithm",ngram)
  ngram<-gsub(  paste0("^",nums," curve$",collapse="|"),"curve",ngram)
  ngram<-gsub(  paste0("^",nums," test$",collapse="|"),"curve",ngram)
  ngram<-gsub(  paste0("^",nums," regression$",collapse="|"),"regression",ngram)
  ngram<-gsub(  paste0("^",nums," linear regression$",collapse="|"),"linear regression",ngram)
  ngram<-gsub(  paste0("^",1:9," anova$",collapse="|"),"anova",ngram)
  ngram<-gsub(  paste0("^",1:9," model$",collapse="|"),"model",ngram)
  ngram<-gsub(  paste0("^",nums," model$",collapse="|"),"model",ngram)
  ngram<-gsub(  paste0("^",nums," distinct ",collapse="|"),"",ngram)
  
    # correct within[a-z], factorial[a-z]
  ngram<-gsub("within([a-z])","within \\1",ngram)
  ngram<-gsub("factorial([a-z])","factorial \\1",ngram)
  
  ngram<-gsub(".*\\[","",ngram)
  # remove space before percent: \U0025
  ngram<-gsub(" \U0025","\U0025",ngram)
  ## reduce to unique methods per article and select only lines still containing patterns
  ngram<-unique(ngram)
  ngram<-unlist(lapply(ngram,get.sentence.with.pattern,patterns))
# reconvert "bayesfactor" to "bayes factor"
  ngram<- gsub("informationcriterion","information criterion",gsub("bayesfactor","bayes factor",ngram))
  ngram<- gsub("areaunderthe curve","area under the curve",ngram)
  ngram<- gsub("^gof |( )gof ","\\1goodness of fit ",ngram)

  # remove elements with test[a-z]
  pat<-"test[a-z]"
  p<-grep(pat,unlist(ngram[grep(pat,ngram)]),value=TRUE)
  ngram<- ngram[!is.element(ngram,p)]
  if(sum(is.null(ngram))==1) ngram<-character()
  ngram<-stats::na.omit(ngram)
  
  # remove further single detection with certain uninfomative patterns
  ngram<-grep("^model$|^test$|^analysis$|^algorithm$|^index$|^statistic$|^method$|^estimation$|^interval$|^intervals$|^review$|^reviews$|^curve$|^curves$",ngram,invert=TRUE,value=TRUE)
  
  # reduce nested list to simple list
  #ngram<-mapply(c,ngram)
 } else ngram<-character() 

return(ngram)
}


