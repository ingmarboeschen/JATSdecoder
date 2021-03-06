#' study.character
#'
#' extracts study characteristics out of a JATS coded XML file or JATSdecoder result
#' @param x JATS coded XML file or JATSdecoder result
#' @param text.mode text parts to extract statistical results from (text.mode=1: abstract and full text, text.mode=2: method and result section, text.mode=3: result section only)
#' @param captions Logical. If TRUE captions text will be scanned for statistical results
#' @param stats.mode Character. Select subset of standard stats. One of: "all", "checkable", "computable"
#' @param recalculate.p Logical. If TRUE recalculates p values (for 2 sided test) if possible
#' @param alternative Character. Select sidedness of recomputed p-values for t-, r- and Z-values. One of c("auto","undirected","directed","both"). If set to "auto" 'alternative' will be be set to 'both' if get.test.direction() detects one-directional hypotheses/tests in text. If no directional hypotheses/tests are dtected only "undirected" recomputed p-values will be returned
#' @param estimateZ Logical. If TRUE detected beta-/d-value is divided by reported standard error "SE" to estimate Z-value ("Zest") for observed beta/d and recompute p-value. Note: This is only valid, if Gauss-Marcov assumptions are met and a sufficiently large sample size is used. If a Z- or t-value is detected in a report of a beta-/d-coefficient with SE, no estimation will be performed, although set to TRUE.
#' @param T2t Logical. If TRUE capital letter T is treated as t-statistic when extracting statistics with get.stats()
#' @param R2r Logical. If TRUE capital letter R is treated as correlation when extracting statistics with get.stats()
#' @param selectStandardStats Select specific standard statistics only (e.g.: c("t","F","Chi2"))
#' @param p2alpha Logical. If TRUE detects and extracts alpha errors denoted with critical p-value (what may lead to some false positive detections) 
#' @param alpha_output One of "list" (list with elements: alpha_error, corrected_alpha, alpha_from_CI, alpha_max, alpha_min), vector with unique alpha errors but no distinction of types
#' @param update.package.list if TRUE updates available R packages with available.packages() function
#' @param add.software additional software names to detect as vector
#' @param quantileDF quantile of (df1+1)+(df2+1) to extract for estimating sample size
#' @param N.max.only return only maximum of estimated sample sizes
#' @param output output selection of specific results c("all", "doi", "title", "year", "n.studies", "methods", "alpha.error", "power", "multi.comparison.correction", "assumptions", "OutlierRemovalInSD", "InteractionModeratorMediatorEffect", "test.direction", "sig.adjectives", "software", "Rpackage", "stats", "standardStats", "estimated.sample.size")
#' @param rm.na.col Logical. If TRUE removes all columns with only NA in extracted standard statistics
#' @export

study.character<-function(x,
                          stats.mode="all",
                          recalculate.p=TRUE,
                          alternative="auto",
                          estimateZ=FALSE,
                          T2t=FALSE,
                          R2r=FALSE,
                          selectStandardStats=NULL,
                          p2alpha=FALSE, 
                          alpha_output="list",
                          captions=TRUE,
                          text.mode=1,
                          update.package.list=FALSE,
                          add.software=NULL,
                          quantileDF=.75,
                          N.max.only=FALSE,
                          output="all",
                          rm.na.col=TRUE){
   # prepare captions object
   caps<-character(0)
   # check if x is xml file or list else stop
   if(length(grep("^<\\?xml",x))==0) if(length(grep("xml$|XML$",x[1]))==0&!is.list(x)) stop("file is not in XML nor NISO-JATS format nor a JATSdecoder result")
   # if x is xml file readLines else copy to temp
   if(length(grep("\\.nxml$|cermxml$|\\.xml$|XML$",x[1]))==1){
      temp<-readLines(x,warn=F)
   }else temp<-x
   
   # if output only contains standardStats add stats
   nostat<-ifelse(!is.element("stats",output)&is.element("standardStats",output), TRUE, FALSE) # remove stat if not set
   if(is.element("standardStats",output)) output<-unique(c("stats",output))
   
   # check if input is made by cermine
   ifelse(length(grep("cermxml$",x[1]))==1, cermine<-TRUE,  cermine<-FALSE)
   
   ## get method and result text from plain xml text
   if(length(grep("!DOCTYPE",temp[1:5]))>0|sum(is.element(c("sections","text"),names(temp)))==2){
      # for JATS coded xml
      if(length(grep("!DOCTYPE",temp[1:5]))>0){
         t<-get.text(temp,sectionsplit=c("intro|background","data|statistic|method|material|sample|solution|analys|procedure|measures","participants|subjects|animals|patients","experiment|study","result|finding","conclusion","discussion","implication|limitation","conflict of|author contrib|ethic"),letter.convert=TRUE,cermine=cermine,greek2text=TRUE)
         if(is.element("captions",names(t))) caps<-t$captions
      }
      
      # for JATSdecoder() results
      if(sum(is.element(c("sections","text"),names(temp)))==2){
         if(is.element("captions",names(t))) caps<-t$captions
         t<-list("section"=unlist(temp["sections"]),"text"=unlist(temp["text"]))
         t<-lapply(t,letter.convert,cermine=TRUE,greek2text=TRUE)
      }
      
      ## get text from participants sections
      sections<-"participants|subjects|animals|patients"
      # get method section text as one line
      j<-grep(tolower(sections),tolower(unlist(t["section"])))
      text<-paste(unlist(t["text"])[j],collapse=" ")
      # clean up white spaces
      text<-gsub("^ *|(?<= ) | *$", "", text, perl = TRUE)
      # participants sentences
      participants<-text2sentences(text)
      rm(text)
      ## get text from method sections
      sections<-"data|statistic|method|material|sample|solution|analys|participants|procedure|measures|subjects|experiment|study|animals|patients"
      # get method section text as one line
      j<-grep(tolower(sections),tolower(unlist(t["section"])))
      text<-paste(unlist(t["text"])[j],collapse=" ")
      # clean up white spaces
      text<-gsub("^ *|(?<= ) | *$", "", text, perl = TRUE)
      # method sentences
      method<-text2sentences(text)
      rm(text)
      ## get text from result sections
      sections<-"result|finding"
      # get result section text as one line
      j<-grep(tolower(sections),tolower(unlist(t["section"])))
      text<-paste(unlist(t["text"])[j],collapse=" ")
      # clean up white spaces
      text<-gsub("^ *|(?<= ) | *$", "", text, perl = TRUE)
      # result sentences
      result<-text2sentences(text)
      rm(text)
      
      ## merge method and result sentences
      both<-unique(c(method,result))
      
      ## for bad section capture of CERMINE split and get text
      if(cermine==T&length(both)==0&length(unlist(t["section"]))==1){ 
         # split text
         text<-unlist(strsplit2(unlist(t["text"]),"Method|Procedure|Material|Result|Findings|Discuss|Conclusion","before"))
         if(length(text)>1){
            # get method texts as one line
            method<-text2sentences(paste(grep("Method|Material|Procedure",text,value=TRUE),collapse=" "))
            # clean up white spaces
            method<-gsub("^ *|(?<= ) | *$", "", method, perl = TRUE)
            # get result texts as one line
            result<-text2sentences(paste(grep("Result|Findings",text,value=TRUE),collapse=" "))
            # clean up white spaces
            result<-gsub("^ *|(?<= ) | *$", "", result, perl = TRUE)
            # merge
            both<-unique(c(method,result))
         }else{
            result<-""
            method<-""
         }
      }# end bad section exception
      
   }# end get method and result text
   
   
   if(length(both)==0) both<-""
   # get all sentences from full text as vector
   fulltext<-unlist(lapply(t,text2sentences))
   
   ###################
   ### export results
   # from JATSdecoder result
   if(sum(is.element(c("doi","title"),names(temp)))==2){
      doi<-as.character(unlist(temp["doi"]))
      title<-as.character(unlist(temp["title"]))
      year<-as.numeric(unlist(lapply(temp["history"],"[","pubyear")))
      history<-unlist(temp["history"])
      abstract<-text2sentences(as.character(unlist(temp["abstract"])))
      temp<-"!DOCTYPE"
   }else{
      # from JATS coded text
      doi<-get.doi(temp)
      title<-get.title(temp)
      year<-as.numeric(get.history(temp)["pubyear"])
      history<-get.history(temp)
      abstract<-text2sentences(get.abstract(temp,cermine=cermine))
   }
   
   # compute time to publish &  time to accept
#   pubDate<-suppressWarnings(as.Date(history["pubDate"]))
#   sub<-suppressWarnings(min(as.Date(history[c("submitted","nihms_submitted")]),na.rm=T))
#   accept<-suppressWarnings(as.Date(history[c("accepted")]))
#   time2publish<-pubDate-sub
#   time2accept<-pubDate-accept
#   if(time2publish==-Inf) time2publish<-NA
#   if(time2accept==-Inf) time2accept<-NA
   
   
   # get n studies from sections
   if(length(grep("!DOCTYPE",temp[1:10]))>0){
      # get n studies from sections
      n.studies<-get.n.studies(unlist(t["section"]),tolower=TRUE)
      # go on searching only if n.studies==1
      if(n.studies>0){
         # get n studies from abstract
         n.studies.abstract<-get.n.studies(text2sentences(gsub("second [Ss]tudy","Study 2",abstract)),tolower=FALSE)
      }else n.studies.abstract<-1
      # go on searching only if also n.studies.abstract==1
      if(n.studies==1&n.studies.abstract==1){
         # get n studies from result text and method if nothing found in abstract
         reduced<-grep("[1-2][0-9][0-9][0-9]",both,invert=TRUE,value=TRUE)
         n.studies.text<-get.n.studies(gsub("second [Ss]tudy","Study 2",reduced),tolower=FALSE)
      }else n.studies.text<-1
      # take maximum 
      n.studies<-max(n.studies,n.studies.abstract,na.rm=T)
      #n.studies<-max(n.studies,n.studies.abstract,n.studies.text,na.rm=TRUE)
      # End n.studies==1
   }else{n.studies<-NA} # End no !DOCTYPE
   # get characteristics
   if(sum(is.element(c("all","methods"),output))>0){ methods<-get.method(both,cermine=cermine) }else{methods<-NA}
   if(sum(is.element(c("all","methods"),output))>0){ categorized.methods<-categorize.methods(methods) }else{categorized.methods<-NA}
   if(sum(is.element(c("all","alpha.error"),output))>0){ alpha.error<-get.alpha.error(c(both,caps),p2alpha=p2alpha,output=alpha_output) }else{alpha.error<-NA}
   if(sum(is.element(c("all","multi.comp"),output))>0){ multi.comp<-get.multi.comparison(c(both,caps)) }else{multi.comp<-NA}
   if(sum(is.element(c("all","power"),output))>0){ power<-unname(unlist(get.power(c(fulltext,caps)))) }else{power<-NA}
   if(sum(is.element(c("all","assumptions"),output))>0){ assumptions<-get.assumptions(both) }else{assumptions<-NA}
   if(sum(is.element(c("all","OutlierRemovalInSD"),output))>0){ outlier<- get.outlier.def(both) }else{outlier<-NA}
   if(sum(is.element(c("all","InteractionModeratorMediatorEffect"),output))>0){ InteractionModeratorMediatorEffect<-has.interaction(both) }else{InteractionModeratorMediatorEffect<-NA}
   if(sum(is.element(c("all","test.direction","standardStats"),output))>0){ test.direction<-get.test.direction(c(both,caps)) }else{test.direction<-NA}
   if(sum(is.element(c("all","software"),output))>0){ software<-get.software(both,add.software=add.software) }else{software<-NA}
   if(sum(is.element(c("all","Rpackage"),output))>0){ Rpackage<-get.R.package(both,update.package.list=update.package.list) }else{Rpackage<-NA}
   
   if(sum(is.element(c("all","stats","statsOnStats"),output))>0){
      if(text.mode==1) stats<-c("\u2022 Results in abstract and full text:",allStats(c(abstract,fulltext)))
      if(text.mode==2) stats<-c("\u2022 Results in methods and results sections:",allStats(both))
      if(text.mode==3) stats<-c("\u2022 Results in results section/s:",allStats(result))
      # add results from captions
      if(captions==TRUE) stats<-c(stats,"\u2022 Results in captions:",allStats(caps))   
      
      # get standard stats if output is desired
      if(sum(is.element(c("all","statsOnStats","standardStats"),output))>0){
         # set direction to "both" for alternative if has one sided hypotheses/tests
         if(length(grep("^one ",test.direction))>0){
            direction<-"both"}else{direction<-"undirected"}
         standardStats<-standardStats(stats,stats.mode=stats.mode,recalculate.p=recalculate.p,alternative=direction,T2t=T2t,R2r=R2r,estimateZ=estimateZ,rm.na.col=rm.na.col,select=selectStandardStats)
      }else standardStats<-NA
   }else{
      stats<-NA;standardStats<-NA}
   
# compute statsOnStats if output is desired
   if(sum(is.element(c("all","statsOnStats"),output))>0){
   if(is.element("p",colnames(standardStats))){
      nPvalues<-sum(!is.na(standardStats[,"p"]))
   }else nPvalues<-NA
   if(is.element("recalculatedP",colnames(standardStats))){
      nPcomp<-sum(!is.na(standardStats[,"recalculatedP"]))
   }else nPcomp<-NA
   if(sum(is.element(c("p","recalculatedP"),colnames(standardStats)))==2){
      nPcheck<-sum(!is.na(standardStats[,"p"])&!is.na(standardStats[,"recalculatedP"]))
   }else nPcheck<-NA
   }else{
   nPvalues<-NA;nPcheck<-NA;nPcomp<-NA
   }
   statsOnStats<-list(nPvalues=nPvalues,nPcomputable=nPcomp,nPcheckable=nPcheck)
   # adjectives in front of significant/insignificant
   if(sum(is.element(c("all","sig.adjectives","insig.adjectives"),output))>0){
          sig.adjectives<-get.sig.adjectives(both)$sig_adjective
          insig.adjectives<-get.sig.adjectives(both)$insig_adjective
          # else
          }else{
             sig.adjectives<-NA
             insig.adjectives<-NA
          }
          
   # SAMPLE SIZE
   if(sum(is.element(c("all","estimated.sample.size"),output))>0){
      out<-est.ss(abstract=abstract,text=both,quantileDF=quantileDF,max.only=N.max.only)
      sample.size<-list(SSabstractNsum=unname(out[1]),SSstats=unname(out[2]),SSstandardStats=unname(out[3]),estimatedSampleSize=unname(out[4]))
   }else sample.size<-NA
   
   ## output
   res<-list(
      doi=doi,
      title=title,
      year=year,
      Nstudies=n.studies,
      methods=methods,
      categorized_methods=categorized.methods,
      alpha_error=alpha.error,
      power=power,
      multi_comparison_correction=multi.comp,
      assumptions=assumptions,
      OutlierRemovalInSD=outlier,
      InteractionModeratorMediatorEffect=InteractionModeratorMediatorEffect,
      test_direction=test.direction,
      sig_adjectives=sig.adjectives,
      insig_adjectives=insig.adjectives,
      software=software,
      Rpackage=Rpackage,
      stats=stats,
      standardStats=standardStats,
      statsOnStats=statsOnStats,
      estimated_sample_size=sample.size
   )
   
   if(nostat==TRUE)  output<-output[output!="stats"]
   # reduce output to list of desired outputs
   if(!is.element("all",output)) res<-res[is.element(names(res),output)]
   
   
   return(res)
}


categorize.methods<-function(x){
   names<-c("descriptive statistics|descriptive analysis|descriptives",
            "permutation test|permutation.*? test|permutation.*? model",
            "neural network","machine learning","random forest","random trees|random.* trees*",
            "single case",
            "sequential analysis|sequential.* test|sequential.* model",
            "distributional analysis|distribution analysis|distribution fitting",
            "inter rater reliability|inter.*rater.*reliability|kappa|intraclass correlation|intra class correlation",
            "reliable change index|reliable change ind",
            "gini index",
            "pearson correlation|pearson product|moment correla|zero order correlation|^correlation$",
            "spearman correlation|spearman brown|spearman coef|spearman rank|spearman rho",
            "cramer v",
            "somer d",
            "contingency table|contingency coef",
            "^chi square|[^d] chi square", 
            "wald test|wald chi square",
            "power analysis|power estima|power analys",
            "bootstrap",
            "confidence interval",
            "highest density interval",
            "fisher exact","fisher z","mcnemar","cochran q","z statistic",
            "mann whitney u test|mann *?whitney|u test",
            "wilcoxon sign rank test|signe*?d*? rank|wilcoxon",
            "kruskal wallis test",
            "friedman test",
            "one sample t test|single sample t test",
            "^t test$|[^n][^e] t test|independent t test|two sample t test", 
            "paired t test|paired samples*? t test",
            " anova|^anova",
            "repeated measures anova|repeated measure.* anova",
            " ancova|^ancova",
            "manova|mancova",
            "path analysis|path model|path coefficient|path estimate",
            "structural equation",
            "multilevel structural equation",
            "growth curve|growth model",
            "multilevel growth|multigroup.*?growth",
            "confirmatory factor analysis|confirmatory factor",
            "exploratory factor analysis|exploratory factor",
            "cronbach alpha|reliability coeff|cronbach coeff",
            "mc donald omega coefficient|omega estimate|mcdonald|mc donald",
            "test retest reliability|test retest corr",
            "convergent validity|discriminant validity|[^a-z]ave |^ave |msv",
            "sensitivity","specificity","specifity",
            "spatial model|spatial.*?analys|spatial.*?model",
            "item analysis|items analysis",
            "item response analysis|dif analysis",
            "cluster analysis|cluster.* analysis",
            "roc curve|receiver operat",
            "markov|marcov",
            "bayes|baysian",
            "simulation",
            "multiple imputation",
            "regression",
            "OLS regression|ols regression|ordinary.least.square",
            "maximum likelihood|maximum.*?likelihoo*?d",
            "hierarchical regression|hie*?r[ia].*regression|hie*?r[ia].*model",
            "^non linear| non.*linear|^non.*?linear",
            "non parametric statistic|non.*?parametric",
            "longitudinal|^paneli*?[zs]*?e*?d*? | paneli*?[zs]*?e*?d*? ",
            "multilevel regression|multi.*?level.*?regres|multi.*?level.*?model|mixed.*?regres|nested.*?regress|nested.*?model",
            "multivariate regresion|multivariate.*?regres|multiple*?regres",
            "poisson regression|poisson.*?regres",
            "logistic regression|log[ia][rst].*regression|log[ia][rst].*model|binary.*regression",
            "probit regression|probit.*model|probit.*regression",
            "multilevel logistic regression|multi.*?level logistic|logistic multilevel",
            "multinomial regresion|multinom.*?regres",
            "systematic review",
            "literature review",
            "meta analysis|meta analyses",
            "allgorithm",
            "propensity score",
            "goodness of fit|^gof | gof ",
            "network analysis",
            "ordinal regression|ordinal model|rank regression",
            "drift diffusion model|drift diffusion|ddm",
            "generalized estimation equation|^gee | gee "
   )
   methodsCat<-unlist(lapply(x,function(x) gsub("\\|.*|^ |\\^|\\$","",which.term(x,names,hits=T))))
   return(methodsCat)
   }

