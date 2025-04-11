#' pCheck
#' 
#' Wrapper function for a standardStats data frame to check extracted and recalculated p-value for consistency
#' @param stats Data frame. A data frame object of standard stats that was created by get.stats() or standardStats() 
#' @param alpha Numeric. Set the alpha level of tests.
#' @param criticalDif Numeric. Defines the absolute threshold of absolute differences in extracted and recalculated p-value to be labeled as inconsistency.
#' @param add Logical. If TRUE the result of Pcheck are added to the input data frame.
#' @param warnings Logical. If FALSE warning messages are omitted. 
#' @return A data frame with error report on each entry in the result of a standard stats data frame.
#' @export
#' @examples
#' ## Extract and check results from plain text input with get.stats(x,checkP=TRUE)
#' get.stats("some text with consistent or inconsistent statistical results: 
#' t(12)=3.4, p<.05 or t(12)=3.4, p>=.05",checkP=TRUE)
#' ## Check standardStats extracted with get.stats(x)$standardStats
#' pCheck(get.stats("some text with consistent or inconsistent statistical results: 
#' t(12)=3.4, p<.05 or t(12)=3.4, p>=.05")$standardStats)

pCheck<-function(stats, 
                 alpha=.05, 
                 criticalDif=.02, 
                 add=TRUE,
                 warnings=TRUE
){
  #  Ndigits of alpha level
  if(!alpha>0&!alpha<1) stop("Set alpha level to a value greater 0 and less 1.") 
  Ndigits <- nchar(gsub("^0\\.","",as.character(alpha)))
  if(length(stats)==0) return(character(0))
  
  # only go on if is data.frame and input has reported and recomputable p-value
  if(is.data.frame(stats)){
    if(is.element("recalculatedP",names(stats))&is.element("p",names(stats))){
      
      # absolute difference of p and reculated p value with all and X digits 
      difference<-stats$recalculatedP-stats$p
      absDif<-abs((stats$p)-(stats$recalculatedP))
      
      ## find and classify ciritical differences
      # empty object
      error<-rep(0,length(absDif))
      
      # definition 1: if |difference| > criticalDif
        error[absDif>criticalDif] <- 1
      
      # definition 2: reported p value is < alpha, recalculated >= alpha 
        error[ is.element(stats$p_op,c("<"))       & stats$p<=alpha & stats$recalculatedP>alpha |
                 is.element(stats$p_op,c("<=","=")) & stats$p<=alpha & round(stats$recalculatedP,Ndigits)>alpha] <- 1
      
      # defintion 3: reported p value is >= alpha, recalculated < alpha 
        error[ is.element(stats$p_op,c(">"))       & stats$p>=alpha & stats$recalculatedP<alpha |
                 is.element(stats$p_op,c(">=","=")) & stats$p>=alpha & round(stats$recalculatedP,Ndigits)<alpha] <- 1
      
      # set detected critical but actually non critical differences to 0
      error[ is.element(stats$p_op,c("<","<="))   & stats$p>stats$recalculatedP ] <- 0
      error[ is.element(stats$p_op,c(">",">="))   & stats$p<stats$recalculatedP ] <- 0
      #error[absDif==0] <- 0
      
      # set error to NA if has not both p and recalculated p
      error[is.na(absDif)]<-NA
      # set standard type of decision error to "numeric"
      errorType<-ifelse(error==1,"numeric inconsistency in p-value",NA)
      
      ## mark false positives
      # if reported p <= alpha and recalculated p > alpha
      errorType<-ifelse(error==1&
                          stats$p < stats$recalculatedP&
                          stats$recalculatedP > alpha&
                          stats$p <= alpha,"false positive p-value",errorType)
      
      ## mark false negatives
      # if reported p > alpha and recalculated p < alpha
      errorType<-ifelse(error==1&
                          stats$p>stats$recalculatedP&
                          stats$recalculatedP<alpha&
                          stats$p>alpha,"false negative p-value",errorType)
      errorType<-ifelse(error==1&
                          stats$p>stats$recalculatedP&
                          stats$recalculatedP<alpha&
                          stats$p>=alpha,"false negative p-value",errorType)
      
      ####### add '?, manual check on operator "<=>" needed!' to errorType for CERMINE compiled files
      if(is.element("<=>",stats$p_op)){
        errorType[is.element(stats$p_op,"<=>")&!is.na(stats$recalculatedP)]<-paste0(errorType[is.element(stats$p_op,"<=>")&!is.na(stats$recalculatedP)],'?, check uncompilable operators "<=>" manually!')
        errorType<-gsub("NA[?], ","",errorType)
      }
      
      # error potentially due to one-sided testing
      if(!is.null(stats$p_H1_less)){ 
        # for results with p=num
        errorType<-ifelse(!is.na(stats$p_H1_greater) & #error==1 & 
                            round(stats$p,Ndigits)==round(stats$p_H1_greater,Ndigits),
                          gsub("^, ","",paste0(errorType,", one sided?")),errorType)
        errorType<-ifelse(!is.na(stats$p_H1_less) & #error==1 & 
                            round(stats$p,Ndigits)==round(stats$p_H1_less,Ndigits),
                          gsub("^, ","",paste0(errorType,", one sided?")),errorType)
        
        # for results with p<num or p<=num
        errorType<-ifelse(!is.na(stats$p_H1_greater) & #error==1 & 
                            is.element(stats$p_op,c("<","<=")) & 
                            round(stats$p_H1_greater,Ndigits)<=stats$p,
                          gsub("^, ","",paste0(errorType,", one sided?")),errorType)
        errorType<-ifelse(!is.na(stats$p_H1_less) & #error==1 & 
                            is.element(stats$p_op,c("<","<=")) & 
                            round(stats$p_H1_less,Ndigits)<=stats$p,
                          gsub("^, ","",paste0(errorType,", one sided?")),errorType)
        
        # for results with p<num or p<=num
        errorType<-ifelse(!is.na(stats$p_H1_greater) & #error==1 & 
                            is.element(stats$p_op,c(">",">=")) & 
                            round(stats$p_H1_greater,Ndigits)>=stats$p,
                          gsub("^, ","",paste0(errorType,", one sided?")),errorType)
        errorType<-ifelse(!is.na(stats$p_H1_less) & #error==1 & 
                            is.element(stats$p_op,c(">",">=")) & 
                            round(stats$p_H1_less,Ndigits)>=stats$p,
                          gsub("^, ","",paste0(errorType,", one sided?")),errorType)
        
        
        # clean up
        errorType<-gsub("NA, ","",gsub(", one sided[?], one sided[?]",", one sided?",errorType))
      }
      
      # warnings
      if(is.null(stats$p_H1_less)&warnings==TRUE) warning(paste0('A check for consistency of the reported and recalculated p-values was carried out with alpha=',alpha,' and critical absolute difference in p-values=',criticalDif,'.'),call.=FALSE)
      if(!is.null(stats$p_H1_less)&warnings==TRUE) warning(paste0('A check for consistency of the reported and recalculated p-values for one sided tests was carried out with alpha=',alpha,' and critical absolute difference in p-values=',criticalDif,'. 
  However, every detected inconsistency marked with "one sided?" has to be checked maually for correct application of test sidedness and interpretation.'),call.=FALSE)
      
      # prepare output
      out1<-data.frame(
        #stats,
        deltaP2tailed=difference,
        #absDif,
        error=error,
        errorType=errorType)
      
      # end of has p and recalculated p values
    } else out1<-data.frame(deltaP2tailed=rep(NA,nrow(stats)),error=NA,errorType=NA)
    
    
    ######################################################
    ###### check coded p and recalculated p
    # if is data.frame and input has reported coded p-value
    if(is.element("recalculatedP",names(stats))&is.element("codedP",names(stats))){
        
        # absolute difference of coded p and recalculated p value with all and X digits 
        difference<-stats$recalculatedP-stats$codedP
        absDif<-abs((stats$codedP)-(stats$recalculatedP))
        
        ## find and classify ciritical differences
        # empty object
        error<-rep(0,length(absDif))
        
        # definition 1: if |difference| > criticalDif
        error[absDif>criticalDif] <- 1
        
        # definition 2: coded p value is < alpha, recalculated >= alpha 
        error[ is.element(stats$codedP_op,c("<"))       & stats$codedP<=alpha & stats$recalculatedP>alpha |
                 is.element(stats$codedP_op,c("<=","=")) & stats$codedP<=alpha & round(stats$recalculatedP,Ndigits)>alpha] <- 1
        
        # defintion 3: coded p value is >= alpha, recalculated < alpha 
        error[ is.element(stats$codedP_op,c(">"))       & stats$codedP>=alpha & stats$recalculatedP<alpha |
                 is.element(stats$codedP_op,c(">=","=")) & stats$codedP>=alpha & round(stats$recalculatedP,Ndigits)<alpha] <- 1
        
        # set detected critical but actually non critical differences to 0
        error[ is.element(stats$codedP_op,c("<","<="))   & stats$codedP>stats$recalculatedP ] <- 0
        error[ is.element(stats$codedP_op,c(">",">="))   & stats$codedP<stats$recalculatedP ] <- 0
        #error[absDif==0] <- 0
        
        # set error to NA if has not both coded p and recalculated p
        error[is.na(absDif)]<-NA
        # set standard type of decision inconsistency to "numeric"
        errorType<-ifelse(error==1,"numeric inconsistency in p coding",NA)
        
        ## mark false positives
        # if coded p <= alpha and recalculated p > alpha
        errorType<-ifelse(error==1&
                            stats$codedP < stats$recalculatedP&
                            stats$recalculatedP > alpha&
                            stats$codedP <= alpha,"false positive p coding",errorType)
        
        ## mark false negatives
        # if coded p > alpha and recalculated p < alpha
        errorType<-ifelse(error==1&
                            stats$codedP>stats$recalculatedP&
                            stats$recalculatedP<alpha&
                            stats$codedP>alpha,"false negative p coding",errorType)
        errorType<-ifelse(error==1&
                            stats$codedP>stats$recalculatedP&
                            stats$recalculatedP<alpha&
                            stats$codedP>=alpha,"false negative p coding",errorType)
        
        ####### add '?, manual check on operator "<=>" needed!' to errorType for CERMINE compiled files
        if(is.element("<=>",stats$codedP_op)){
          errorType[is.element(stats$codedP_op,"<=>")&!is.na(stats$recalculatedP)]<-paste0(errorType[is.element(stats$codedP_op,"<=>")&!is.na(stats$recalculatedP)],'?, check uncompilable operators "<=>" manually!')
          errorType<-gsub("NA[?], ","",errorType)
        }
        
        # error potentially due to one-sided testing
        if(!is.null(stats$codedP_H1_less)){ 
          # for results with p=num
          errorType<-ifelse(!is.na(stats$codedP_H1_greater) & #error==1 & 
                              round(stats$codedP,Ndigits)==round(stats$codedP_H1_greater,Ndigits),
                            gsub("^, ","",paste0(errorType,", one sided?")),errorType)
          errorType<-ifelse(!is.na(stats$codedP_H1_less) & #error==1 & 
                              round(stats$codedP,Ndigits)==round(stats$codedP_H1_less,Ndigits),
                            gsub("^, ","",paste0(errorType,", one sided?")),errorType)
          
          # for results with p<num or p<=num
          errorType<-ifelse(!is.na(stats$codedP_H1_greater) & #error==1 & 
                              is.element(stats$codedP_op,c("<","<=")) & 
                              round(stats$codedP_H1_greater,Ndigits)<=stats$codedP,
                            gsub("^, ","",paste0(errorType,", one sided?")),errorType)
          errorType<-ifelse(!is.na(stats$codedP_H1_less) & #error==1 & 
                              is.element(stats$codedP_op,c("<","<=")) & 
                              round(stats$codedP_H1_less,Ndigits)<=stats$codedP,
                            gsub("^, ","",paste0(errorType,", one sided?")),errorType)
          
          # for results with p<num or p<=num
          errorType<-ifelse(!is.na(stats$codedP_H1_greater) & #error==1 & 
                              is.element(stats$codedP_op,c(">",">=")) & 
                              round(stats$codedP_H1_greater,Ndigits)>=stats$codedP,
                            gsub("^, ","",paste0(errorType,", one sided?")),errorType)
          errorType<-ifelse(!is.na(stats$codedP_H1_less) & #error==1 & 
                              is.element(stats$codedP_op,c(">",">=")) & 
                              round(stats$codedP_H1_less,Ndigits)>=stats$codedP,
                            gsub("^, ","",paste0(errorType,", one sided?")),errorType)
          # clean up
          errorType<-gsub("NA, ","",gsub(", one sided[?], one sided[?]",", one sided?",errorType))
        }
        
        # warnings
        if(is.null(stats$codedP_H1_less)&warnings==TRUE) warning(paste0('A check for consistency of the coded and recalculated p-values was carried out with alpha=',alpha,' and critical absolute difference in p-values=',criticalDif,'.'),call.=FALSE)
        if(!is.null(stats$codedP_H1_less)&warnings==TRUE) warning(paste0('A check for consistency of the coded and recalculated p-values for one sided tests was carried out with alpha=',alpha,' and critical absolute difference in p-values=',criticalDif,'. 
  However, every detected inconsistency marked with "one sided?" has to be checked maually for correct application of test sidedness and interpretation.'),call.=FALSE)
        
        # prepare output
        out2<-data.frame(
          deltaP2tailed=difference,
          #absDif,
          error=error,
          errorType=errorType)
        
      }else out2<-data.frame(deltaP2tailed=rep(NA,nrow(stats)),error=NA,errorType=NA)
      # end if has coded p and recalculated p values
    

    # combine results
    out<-out1
    out$error[which(out1$error==1|out2$error==1)]<-1
    out$errorType<-gsub("NA, |, NA","",paste(out1$errorType,out2$errorType,sep=", "))
    out$errorType[which(out$errorType=="NA")]<-NA

    # add to input table
    if(add==TRUE) out<-data.frame(stats,out)
    
    # end if !is.data.frame
  } else out<-stats
  
  return(out)
}

