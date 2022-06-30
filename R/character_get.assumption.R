#' get.assumptions
#'
#' Extracts the mentioned statistical assumptions from a text string by a dictionary search of 22 common statistical assumptions.
#' @param x text string to process.
#' @param hits_only Logical. If TRUE returns the detected assumtions only, else a hit matrix with all potential assumptions is returned.
#' @export
#' @examples
#' x<-"Sphericity assumption and gaus-marcov was violated."
#' get.assumptions(x)

get.assumptions<-function(x,hits_only=TRUE){

# get lines with pattern
x<-x[grep("violation|violate|assumption|assuming|assume[ d]|correct|adjust|checked|data|analys|theorem",tolower(x))]
# set x to "" if length==0
if(length(x)==0) x<-""
# collapse x
x<-paste(x,collapse=" ")
hits<-lapply(x,which.term,c(
"equal variances|equal variance|equality of variance",
"homogeneity of variances|homogeneity .* variance|homogene*o*us variance|[^a-z]hov[^a-z]",# homogene*i*ty| 
"sphericity",
"normal distribution|normal distribut|normality assumption|normally distribut|assumption of normal|gaussianity",
"multivariate normal",
"[a-z]linearity|[a-z]linear|^linearity|^linear",
"homoscedasticity|homo[- ]*scedastic",
"autocorrelation|auto[- ]correlation|autocor|[^a-z]acf[^a-z]",
"multicollinearity|multi[- ]*coll*inear",
"missing completely at random|[^a-z]mcar[^a-z]|[^a-z]marc[^a-z]",
"missing at random|[^a-z]mar assumption|non *informative missing",
"gauss-marcov|gauss[- ]mar[ck]o[wv]|gau[s\U00DF][- ]mar[ck]o[wv]",
"^mar[ck]ov assumption|[^s\U00DF] mar[ck]ov assumption",
" independency| independence|^independenc[ey]",
"orthogonality",
"monotonicity",
"proportional hazards|proportional[- ]hazard|proportionality assumption|[^a-z]ph assump",
"proportional odds|proportional.* odds",
"weibull assumption|[^a-z]weibul",
"uni dimensionality|unidimensionality|uni[ -]dimensionality",
"local independency|local independence"
),hits_only=hits_only)

if(hits_only==TRUE){
  out<-unique(unlist(hits))
  out<-gsub("\\|.*","",out)
  out<-gsub("\\[ck\\]","k",out)
  out<-gsub("\\[a-z\\]|\\*|\\^|^ ","",out)
  return(out)
}
if(hits_only==FALSE) return(hits)
}
