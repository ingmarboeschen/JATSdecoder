#' get.assumptions
#'
#' Extract mentioned assumptions in text out of list with 22 statistical assumptions
#' @param x text to process
#' @param hits_only Logical. If TRUE returns the detected assumtions only, else a hit matrix with all potential assumptions
#' @export
#' @examples
#' x<-"Sphericity assumption and gaus-marcov was violated."
#' get.assumptions(x)

get.assumptions<-function(x,hits_only=TRUE){
# get lines with pattern
x<-x[grep("violation|violate|assumption|assuming|assumed|correct|adjust|checked|data|analys|theorem",tolower(x))]
# set x to "" if length==0
if(length(x)==0) x<-""
# collapse x
x<-paste(x,collapse=" ")
hits<-lapply(x,which.term,c(
"equal variances|equal variance|equality of variance",
"homogenous variance|homogenous variances|homogenity|[^a-z]hov[^a-z]",
"sphericity",
"normal distribution|normal distribut|^normality[^a][^t][^e] normality",
"multivariate normal",
"linearity|linear",
"homoscedasticity|homoscedastic",
"auto correlation|autocor",
"multi collinearity|multicollinearity|multi[- ]collinear|multicollinear",
"missing completely at random|mcar|[^a-z]marc[^a-z]",
"missing at random|mar assumption",
#"[^a-z]mnar|informative missing",
"gauss-marcov|gauss[- ]mar[ck]o[wv]|gau[s\U00DF][- ]mar[ck]o[wv]",
"^mar[ck]ov assumption|[^s\U00DF] mar[ck]ov assumption",
"independency|independence",
"orthogonality",
"monotonicity",
"proportional hazards|proportional[- ]hazard|proportionality assumption| ph assump",
"proportional odds",
"weibull|weibul",
"gaussianity",
"uni dimensionality|unidimensionality|uni[ -]dimensionality",
"local independency|local independence"
),hits_only=hits_only)

if(hits_only==TRUE){
  out<-unique(unlist(hits))
  out<-gsub("\\|.*","",out)
  return(out)
}
if(hits_only==FALSE) return(hits)
}
