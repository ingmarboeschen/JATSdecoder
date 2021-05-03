#' grep2
#'
#' Extension of grep(). Allows to identify and extract cells with/without multiple search patterns that are connected with AND. 
#' @param pattern Character string containing a regular expression (or character string for fixed = TRUE) to be matched in the given character vector. Coerced by as.character to a character string if possible. If a character vector of length 2 or more is supplied, the first element is used with a warning. Missing values are allowed except for regexpr, gregexpr and regexec.
#' @param x A character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
#' @param value Logical. if FALSE, a vector containing the (integer) indices of the matches determined by grep is returned, and if TRUE, a vector containing the matching elements themselves is returned.
#' @param invert Logical. If TRUE return indices or values for elements that do not match.
#' @param perl Logical. Should Perl-compatible regexps be used?
#' @export
#' @examples
#' x<-c("ab","ac","ad","bc","bad")
#' grep2(c("a","b"),x)
#' grep2(c("a","b"),x,invert=TRUE)
#' grep2(c("a","b"),x,value=FALSE)

grep2 <- function(pattern,x,
                     value = TRUE,
                     invert = FALSE, 
                     perl = FALSE
                     ) {
# get index of cells matching all patterns
  for(i in 1:length(pattern)){
    if(i==1) index<-grep(pattern[i],x,perl=perl)
    if(i>1) index<-index[is.element(index,grep(pattern[i],x,perl=perl))]
  }
  # prepare and output
  if(invert==TRUE) index<-(1:length(x))[-index]
  if(length(index)==0) return(integer(0))
  if(value==FALSE) return(index)
  if(value==TRUE) return(x[index])
}
  
