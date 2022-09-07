#' grep2
#'
#' Extension of grep(). Allows to identify and extract cells with/without multiple search patterns that are connected with AND. 
#' @param pattern Character vector containing regular expression as cells to be matched in the given character vector.
#' @param x A character vector where matches are sought, or an object which can be coerced by as.character to a character vector. Long vectors are supported.
#' @param value Logical. If FALSE, a vector containing the (integer) indices of the matches determined by grep2 is returned, and if TRUE, a vector containing the matching elements themselves is returned.
#' @param invert Logical. If TRUE return indices or values for elements that do not match.
#' @param perl Logical. Should Perl-compatible regexps be used?
#' @return grep2(value = FALSE) returns a vector of the indices of the elements of x that yielded a match (or not, for invert = TRUE). This will be an integer vector unless the input is a long vector, when it will be a double vector. \cr\cr grep2(value = TRUE) returns a character vector containing the selected elements of x (after coercion, preserving names but no other attributes).
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
  x<-as.character(x)
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
  
