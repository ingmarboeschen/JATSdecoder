#' preCheck
#'
#' Performs prechecks and returns error messages for wrong input. If x is a textual input, x is returned unprocessed. If x is a file, x is read with readLines() and returned.
#' @param x anything
#' @return The original input, error messages, empty objects or the read file content with readLines().
#' @keywords internal
#' @export


preCheck<-function(x){
# check if x is an object else return standard error message
if(length(x)>0) if(is.na(x)[1]) tryCatch(x,error=function(e) message(e))
# check if x is of length 0
if(length(x)==0) return(character(0))
# check if x is NA
if(is.na(x)[1]) return(character(0))
# check if x is character
stopifnot('"x" must be a NISO-JATS coded file or text' = is.character(x[1]))
# readLines if x is file
if(file.exists(x[1])){
  # check if x is of length=1
  stopifnot('"x" must have length 1'=length(x)==1)
  # read file content
  x<-readLines(x,warn=FALSE,encoding="UTF-8")
}

return(x)
}