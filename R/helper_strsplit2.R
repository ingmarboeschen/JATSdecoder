#' strsplit2
#'
#' Extension of strsplit(). Makes it possible to split lines before or after a pattern match without removing the pattern.
#' @param x text string to process.
#' @param split pattern to split text at.
#' @param type one out of c("remove", "before", "after").
#' @param perl Logical. If TRUE uses perl expressions.
#' @return A list of the same length as x, the i-th element of which contains the vector of splits of x[i].
#' @export
#' @examples
#' x<-"This is some text, where text is the split pattern of the text."
#' strsplit2(x,"text","after")

## strsplit2
strsplit2 <- function(x,
                     split,
                     type = "remove",
                     perl = FALSE
                     ) {
  if (type == "remove") {
    # use base::strsplit
    out <- base::strsplit(x = x, split = split, perl = perl)
  } else if (type == "before") {
    # split before the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=.)(?=", split, ")"),
                          perl = TRUE
                          )
  } else if (type == "after") {
    # split after the delimiter and keep it
    out <- base::strsplit(x = x,
                          split = paste0("(?<=", split, ")"),
                          perl = TRUE
                          )
  } else {
    # wrong type input
    stop("type must be remove, after or before!")
  }
  return(out)
}
