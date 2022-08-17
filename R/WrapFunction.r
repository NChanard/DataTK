#' WrapFunction
#'
#' Wrap a string in a function
#' @param ... <character>: a String that could be parse and eval as a function.
#' @return the result of the function or a function
#' @examples
#' X <- c(2,4,NA)
#' myString <- "mean(X, na.rm=TRUE)"
#' WrapFunction(myString)
#' myResult <- WrapFunction(myString)
#' myString_2 <- "function(X){mean(X, na.rm=TRUE)}"
#' WrapFunction(myString_2)
#' myFunction <- WrapFunction(myString_2)
#' myFunction(c(1,2,3))

WrapFunction <-function (...) {eval(parse(text=paste(...,collapse=" ")))}