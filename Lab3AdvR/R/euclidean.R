#' The Euclidean Algorithm is a technique used for finding the Greatest Common divisor of two integers.
#'
#' @param x A number.
#' @param y A number.
#' @return The sum of \code{x} and \code{y}.
#' @examples
#' euclidean(123612, 13892347912)
#' euclidean(100, 1000)
#' 
#' @references 
#' #' \href{https://en.wikipedia.org/wiki/Euclidean_algorithm}{Euclidean}

#' @export


#1.1.1 euclidean()
euclidean <- function(x, y){
  
  stopifnot(is.numeric(x), is.numeric(y))
  x = abs(x)
  y = abs(y)
  while (y != 0)
  {
    z = x %% y
    x = y
    y = z
  }
  return (x)
}