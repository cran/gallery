#' @importFrom pracma hilb

# Involutory matrix -----------------------------------------------------------

#' @name invol
#' @title Involutory matrix (a matrix that is its own inverse)
#'
#' @description a \code{n}-by\code{n} involutory matrix and ill-conditioned.
#' It is a diagonally scaled version of a Hilbert matrix.
#'
#' @param n order of matrix
#'
#' @return Involutory matrix (a matrix that is its own inverse).
#'
#' @export
invol <- function(n){
  A <- hilb(n)

  d <- -n
  A[, 1] <- d*A[, 1]

  for(i in 1:(n-1)){
    d <- -(n+i)*(n-i)*d/(i^2)
    A[i+1, ] <- d*A[i+1, ]
  }
  return(A)
}
