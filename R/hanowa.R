# Hanowa matrix -----------------------------------------------------------

#' @name hanowa
#' @title Hanowa matrix
#'
#' @description Matrix whose eigenvalues lie on vertical plane in complex plane.
#' Returns a 2-by-2 block matrix with four \code{n/2} by \code{n/2} blocks. \code{n}
#' must be an even integer.
#'
#' \code{[d*eye(m) -diag(1:m),}
#' \code{diag(1:m) d*eye(m)]}
#'
#' @param n order of matrix
#' @param d value of main diagonal
#'
#' @return Matrix whose eigenvalues lie on a vertical line in the complex plane.
#'
#' @export
hanowa <- function(n, d = NULL){
  if(is.null(d)){d <- -1}

  if(n %% 2 != 0){stop("hanowa:OddN")}

  m <- n / 2

  A <- rbind( cbind(d*diag(m), -diag(1:m)),
              cbind(diag(1:m), d*diag(m)) )

  return(A)
}
