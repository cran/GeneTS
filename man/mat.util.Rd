\name{mat.util}
\alias{is.positive.definite}
\alias{rank.condition}
\alias{is.square}
\alias{is.symmetric}

\title{Various Matrix Utilities}
\usage{
is.positive.definite(m, eps = .Machine$double.eps)
is.square(m)
is.symmetric(m, eps = .Machine$double.eps)
rank.condition(m, tol = sqrt(.Machine$double.eps))
}
\arguments{
  \item{m}{matrix}
  \item{eps}{values smaller than < eps are considered zero (e.g., eigenvalues in is.positive.definite()
  and matrix differences in is.symmetric()}
  \item{tol}{relative tolerance - singular values larger than \code{tol} times
           the maximum singular value are considered non-zero}
}
\description{
  \code{is.positive.definite} tests whether all eigenvalues of a matrix
  are positive.
     
  \code{is.square} tests whether a matrix
  has squared form.
  
  \code{is.symmetric} tests whether a matrix is symmetric.
  
   \code{rank.condition} estimates the rank and the condition
  of a matrix by 
  computing its singular values D[i] (using  \code{\link{svd}}).
  The rank of the matrix is the number of singular values D[i] > tol*max(D)
  and the condition is the ratio  of the largest and the smallest
  singular value.

}

\value{
  For \code{is.positive.definite}, \code{is.square}, and \code{is.symmetric}
  a logical value (\code{TRUE} or \code{FALSE}).
  
  For \code{rank.condition} a list object with the following components:
  \item{rank}{Rank of the matrix.}
  \item{condition}{Condition number.}
}


\author{
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\seealso{\code{\link{svd}}, \code{\link{pseudoinverse}}.}

\examples{
# load GeneTS library
library(GeneTS)

# Hilbert matrix
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }

# positive definite ?
m <- hilbert(8)
is.positive.definite(m)

# numerically ill-conditioned
m <- hilbert(15)
rank.condition(m)

# square and symmetric ?
is.square(m)
is.symmetric(m)
}
\keyword{array}
