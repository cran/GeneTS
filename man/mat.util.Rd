\name{mat.util}
\alias{is.positive.definite}
\alias{make.positive.definite}
\alias{rank.condition}
\alias{is.square}
\alias{is.symmetric}

\title{Various Matrix Utilities}
\usage{
is.positive.definite(m, tol, method=c("eigen", "chol"))
make.positive.definite(m, tol)
rank.condition(m, tol)
is.square(m)
is.symmetric(m, eps = .Machine$double.eps)

}
\arguments{
  \item{m}{matrix}
  
  \item{tol}{tolerance for singular values and for absolute eigenvalues
              -  only those with values larger than
             tol are considered non-zero (default:
	     \code{tol = max(dim(m))*max(D)*.Machine$double.eps})
	     }
   \item{method}{Determines the method to check for positive definiteness:
     eigenvalue computation (\code{eigen}, default) or Cholesky decomposition
     (\code{chol}).}
     
	     
   \item{eps}{values smaller than < eps are considered zero }
}
\description{
  \code{is.positive.definite} tests whether all eigenvalues of a symmetric matrix
  are positive.
  
  \code{make.positive.definite} computes the nearest positive definite of a
  real symmetric matrix, using the algorithm of NJ Higham (1988, Linear Algebra
  Appl. 103:103-118).
  

   \code{rank.condition} estimates the rank and the condition
  of a matrix by 
  computing its singular values D[i] (using  \code{\link{svd}}).
  The rank of the matrix is the number of singular values D[i] > tol)
  and the condition is the ratio  of the largest and the smallest
  singular value.

     
  \code{is.square} tests whether a matrix
  has squared form.
  
  \code{is.symmetric} tests whether a matrix is symmetric.
  

}

\value{
  For \code{is.positive.definite}, \code{is.square}, and \code{is.symmetric}
  a logical value (\code{TRUE} or \code{FALSE}).
  
  For \code{rank.condition} a list object with the following components:
  \item{rank}{Rank of the matrix.}
  \item{condition}{Condition number.}
  \item{tol}{Tolerance.}
  
  For \code{make.positive.definite} a symmetric positive definite matrix.
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

# make positive definite
m2 <- make.positive.definite(m)
is.positive.definite(m2)
rank.condition(m2)
m2 - m

# square and symmetric ?
is.square(m)
is.symmetric(m)
}
\keyword{array}
