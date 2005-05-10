\name{mat.util}
\alias{is.square}
\alias{is.symmetric}

\title{Various Matrix Utilities}
\usage{
is.square(m)
is.symmetric(m, eps = .Machine$double.eps)

}
\arguments{
  \item{m}{matrix}
       
	     
   \item{eps}{values smaller than < eps are considered zero }
}
\description{
      
  \code{is.square} checks whether a matrix
  has squared form.
  
  \code{is.symmetric} checks whether a matrix is symmetric.
  

}

\value{
  \code{is.square} and \code{is.symmetric} return
  a logical value (\code{TRUE} or \code{FALSE}).
  
}


\author{
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
}
\seealso{\code{\link{svd}}, \code{\link{pseudoinverse}}.}

\examples{
# load GeneTS library
library(GeneTS)

# Hilbert matrix
hilbert <- function(n) { i <- 1:n; 1 / outer(i - 1, i, "+") }
m <- hilbert(10)

# square and symmetric ?
is.square(m)
is.symmetric(m)
}
\keyword{array}
