\name{partial.cor}
\alias{partial.cor}
\alias{cor2pcor}
\alias{pcor2cor}

\title{Partial Correlation from Correlation Matrix (and Vice Versa)}
\usage{
cor2pcor(m, exact.inversion=FALSE, \dots)
pcor2cor(m, exact.inversion=FALSE, \dots)
partial.cor(x, tol)
}
\arguments{
  \item{m}{covariance matrix or (partial) correlation matrix}
  \item{x}{data matrix or data frame}
  \item{exact.inversion}{determines whether the inverse is computed
        exactly (using \code{\link{solve}}) or via \code{\link{pseudoinverse}}  }
 
 \item{tol}{tolerance for \code{\link{pseudoinverse}} - singular values larger than
             tol are considered non-zero (default value:
	     \code{tol = max(dim(m))*max(D)*.Machine$double.eps})
	     }		
  
  
  \item{\dots}{options passed to \code{\link{pseudoinverse}}}
}
\description{
  \code{cor2pcor} computes the pairwise 
  \emph{partial} correlation coefficients from either a correlation 
  or a covariance matrix. The partial correlations represent the direct
  interactions between two variables, with the indirect effects of all
  remaining variables removed.

  \code{pcor2cor} takes a partial correlation matrix and computes
  the corresponding correlation matrix.
  
  \code{partial.cor} computes a partial correlation matrix directly from the
  data.
  
  The underlying algorithms are based on computing the inverse of the
  covariance or correlation matrix - see Whittaker (1990) for details.
  For stability reasons and to allow near-singular matrices  the default
  matrix inversion is obtained via the function  \code{\link{pseudoinverse}} 
  rather than using \code{\link{solve}}.
}

\details{
\code{partial.cor(x, tol)} is \emph{numerically} the same as 
  \code{cor2pcor(cor(x), exact.inversion=FALSE, tol)}.  However, the actually employed algorithm is much faster 
  for small sample size and large number of variables due the use
  of \code{\link{fast.svd}} on x rather than on cor(x)).
}


\value{
  A matrix with the pairwise partial correlation coefficients
  (\code{cor2pcor} and \code{pcor}) or with pairwise
  correlations (\code{pcor2cor})
}


\author{
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\references{
  Whittaker J. (1990).  Graphical Models in Applied Multivariate Statistics.
   John Wiley, Chichester.
}

\seealso{\code{\link{cor}}, \code{\link{pseudoinverse}}.}

\examples{
# load GeneTS library
library(GeneTS)

# covariance matrix
m.cov <- rbind(
 c(3,1,1,0),
 c(1,3,0,1),
 c(1,0,2,0),
 c(0,1,0,2)
)
m.cov

# corresponding correlation matrix
m.cor.1 <- cov2cor(m.cov)
m.cor.1

# compute partial correlations (from covariance matrix)
m.pcor.1 <- cor2pcor(m.cov)
m.pcor.1

# compute partial correlations (from correlation matrix)
m.pcor.2 <- cor2pcor(m.cor.1)
m.pcor.2

zapsmall( m.pcor.1 ) == zapsmall( m.pcor.2 )

# backtransformation
m.cor.2 <- pcor2cor(m.pcor.1)
m.cor.2
zapsmall( m.cor.1 ) == zapsmall( m.cor.2 )

# speed comparison
n <- 20
p <- 800
X <- matrix(rnorm(n*p), n, p)

system.time( p1 <- cor2pcor(cor(X)) )
system.time( p2 <- partial.cor(X) )

# p2 is much faster but still the same as p1!
eps <- 1e-10
sum(abs(p1-p2) > eps)

}
\keyword{multivariate}
