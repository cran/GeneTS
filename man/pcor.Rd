\name{pcor}
\alias{pcor}
\alias{cor2pcor}
\alias{pcor2cor}

\title{Partial Correlation from Correlation Matrix (and Vice Versa)}
\usage{
cor2pcor(m, exact.inversion=FALSE, \dots)
pcor2cor(m, exact.inversion=FALSE, \dots)
pcor(x, use=c("all.obs", "complete.obs", "pairwise.complete.obs"),
   method=c("pearson", "kendall", "spearman"), exact.inversion=FALSE, \dots)
}
\arguments{
  \item{m}{covariance matrix or (partial) correlation matrix}
  \item{x}{data matrix or data frame}
  \item{exact.inversion}{determines whether the inverse is computed
        exactly (using \code{\link{solve}}) or via \code{\link{pseudoinverse}}  }
  \item{use}{an optional character string giving a method for computing covariances in the
             presence of missing values. This must be one of the strings "all.obs" (default),
	     "complete.obs" or "pairwise.complete.obs".}
  \item{method}{a character string indicating which correlation coefficient (or covariance) is to be computed.
                One of "pearson" (default), "kendall", or "spearman".}
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
  
  \code{pcor} computes a partial correlation matrix directly from the
  data (\code{pcor(x)} is the same as \code{cor2pcor(cor(x))}).
  
  The underlying algorithms are based on computing the inverse of the
  covariance or correlation matrix - see Whittaker (1990) for details.
  For stability reasons and to allow near-singular matrices  the default
  matrix inversion is obtained via the function  \code{\link{pseudoinverse}} 
  rather than using \code{\link{solve}}.
}

\value{
  A matrix with the pairwise partial correlation coefficients
  (\code{cor2pcor} and \code{pcor}) or with pairwise
  correlations (\code{pcor2cor})
}


\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
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
m.cor.1 <- standardize.cov(m.cov)
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
}
\keyword{multivariate}
