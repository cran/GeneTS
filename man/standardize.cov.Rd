\name{standardize.cov}
\alias{standardize.cov}
\alias{rebuild.cov}

\title{Standardize Covariance Matrix}
\usage{
standardize.cov(m)
rebuild.cov(r, v)
}
\arguments{
  \item{m}{covariance matrix}
  \item{r}{correlation matrix}
  \item{v}{variance vector}
}
\description{
  \code{standardize.cov} takes a covariance matrix and turns it into
  a correlation matrix by standardizing each entry with the product of the 
  corresponding standard deviations (so that all diagonal entries equal 1).
  
  \code{rebuild.cov} takes a correlation matrix and a vector with variances
  and reconstructs the corresponding covariance matrix.
}

\value{

  A matrix (correlation or covariance matrix).
}


\author{
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\seealso{\code{\link{cor}}, \code{\link{cov}}}

\examples{
# load GeneTS library
library(GeneTS)

# first 10 genes from the caulobacter data set
data(caulobacter)
m <- caulobacter[,1:10]

# covariance matrix
m.cov <- cov(m)
m.cov

# variances
m.var <- diag(m.cov)
m.var

# correlation matrix
m.cor.1 <- cor(m)
m.cor.1

# correlation matrix via covariance matrix
m.cor.2 <- standardize.cov(m.cov)
m.cor.2

zapsmall(m.cor.1) == zapsmall(m.cor.2)

# reconstruct covariance matrix
rebuild.cov(m.cor.1, m.var)

}
\keyword{multivariate}
