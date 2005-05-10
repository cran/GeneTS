\name{rebuild.cov}
\alias{rebuild.cov}

\title{Rebuild Covariance Matrix from Correlation Matrix}
\usage{
rebuild.cov(r, v)
}
\arguments{
  \item{r}{correlation matrix}
  \item{v}{variance vector}
}
\description{
  
  \code{rebuild.cov} takes a correlation matrix and a vector with variances
  and reconstructs the corresponding covariance matrix.
  
   For the inverse operation use \code{\link{cov2cor}}.

}

\value{

  A covariance matrix.
}


\author{
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
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
m.cor.2 <- cov2cor(m.cov)
m.cor.2

zapsmall(m.cor.1) == zapsmall(m.cor.2)

# reconstruct covariance matrix
rebuild.cov(m.cor.1, m.var)

}
\keyword{multivariate}
