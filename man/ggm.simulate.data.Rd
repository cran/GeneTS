\name{ggm.simulate.data}
\alias{ggm.simulate.data}

\title{Graphical Gaussian Models: Simulation of of Data}

\usage{
ggm.simulate.data(sample.size, pcor)
}

\arguments{
  \item{sample.size}{sample size of simulated data set}
  \item{pcor}{partial correlation matrix}
}
\description{
  \code{ggm.simulate.data} takes a positive definite partial correlation matrix and
  generates an iid sample from the corresponding standard multinormal distribution 
  (with mean 0 and variance 1).
}


\value{
  A multinormal data matrix.
}
\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\references{
  Schaefer, J., and Strimmer, K. (2003).  A practical approach to inferring
  large graphical models from sparse microarray data. Submitted to
  \emph{Bioinformatics} [preprint available online].
}
\seealso{\code{\link{ggm.simulate.pcor}}, \code{\link{ggm.estimate.pcor}}.}

\examples{
# load GeneTS library
library(GeneTS)

# generate random network with 40 nodes 
# it contains 780=40*39/2 edges of which 5 percent (=39) are non-zero
true.pcor <- ggm.simulate.pcor(40)
  
# simulate data set with 40 observations
m.sim <- ggm.simulate.data(40, true.pcor)

# simple estimate of partial correlations
estimated.pcor <- partial.cor(m.sim)

# comparison of estimated and true model
sum((true.pcor-estimated.pcor)^2)

# a slightly better estimate ...
estimated.pcor.2 <- ggm.estimate.pcor(m.sim, method = c("bagged.pcor"))
sum((true.pcor-estimated.pcor.2)^2)
}
\keyword{multivariate}
