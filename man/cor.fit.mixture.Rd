\name{cor.fit.mixture}
\alias{cor.fit.mixture}
\alias{cor.prob.nonzero}

\title{Graphical Gaussian Models: Fit Mixture Distribution to Sample Correlation Coefficients}
\usage{
cor.fit.mixture(r, MAXKAPPA=5000)
cor.prob.nonzero(r, kappa, eta0)
}
\arguments{
  \item{r}{vector of sample correlations}
  \item{kappa}{the degree of freedom of the null distribution}
  \item{eta0}{the proportion of true null r-values (=the prior)}
  \item{MAXKAPPA}{upper bound for the estimated kappa (default: MAXKAPPA=5000)}
}
\description{

  \code{cor.fit.mixture} fits a mixture model 
  
    \code{f(r) = eta0 dcor0(r, kappa) + etaA dunif(r, -1, 1)}
  
  to a vector of empirical partial correlation coefficients using likelihood maximization.
  This allows to estimates both the degree of freedom \code{kappa} in the
  null-distribution and the proportion eta0 of null r-values (note that etaA = 1-eta0).
  
  \code{cor.prob.nonzero} computes

    \code{etaA dunif(r, -1, 1)/f(r)}, 

  i.e. the posterior probability that the true correlation is non-zero given the empirical
  correlation r, the degree of freedom of the null-distribution kappa, and the prior eta0
  for the null-distribution. 
}

\details{
  The above functions are useful to determine the null-distribution of edges in a sparse graphical
  Gaussian model, see Schaefer and Strimmer (2004) for more details and an application to infer 
  genetic networks from microarray data.
}

\value{
  A list object with the following components:
  \item{kappa}{the degree of freedom of the null distribution} 
  \item{eta0}{the prior for the null distribution, i.e. the proportion of null r-values}
  \item{logL}{the maximized log-likelihood}
}


\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\references{
Schaefer, J., and Strimmer, K. (2004).  An empirical Bayes approach to inferring
  large-scale gene association networks. \emph{Bioinformatics} in press.
}

\seealso{\code{\link{dcor0}}, \code{\link{cor0.estimate.kappa}}, 
\code{\link{kappa2N}}, \code{\link{fdr.estimate.eta0}}.}

\examples{
# load GeneTS library
library(GeneTS)

# simulate mixture distribution
r <- rcor0(700, kappa=10)
u <- runif(200, min=-1, max=1)
rc <- c(r,u)

# estimate kappa and eta0 (=7/9)
cor.fit.mixture(r)
cor.fit.mixture(rc) 

# for comparison
cor0.estimate.kappa(r)
cor0.estimate.kappa(rc)

# posterior probabilities
r <- seq(-1,1, .01)
post.prob <- cor.prob.nonzero(r,kappa=10,eta0=7/9)
plot(r, post.prob, type="l")
}
\keyword{htest}
