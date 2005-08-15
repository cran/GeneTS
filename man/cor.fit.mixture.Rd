\name{cor.fit.mixture}
\alias{cor.fit.mixture}

\title{Graphical Gaussian Models: Fit Mixture Distribution to Sample Correlation Coefficients}
\usage{
cor.fit.mixture(r, MAXKAPPA=5000, fA.type=c("nonparametric", "uniform"), df=7, plot.locfdr=0)
}
\arguments{
  \item{r}{vector of sample correlations}
  \item{fA.type}{assumed type of alternative distribution}
  \item{MAXKAPPA}{upper bound for the estimated \code{kappa} (default: MAXKAPPA=5000)}
  \item{df}{degrees of freedom for the spline fitting the density (only if fA.type="nonparametric")}
  \item{plot.locfdr}{controls plot option in \code{locfdr} }
}
\description{

  \code{cor.fit.mixture} fits a mixture model 
  
    \code{f(r) = eta0 dcor0(r, kappa) + (1-eta0) etaA fA}
  
  to a vector of empirical partial correlation coefficients using likelihood maximization.
  This allows to estimates both the degree of freedom \code{kappa} in the
  null-distribution and the proportion eta0 of null r-values.  The alternative distribution
  is either assumed to be the unform \code{dunif(r, -1, 1)}, or that it is an arbitrary
  nonparametric distribution which vanishes for values of r near the center r=0. 
  
  \code{cor.fit.mixture} also computes

    \code{etaA fA/f(r)}, 

  i.e. the (empirical) posterior probability that the true correlation is non-zero given the empirical
  correlation r, the degree of freedom of the null-distribution kappa, and the prior eta0
  for the null-distribution. 
}

\details{
  The above functions are useful to determine the null-distribution of edges in a sparse graphical
  Gaussian model, see Schaefer and Strimmer (2005) for more details and an application to infer 
  genetic networks from microarray data.
  
  For details on how to fit the empirical null distribution while at the same time non-parametrically
  estimating the alternative hypothesis see Efron (2004) and the associated R package \code{locfdr}.
}

\value{
  A list object with the following components:
  \item{kappa}{the degree of freedom of the null distribution (see \code{\link{dcor0}})} 
  \item{eta0}{the prior for the null distribution, i.e. the proportion of null r-values}
  \item{logL}{the maximized log-likelihood (only if \code{fA.type="uniform"})}
  \item{prob.nonzero}{empirical posterior probability that the observed correlations
                      are non-zero.}
}


\author{
  Juliane Schaefer (\url{http://www.statistik.lmu.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
}
\references{
  Efron, B. (2004). Large-scale simulataneous hypothesis testing: the choice
  of a null hypothesis. \emph{JASA} \bold{99}:96-104.

  Schaefer, J., and Strimmer, K. (2005).  An empirical Bayes approach to inferring
  large-scale gene association networks. \emph{Bioinformatics} \bold{21}:754-764.
}

\seealso{\code{\link{dcor0}}, \code{\link{cor0.estimate.kappa}}, 
\code{\link{kappa2n}}, \code{\link{fdr.estimate.eta0}}.}

\examples{
# load GeneTS library
library("GeneTS")

# simulate mixture distribution
r <- rcor0(700, kappa=10)
u <- runif(200, min=-1, max=1)
rc <- c(r,u)

# estimate kappa and eta0 (=7/9)
c1 <- cor.fit.mixture(r, fA.type="uniform")
c1$eta0
c1$kappa
c2 <- cor.fit.mixture(rc, fA.type="uniform") 
c2$eta0
c2$kappa

# for comparison
cor0.estimate.kappa(r)
cor0.estimate.kappa(rc)
}
\keyword{htest}
