\name{cor0.estimate.kappa}
\alias{cor0.estimate.kappa}

\title{Estimating the Degree of Freedom of the Null Distribution
       of the Correlation Coefficient }
\usage{
cor0.estimate.kappa(r, method=c("fisher", "likelihood", "robust"), w=1.0)
}
\arguments{
  \item{r}{vector of sample correlations (assumed true value of rho=0)}
  \item{method}{use Fisher's rule (\code{fisher}),
                optimize likelihood function of null distribution (\code{likelihood}), or
		use Fisher's rule with robust estimate of variance (\code{robust}),
		}
  \item{w}{winsorize at `w' standard deviations (only for robust estimate of variance)}		
}
\description{

  \code{cor0.estimate.kappa} estimates the degree of freedom \code{kappa} in the
  null-distribution of the correlation coefficient (i.e. assuming that rho=0).
  
  According to Fisher's rule \code{kappa = round(1/var(z) + 2)} the degree of freedom
  can be estimated from the variance of the z-transformed sample correlations. 
  
  Maximum-likelihood estimates of the degree of freedom can be obtained 
  on the basis of the null distribution of the sample correlation coefficient
  (i.e. assuming rho = 0) using \code{method="likelihood"}. This results
  almost always in the same estimate of kappa as with the simple Fisher's rule.
  
  If \code{method="robust"} then the variance employed in Fisher's rule
  is estimated using the Huber M-estimate of the scale.  This is useful
  if the null-distribution is slightly "contaminated". 
  
  The degree of freedom \code{kappa} depends both on the sample size N and the number 
  G of investigated variables, 
  i.e. whether simple or partial correlation coefficients are being considered.
  For G=2 (simple correlation coefficient) the degree of freedom equals kappa = N-1,
  whereas for arbitrary G (with G-2 variables eliminated in the partial correlation coefficient)
  kappa = N-G+1 (see also \code{\link{dcor0}} and \code{\link{kappa2N}}).

  If the empirical sampling distribution is a mixture
  distribution then use of \code{cor0.estimate.kappa} may not be appropriate;
  instead \code{\link{cor.fit.mixture}} may be used.
}

\value{
  The estimated degree of freedom kappa.
}


\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}

\seealso{\code{\link{dcor0}}, \code{\link{z.transform}}, 
  \code{\link{hubers}}, \code{\link{kappa2N}}, \code{\link{cor.fit.mixture}}.}

\examples{
# load GeneTS library
library(GeneTS)

# distribution of r for kappa=7
x <- seq(-1,1,0.01)
y <- dcor0(x, kappa=7)

# simulated data
r <- rcor0(1000, kappa=7)
hist(r, freq=FALSE, 
  xlim=c(-1,1), ylim=c(0,5))
lines(x,y,type="l")

# estimate kappa
cor0.estimate.kappa(r)
}
\keyword{htest}
