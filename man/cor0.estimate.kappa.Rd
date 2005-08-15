\name{cor0.estimate.kappa}
\alias{cor0.estimate.kappa}

\title{Estimating the Degree of Freedom of the Null Distribution
       of the Correlation Coefficient }
\usage{
cor0.estimate.kappa(r, method=c("fisher", "likelihood", "robust"), MAXKAPPA=5000, w=1.0)
}
\arguments{
  \item{r}{vector of sample correlations (assumed true value of rho=0)}
  \item{method}{use Fisher's rule (\code{fisher}),
                optimize likelihood function of null distribution (\code{likelihood}), or
		use Fisher's rule with robust estimate of variance (\code{robust}),
		}
  \item{MAXKAPPA}{upper bound for the estimated kappa (default: MAXKAPPA=5000);
                  only for likelihood estimate}
  \item{w}{winsorize at `w' standard deviations; only for robust estimate}		
}
\description{

  \code{cor0.estimate.kappa} estimates the degree of freedom \code{kappa} of the
  null-distribution of the correlation coefficient \code{r} using a variety
  of methods. 
  
  If \code{method="fisher"} is selected then \code{kappa} is estimated according
  to Fisher's rule, i.e. \code{kappa = round(1/var(z.transform(r)) + 2)}.
  
  If \code{method="likelihood"} is selected then the ML estimate is computed
  by optimizing \code{kappa} in the null distribution of the sample correlation
  coefficient. This results almost always in the same estimate of \code{kappa}
  as with the simple Fisher's rule.
  
  If \code{method="robust"} then the variance employed in Fisher's rule
  is estimated using the Huber M-estimate of the scale.  This is useful
  if the null-distribution is slightly "contaminated". 
  
  
  
 }
 
\details{ 
    
   The degree of freedom \code{kappa} of the distribution of the sample correlation
   coefficient depends both on the sample size n and the number 
  p of investigated variables. For \code{rho=0} the degree of freedom
  \code{kappa} equals the inverse of the variance of \code{r}.
   
  For p=2 (simple correlation coefficient) the degree of freedom equals kappa = n-1,
  whereas for arbitrary p (with p-2 variables eliminated in the partial correlation
  coefficient)
  kappa = n-p+1 (see also \code{\link{dcor0}} and \code{\link{kappa2n}}).

  If the empirical sampling distribution is a mixture
  distribution then use of \code{cor0.estimate.kappa} may not be appropriate;
  instead \code{\link{cor.fit.mixture}} may be used.
}

\value{
  The estimated degree of freedom \code{kappa}.
}


\author{
  Juliane Schaefer (\url{http://www.statistik.lmu.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
}

\seealso{\code{\link{dcor0}}, \code{\link{z.transform}}, 
  \code{\link{hubers}}, \code{\link{kappa2n}}, \code{\link{cor.fit.mixture}}.}

\examples{
# load GeneTS library
library("GeneTS")

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
