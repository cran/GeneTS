\name{fdr.estimate.eta0}
\alias{fdr.estimate.eta0}
\title{Estimate the Proportion of Null p-Values}
\usage{
fdr.estimate.eta0(p, method=c("conservative", "adaptive", "bootstrap", "smoother"),
    lambda=seq(0,0.95,0.05) )
}
\description{
  \code{fdr.estimate.eta0} estimates the proportion eta0 of null p-values in a given
  vector of p-values. 
}

\arguments{
  \item{p}{vector of p-values}
  \item{method}{algorithm used to estimate the proportion of null p-values.
     Available options are
    "conservative" (default), "adaptive", "bootstrap", and "smoother".}
   \item{lambda}{optional tuning parameter vector needed for "bootstrap"
     and "smoothing" methods (defaults to \code{seq(0,0.95,0.05)})
     - see Storey (2002) and Storey and Tibshirani (2003).}
  }

\details{

This quantity \code{eta0}, i.e.  the proportion eta0 of null p-values in a given
  vector of p-values, is an important parameter 
  when controlling  the false discovery rate (FDR).  A conservative choice is
  eta0 = 1 but a choice closer to the true value will increase efficiency
  and power 
  - see   Benjamini and Hochberg (1995, 2000) and Storey (2002) for details.

The function \code{fdr.estimate.eta0} provides four algorithms: the "conservative"
method always returns eta0 = 1  (Benjamini and Hochberg, 1995), "adaptive"
uses the approach suggested in Benjamini and Hochberg (2000), "bootstrap"
employs the method from Storey (2002), and "smoother" uses the smoothing spline
approach in Storey and Tibshirani (2003).
}
\value{
  The estimated proportion eta0 of null p-values. 
}

\references{
 
 \emph{"conservative" procedure:} Benjamini, Y., and Y. Hochberg (1995)  Controlling the false
 discovery rate: a practical and powerful approach to multiple testing.
 \emph{J. Roy. Statist. Soc. B}, \bold{57}, 289--300.

 \emph{"adaptive" procedure:} Benjamini, Y., and Y. Hochberg (2000) The adaptive control
 of the false discovery rate in multiple hypotheses testing with independent statistics.
 \emph{J. Behav. Educ. Statist.}, \bold{25}, 60--83.
 
 \emph{"bootstrap" procedure:} Storey, J. D. (2002) A direct approach to false
 discovery rates.
 \emph{J. Roy. Statist. Soc. B.}, \bold{64}, 479--498.
 
 \emph{"smoother" procedure:} Storey, J. D., and R. Tibshirani (2003)
  Statistical significance for genome-wide experiments. 
 \emph{Proc. Nat. Acad. Sci. USA}, \bold{100}, 9440-9445.

}
\author{
  Konstantinos Fokianos (\url{http://www.ucy.ac.cy/~fokianos/}) and
  Korbinian Strimmer (\url{http://www.www.statistik.lmu.de/~strimmer/}).
  
  Adapted in part from S-PLUS code by  Y. Benjamini (\url{http://www.math.tau.ac.il/~roee/FDR_Splus.txt})
  and R code from J.D. Storey (\url{http://faculty.washington.edu/~jstorey/}). 
}
\seealso{\code{\link{fdr.control}}.}

\examples{
# load mixfdr library
library("GeneTS")


# simulate case-control data
p <- 100 # total number of variables
d <- 20  # number of cases where case and control differs
n <- 5   # sample size per group

r1a <- rnorm(d*n,     mean = 3, sd = 1)
r1b <- rnorm((p-d)*n, mean = 0, sd = 1)
r2 <- rnorm(p*n, mean = 0, sd = 1)
x1 <- matrix(c(r1a, r1b), nrow = p, byrow = TRUE) # group 1.
x2 <- matrix(r2, nrow = p, byrow = TRUE) # group 2.

# p-values from t-test
pval <- numeric(p)
for(i in 1:p) pval[i] <- t.test(x1[i, ], x2[i, ])$p.value


# Proportion of null p-values for different methods
fdr.estimate.eta0(pval)
fdr.estimate.eta0(pval, method="adaptive")
fdr.estimate.eta0(pval, method="bootstrap")
fdr.estimate.eta0(pval, method="smoother")


# FDR test on the level 0.05 (assuming eta0=0
fdr.control(pval, Q = 0.05)  

# FDR test on the level 0.05 (estimating eta0)
e0 <- fdr.estimate.eta0(pval, method="adaptive")
fdr.control(pval, Q = 0.05, eta0=e0)  
}
\keyword{htest}
