\name{fdr.estimate.eta0}
\alias{fdr.estimate.eta0}
\title{Estimate the Proportion of Null p-Values}
\usage{
fdr.estimate.eta0(p, method=c("conservative", "adaptive", "bootstrap", "smoother"),
    lambda=seq(0,0.95,0.05) )
}
\description{
  \code{fdr.estimate.eta0} estimates the proportion eta0 of null p-values in a given
  vector of p-values. This quantity is an important parameter 
  when controlling  the false discovery rate (FDR).  A conservative choice is
  eta0 = 1 but a choice closer to the true value will increase efficiency
  and power 
  - see   Benjamini and Hochberg (1995, 2000) and Storey (2002) for details.
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
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
  
  Adapted in part from S-PLUS code by  Y. Benjamini (\url{http://www.math.tau.ac.il/~roee/FDR_Splus.txt})
  and R code from J.D. Storey (\url{http://faculty.washington.edu/~jstorey/}). 
}
\seealso{\code{\link{fdr.control}}.}

\examples{
# load GeneTS library
library(GeneTS)

# load data set
data(caulobacter)

# how many genes and how many samples?
dim(caulobacter)

# p-values from Fisher's g test
pval.caulobacter <- fisher.g.test(caulobacter)

# Proportion of null p-values for different methods
fdr.estimate.eta0(pval.caulobacter)
fdr.estimate.eta0(pval.caulobacter, method="adaptive")
fdr.estimate.eta0(pval.caulobacter, method="bootstrap")
fdr.estimate.eta0(pval.caulobacter, method="smoother")
}
\keyword{htest}
