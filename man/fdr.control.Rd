\name{fdr.control}
\alias{fdr.control}
\title{Controlling the False Discovery Rate in Multiple Testing}
\usage{
fdr.control(p, Q=0.05, eta0=1.0, robust=FALSE)
}
\description{
  \code{fdr.control} controls the False Discovery Rate (FDR) at a
  given level Q.
}

\arguments{
  \item{p}{vector of p-values}
  \item{Q}{desired FDR level}
  \item{eta0}{proportion of null p-values (default: eta0=1).}
  \item{robust}{use small sample approximation for estimating q-values (default: robust=FALSE)}
}
\details{
  The FDR is the expected proportion
  of false positives (erroneous rejections) among the significant tests (rejections).
 \code{fdr.control} uses the algorithms described in Benjamini and Hochberg (1995)
  and Storey (2002).  
  For a given vector of p-values and the desired FDR level Q the corresponding p-value
  cut-off and the q-values for each hypothesis (see Storey, 2002) are computed. 

Notes:
\enumerate{
 \item the default settings correspond to the step-up procedure to control the FDR 
    by Benjamini and Hochberg (1995).
    
 \item q-values for each hypothesis are computed as defined in Storey (2002).
 
 \item small sample approximation for q-value (robust=TRUE) is from Storey (2002).
 \item default eta0=0 is safe but also most conservative choice (for other possibilities
    see \code{\link{fdr.estimate.eta0}}).
}
}
\value{

  A list object with the following components:
  \item{qvalues}{a vector with the q-values for each hypothesis.} 
  \item{significant}{a vector with a TRUE/FALSE value for each hypothesis}
  \item{num.significant}{number of significant hypotheses.}
  \item{pvalue.cutoff}{cutoff level for the individual p-values to obtain the
        desired control of FDR.
        Hypotheses whose corresponding p-values are below or equal to this
        cuttoff level are rejected (i.e. significant).}   
}

\references{
 
 Benjamini, Y., and Y. Hochberg (1995)  Controlling the false
 discovery rate: a practical and powerful approach to multiple testing.
 \emph{J. Roy. Statist. Soc. B}, \bold{57}, 289--300.

 Storey, J. D. (2002) A direct approach to false
 discovery rates.
 \emph{J. Roy. Statist. Soc. B.}, \bold{64}, 479--498.
}
\author{
  Konstantinos Fokianos (\url{http://www.ucy.ac.cy/~fokianos/}) and
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
  
  Adapted in part from S-PLUS code by  Y. Benjamini (\url{http://www.math.tau.ac.il/~roee/FDR_Splus.txt})
  and R code from J.D. Storey (\url{http://faculty.washington.edu/~jstorey/}). 
}
\seealso{\code{\link{fdr.estimate.eta0}}.}

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
