\name{ggm.estimate.pcor}
\alias{ggm.estimate.pcor}

\title{Graphical Gaussian Models: Small Sample Estimation of Partial Correlation}

\usage{
ggm.estimate.pcor(x, method = c("shrinkage", "observed.pcor",
  "partial.bagged.cor", "bagged.pcor"), R = 1000, \dots)
}

\arguments{
  \item{x}{data matrix (each rows corresponds to one multivariate observation)}
  \item{method}{method used to estimate the partial correlation matrix.
     Available options are "shrinkage" (the default), "observed.pcor",
    "partial.bagged.cor", and "bagged.pcor". }
  \item{R}{number of bootstrap replicates (bagged estimators only)}
  \item{\dots}{options passed to  \code{\link[corpcor]{cor.bagged}},
     \code{\link[corpcor]{pcor.bagged}}, and
        \code{\link[corpcor]{pcor.shrink}}.}
   
}
\description{
  \code{ggm.estimate.pcor} implements various small-sample point estimators of partial
  correlation that can be employed also for small sample data sets. Their statistical
  properties are investigated in detail in Schaefer and Strimmer (2005a,b,c).  The basic principle
  behind all the estimators is variance reduction, either nonparametrically (via the bootstrap)
  or in a shrinkage approach.


}

\details{
  The result can be summarized as follows (with n being the sample size, and p being the
  number of variables): 

\emph{shrinkage:} This employs \code{\link[corpcor]{cov.shrink}} to estimate an optimal positive definite
     covariance matrix that subsequently serves as basis to compute the partial correlation
     coefficients.
     This method is very fast (compared to the bootstrap procedures) yet it also produces highly
     accurate estimates (see Schaefer and Strimmer 2005c).
 

\emph{observed.pcor:} Observed partial correlation (Pi-1). Should be used preferentially for n >> p. 
 In this region  the other two estimators perform equally well but are slower due to bagging.

\emph{partial.bagged.cor:}  Partial bagged correlation (Pi-2). Best used for small sample applications 
  with n < p.
 Here the advantages of Pi-2 are its small variance, its high accuracy as a point estimate,
 and its overall best power and positive predictive value (PPV). In addition it is
  computationally less expensive than Pi-3.

\emph{bagged.pcor:} Bagged partial correlation (Pi-3). May be used in the critical zone (n approx. p)
and for sample sizes n slightly
 larger than the number of variables p.
 

As a result, this  particularly promotes the shrinkage estimator as optimal choice
for the inference of GGM networks from small-sample (gene expression) data
(see Schaefer and Strimmer 2005c).  Second best estimator is the partial bagged
correlation Pi-2 (see Schaefer and Strimmer 2005a,b).  
}

\value{
  An estimated partial correlation matrix.
}
\author{
  Juliane Schaefer (\url{http://www.statistik.lmu.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
}
\references{
  Schaefer, J., and Strimmer, K. (2005a).  An empirical Bayes approach to inferring
  large-scale gene association networks. \emph{Bioinformatics} \bold{21}:754-764.
  
  Schaefer, J., and Strimmer, K. (2005b). Learning large-scale graphical Gaussian 
  models from genomic data. Proceedings of CNET 2004, Aveiro, Pt. (AIP)
  
  Schaefer, J., and Strimmer, K. (2005c).  A shrinkage approach to large-scale
  covariance estimation and implications for functional genomics.  Submitted.
  
}
\seealso{\code{\link{ggm.simulate.data}}, \code{\link{ggm.estimate.pcor}}, \code{\link[corpcor]{cov.shrink}}.}

\examples{
\dontrun{

# load GeneTS library
library("GeneTS")

# generate random network with 40 nodes 
# it contains 780=40*39/2 edges of which 5 percent (=39) are non-zero
true.pcor <- ggm.simulate.pcor(40)
  
# simulate data set with 40 observations
m.sim <- ggm.simulate.data(40, true.pcor)

# simple estimate of partial correlations
estimated.pcor <- ggm.estimate.pcor(m.sim, method = c("observed.pcor"))

# comparison of estimated and true model
sum((true.pcor-estimated.pcor)^2)

# a slightly better estimate ...
estimated.pcor.2 <- ggm.estimate.pcor(m.sim, method = c("bagged.pcor"))
sum((true.pcor-estimated.pcor.2)^2)

# this is even better!
estimated.pcor.3 <- ggm.estimate.pcor(m.sim, method = c("shrinkage"))
sum((true.pcor-estimated.pcor.3)^2)

}
}


\keyword{htest}
