\name{ggm.estimate.pcor}
\alias{ggm.estimate.pcor}

\title{Graphical Gaussian Models: Small Sample Estimation of Partial Correlation}

\usage{
ggm.estimate.pcor(x, method = c("observed.pcor", "partial.bagged.cor", "bagged.pcor"), R = 1000, \dots)
}

\arguments{
  \item{x}{data matrix (each rows corresponds to one multivariate observation)}
  \item{method}{method used to estimate the partial correlation matrix.
     Available options are "observed.pcor" (default),
    "partial.bagged.cor", and "bagged.pcor". }
  \item{R}{number of bootstrap replicates (bagged estimators only)}
  \item{\dots}{options passed to \code{\link{partial.cor}}, \code{\link{bagged.cor}},
        and \code{\link{bagged.pcor}}. }
   
}
\description{
  \code{ggm.estimate.pcor} implements various small-sample point estimators of partial
  correlation that can be employed also for small sample data sets. Their statistical
  properties are investigated in detail in Schaefer and Strimmer (2004).


}

\details{
  The result can be summarized as follows (with N being the sample size, and G being the
  number of variables): 

\emph{observed.pcor:} Observed partial correlation (Pi-1). Should be used preferentially for N >> G. 
 In this region  the other two estimators perform equally well but are slower due to bagging.

\emph{partial.bagged.cor:}  Partial bagged correlation (Pi-2). Best used for small sample applications with N < G.
 Here the advantages of Pi-2 are its small variance, its high accuracy as a point estimate,
 and its overall best power and positive predictive value (PPV). In addition it is computationally less expensive than Pi-3.

\emph{bagged.pcor:} Bagged partial correlation (Pi-3). May be used in the critical zone (N = G) and for sample sizes N slightly
 larger than the number of variables G.

As a result, this  particularly promotes the partial bagged correlation Pi-3 as estimator of choice for the inference of GGM networks from small-sample
 (gene expression) data.
}

\value{
  An estimated partial correlation matrix.
}
\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\references{
Schaefer, J., and Strimmer, K. (2004).  An empirical Bayes approach to inferring
  large-scale gene association networks. \emph{Bioinformatics} in press.
}
\seealso{\code{\link{ggm.simulate.data}},\code{\link{ggm.estimate.pcor}}.}

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
\keyword{htest}
