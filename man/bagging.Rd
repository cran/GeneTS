\name{bagging}
\alias{bagged.cov}
\alias{bagged.cor}
\alias{bagged.pcor}

\title{Bagged Versions of Covariance and (Partial) Correlation Matrix}

\usage{
bagged.cov(x, R=1000, \dots)
bagged.cor(x, R=1000, \dots)
bagged.pcor(x, R=1000, \dots)
}

\arguments{
  \item{x}{data matrix or data frame}
  \item{R}{number of bootstrap replicates (default: 1000)}
  \item{\dots}{options passed to \code{\link{cov}}, \code{\link{cor}}, and \code{\link{partial.cor}} 
             (e.g., to control handling of missing values) }
}
\description{
  \code{bagged.cov}, \code{bagged.cor}, and \code{bagged.pcor} calculate
  the bootstrap aggregated (=bagged) versions of the covariance and
  (partial) covariance estimators.  
  
  The bagged covariance and correlation estimators are advantageous especially
  for small sample size problems. For example, the bagged correlation matrix typically remains positive
  definite even when the sample size is much smaller than the number of variables.
  
  In Schaefer and Strimmer (2005) the inverse of the bagged correlation matrix
  is used to estimate graphical Gaussian models from sparse microarray data - 
  see also \code{\link{ggm.estimate.pcor}} for various strategies to
  estimate partial correlation coefficients.
}

\details{
  Bagging was first suggested by Breiman (1996) as a means to improve
  and estimator using the bootstrap. The bagged estimate is simply the
  mean of the bootstrap sampling distribution. 
  
  Bagging is essentially a non-parametric variance reduction method.
  The bagged estimate can also be interpreted
  as (approximate) posterior mean estimate assuming some implicit prior.
  
}
\value{
  A symmetric matrix.
}
\author{
  Juliane Schaefer (\url{http://www.statistik.lmu.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
}
\references{
  Breiman, L. (1996). Bagging predictors. \emph{Machine Learning}, \bold{24}, 123--140.

  Schaefer, J., and Strimmer, K. (2005).  An empirical Bayes approach to inferring
  large-scale gene association networks. \emph{Bioinformatics} \bold{21}:754-764.
  
  Schaefer, J., and Strimmer, K. (2005). Learning large-scale graphical Gaussian 
  models from genomic data. Proceedings of CNET 2004, Aveiro, Pt. (AIP)
  
  
}
\seealso{\code{\link{cov}}, \code{\link{cor}}, \code{\link{partial.cor}},
          \code{\link{ggm.estimate.pcor}}, \code{\link{robust.boot}}.}

\examples{
# load GeneTS library
library(GeneTS)

# small example data set 
data(caulobacter)
dat <- caulobacter[,1:15]
dim(dat)

# bagged estimates
b.cov <- bagged.cov(dat)
b.cor <- bagged.cor(dat)
b.pcor <- bagged.pcor(dat)

# total squared difference
sum( (b.cov - cov(dat))^2  )
sum( (b.cor - cor(dat))^2  )
sum( (b.pcor - partial.cor(dat))^2  )

# positive definiteness of bagged correlation
is.positive.definite(cor(dat))
is.positive.definite(b.cor)
}
\keyword{multivariate}
