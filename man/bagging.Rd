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
  \item{\dots}{options passed to \code{\link{cov}}, \code{\link{cor}}, and \code{\link{pcor}} 
             (e.g., to control handling of missing values) }
}
\description{
  \code{bagged.cov}, \code{bagged.cor}, and \code{bagged.pcor} calculate
  the bootstrap aggregated (=bagged) versions of the covariance and
  (partial) covariance estimators.  
  
  Theses estimators are advantageous especially for small sample size
  problems. For example, the bagged correlation matrix typically remains positive
  definite even when the sample size is much smaller than the number of variables.
  
  In Schaefer and Strimmer (2003) the inverse of the bagged correlation matrix
  is used to estimate graphical Gaussian models from sparse microarray data - 
  see also \code{\link{ggm.estimate.pcor}} for various strategies to
  estimate partial correlation coefficients.
}

\details{
  Bagging was first suggested by Breiman (1996) as a means to improve
  and estimator using the bootstrap. The bagged estimate is simply the
  mean of the bootstrap sampling distribution. Thus, bagging is essentially
  a variance reduction method. The bagged estimate may also be interpreted
  as (approximate) posterior mean estimate assuming some implicit prior.
  
}
\value{
  A symmetric matrix.
}
\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\references{
  Breiman, L. (1996). Bagging predictors. \emph{Machine Learning}, \bold{24}, 123--140.

  Schaefer, J., and Strimmer, K. (2003).  A practical approach to inferring
  large graphical models from sparse microarray data. Submitted to
  \emph{Bioinformatics} [preprint available online].
}
\seealso{\code{\link{cov}}, \code{\link{cor}}, \code{\link{pcor}},
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
sum( (b.pcor - pcor(dat))^2  )

# positive definiteness of bagged correlation
is.positive.definite(cor(dat))
is.positive.definite(b.cor)
}
\keyword{multivariate}
