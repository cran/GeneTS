\name{kappa2N}
\alias{kappa2N}
\alias{N2kappa}

\title{Determine the Sample Size from the Degree of Freedom of Correlation Distribution}
\usage{
kappa2N(kappa, G=2)
N2kappa(N, G=2)
}
\arguments{
  \item{kappa}{degree of freedom}
  \item{G}{number of variables (G=2 corresponds to simple correlation)}
  \item{N}{sample size}
}
\description{

      
  The degree of freedom kappa of the sample distribution of the correlation
  coefficient depends both on the sample size N and the number G of investigated variables, 
  i.e. whether simple or partial correlation coefficients are being considered.
  For G=2 (simple correlation coefficient) the degree of freedom equals kappa = N-1,
  whereas for arbitrary G (with G-2 variables eliminated in the partial correlation coefficient)
  kappa = N-G+1 (see also \code{\link{dcor0}}).
 
  The function \code{kappa2N} returns the sample size that
  corresponds to a given degree of freedom kappa, whereas \code{N2kappa}
  converts sample size to the corresponding degree of freedom. 
}

\value{
  The sample size N corresponding to a given kappa, or the degree of freedom
  kappa corresponding to a given N.
}


\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}

\seealso{\code{\link{dcor0}}, \code{\link{cor0.estimate.kappa}}.}

\examples{
# load GeneTS library
library(GeneTS)


# sample sizes corresponding to kappa=7
kappa2N(7)     # simple correlation
kappa2N(7, 40) # partial correlation with 40 variables

# degree of freedom corresponding to N=100
N2kappa(100)
N2kappa(100,40)
}
\keyword{univar}
