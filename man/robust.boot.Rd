\name{robust.boot}
\alias{robust.boot}

\title{Robust Error Resistant Bootstrap Algorithm}

\usage{
robust.boot(data, statistic, R)
}

\arguments{
  \item{data}{data matrix or data frame (each row is considered as one multivariate observation)}
  \item{statistic}{A function which when applied to data returns a vector
          containing the statistic(s) of interest}
  \item{R}{number of bootstrap replicates}
}
\description{
  \code{robust.boot} generates ordinary nonparametric bootstrap replicates. If an error occurs during the
  function evaluation (e.g., due to numerical problems) the bootstrap draw is repeated. 

  \code{robust.boot} offers only very limited bootstrap support, for much more advanced bootstrapping methods
   use \code{\link{boot}}.
 
}

\details{
  \code{robust.boot} is used in the functions \code{\link{bagged.cov}}, \code{\link{bagged.cov}},
  and \code{\link{bagged.pcor}}.
  
}
\value{
  A list with one component:
  \item{t}{a matrix with 'R' rows each of which is a bootstrap replicate of 'statistic'.}


  
}
\author{
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
}

\seealso{\code{\link{boot}}, \code{\link{bagged.pcor}}.}

\examples{
# load GeneTS library
library(GeneTS)

# small example data set 
data(caulobacter)
dat <- caulobacter[,1:15]
dim(dat)

# test statistic: vector of means 
test.fun <- function(data, i)
{
  res <- apply(data[i,], 2, mean) 
  if (runif(1) < .01) stop("Error!") # in 1 percent of cases an error occurs ... 
  return(res)
}

# perform bootstrap
b.out <- robust.boot(dat, test.fun, 1000)

# despite the errors bootstrapping has finished
dim(b.out$t)

# bootstrap means
bag <- apply(b.out$t, 2, mean)
bag
}
\keyword{nonparametric}
