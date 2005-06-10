\name{cov.shrink}
\alias{cov.shrink}
\alias{cor.shrink}
\alias{varcov}

\title{Shrinkage Estimation of Covariance and Correlation Matrix}
\description{
  Thes functions \code{cov.shrink} and \code{cor.shrink} implement a shrinkage approach to estimate covariance and
  correlation matrices.
  
  In comparison with the standard methods (e.g. \code{\link{cov}} and \code{\link{cor}})
   the shrinkage estimates are always 1)  positive definite, 2)  well conditioned (so that the inverse
  always exists), and 3) exhibit (in part dramatically) better mean squared error.
  
  
  \code{varcov} computes both the standard empirical covariance matrix (\eqn{S}) and  a matrix 
   containing the estimated variances\eqn{ var(s_{ij})} of the individual components of S.
 
}
\usage{
cov.shrink(X, lambda, verbose=TRUE)
cor.shrink(X, lambda, verbose=TRUE)
varcov(X, type=c("unbiased", "ML"), verbose=TRUE)}
\arguments{
  \item{X}{a data matrix}
  \item{lambda}{the shrinkage intensity (range 0-1). It is is not specified (the default) lambda is
         chosen such that the resulting shrinkage estimate has minimal MSE (see below for details).}
  \item{verbose}{report progress while computing (default: TRUE)}
  \item{type}{estimate unbiased or ML variant of empirical covariance matrix
              (default: "unbiased", as in \code{\link{cov}})}
}
\details{
   
  Let \eqn{S} be the usual empirical unbiased estimator of the true covariance matrix 
  \eqn{\Sigma} with \eqn{E(S)=\Sigma},
  and the target \eqn{F} an unbiased estimator of a structured covariance matrix 
  (with \eqn{E(F)=\Phi}.  Then a shrinkage
  estimator \eqn{S^{*}} can be constructed by setting  
  
      
     \deqn{S^{*} = (1-\lambda) S + \lambda F.}
      
  
  If the shrinkage intensity  \eqn{\lambda} is large, then the resulting estimate 
  will be close the structured
  estimate \eqn{F}, otherwise the unstructured estimate \eqn{S} dominates.   
  Ledoit and Wolf (2003) suggest to estimate \eqn{\lambda} from the observed data such
   that the mean squared error (MSE)
  of \eqn{S^{*}} is minimal. Note that by construction the MSE of \eqn{S^{*}} 
   can only improve relative to both \eqn{S} and \eqn{F}.
  
  In \code{cov.shrink} the target \eqn{F} is the diagonal matrix \eqn{diag(S)}. 
  The above equation then simplifies to a shrinkage estimator of the offdiagonal elements only, i.e.
  with \eqn{s^{*}_{ii} = s_{ii}}
  and \eqn{s^{*}_{ij} = (1-\lambda) s_{ij}} for \eqn{i \neq j}.
 It then can be shown (Schaefer and Strimmer 2005) that an estimator for the optimal intensity is
 given by
  
  \deqn{\lambda^{*} = \sum_{i \neq j} var(s_{ij}) / \sum_{i \neq j} s_{ij}^2  .}
    
    
  This shrinkage are especially useful when computing sparse large-scale covariance
  matrices in a 'small n, large p' setting. These situations are often encountered
  in problems bioinformatics and statistical genomics (see Schaefer and Strimmer 2005 for examples).
  
  
}
\value{
  \code{cov.shrink} returns a matrix with the shrinkage estimate of the covariance matrix.
  
  \code{cor.shrink} returns the corresponding standardized matrix.  
  
  \code{varcov} returns a list with the following two components:
  \item{S}{ the empirical covariance matrix.   } 
  \item{var.S}{ a matrix containing the variances of each element in S.}
}
\author{
  Juliane Schaefer (\url{http://www.statistik.lmu.de/~schaefer/}) 
  and Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
  
}
\references{

  Ledoit, O., and Wolf. M. (2003).  Improved estimation of the covariance
  matrix of stock returns with an application to portfolio selection.
  \emph{J. Emp. Finance} \bold{10}:503-621. 

  Schaefer, J., and Strimmer, K. (2005).  A shrinkage approach to large-scale
  covariance estimation and implications for functional genomics.  
  
}
\seealso{\code{\link{cov}}, \code{\link{cor}}, \code{\link{ggm.estimate.pcor}}.}

\examples{
# load GeneTS library
library(GeneTS)

# small n, large p
p <- 100
n <- 20

# generate random pxp covariance matrix
sigma <- matrix(rnorm(p*p),ncol=p)
sigma <- crossprod(sigma)+ diag(rep(0.1, p))

# simulate multinormal data of sample size n  
sigsvd <- svd(sigma)
Y <- t(sigsvd$v \%*\% (t(sigsvd$u) * sqrt(sigsvd$d)))
X <- matrix(rnorm(n * ncol(sigma)), nrow = n) \%*\% Y


# estimate covariance matrix
s1 <- cov(X)
s2 <- cov.shrink(X)

# MSE
sum((s1-sigma)^2)
sum((s2-sigma)^2)

# varcov produces the same results as cov
vc <- varcov(X)
sum(abs(vc$S-s1))

# compare positive definitess
is.positive.definite(s1)
is.positive.definite(s2)
is.positive.definite(sigma)

# compare ranks and condition
rank.condition(s1)
rank.condition(s2)
rank.condition(sigma)

# compare eigenvalues
e1 <- eigen(s1, symmetric=TRUE)$values
e2 <- eigen(s2, symmetric=TRUE)$values
e3 <- eigen(sigma, symmetric=TRUE)$values
m <-max(e1, e2, e3)
yl <- c(0, m)

par(mfrow=c(1,3))
plot(e1,  main="empirical")
plot(e2,  ylim=yl, main="shrinkage")
plot(e3,  ylim=yl, main="true")
par(mfrow=c(1,1))

}
\keyword{multivariate}
