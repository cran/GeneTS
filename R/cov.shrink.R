### cov.shrink.R  (2005-06-07)
###
###    Shrinkage Estimation of Covariance and Correlation Matrix
###
### Copyright 2005 Juliane Schaefer and Korbinian Strimmer
###
###
###
### This file is part of the `GeneTS' library for R and related languages.
### It is made available under the terms of the GNU General Public
### License, version 2, or at your option, any later version,
### incorporated herein by reference.
### 
### This program is distributed in the hope that it will be
### useful, but WITHOUT ANY WARRANTY; without even the implied
### warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
### PURPOSE.  See the GNU General Public License for more
### details.
### 
### You should have received a copy of the GNU General Public
### License along with this program; if not, write to the Free
### Software Foundation, Inc., 59 Temple Place - Suite 330, Boston,
### MA 02111-1307, USA


# compute the empirical covariance matrix S=cov(X) given a data matrix X
# as well as the *variances* associated with the individual entries S[i,j]
#

varcov <- function(X, type=c("unbiased", "ML"), verbose=TRUE)
{
    if (!is.matrix(X)) X <- as.matrix(X)     
    n <- dim(X)[1]
    p <- dim(X)[2]
 
    if (verbose && p > 50)
      cat("Computing empirical covariance matrix and its variance\n")
       
     
    # weights for the "unbiased" and "ML" cases
    type <- match.arg(type)
    if (type=="unbiased")
    {
      h1 <- 1/(n-1)
      h2 <- n/(n-1)/(n-1)
    }    
    if (type=="ML")
    {
      h1 <- 1/n
      h2 <- (n-1)/n/n
    }
 
    s <- matrix(NA, ncol=p, nrow=p)   
    vs <- matrix(NA, ncol=p, nrow=p)
    Xc <- scale(X, scale=FALSE) # center the data
    
    # diagonal elements
    for (i in 1:p)
    {
      zii <- Xc[,i]^2
      s[i,i] <- sum(zii)*h1
      vs[i,i] <- var(zii)*h2
    }
    
    if (p == 1) return(list(S=s, var.S=vs))
    
    if (verbose && p > 50)
      cat(paste("Wait for", p, "points (50 per row):\n")) 
    
    # off-diagonal elements
    for (i in 1:(p-1))
    {
      if (verbose && p > 50)
      {
        cat(".")
        if (i %% 50 == 0) cat(paste(" ", i, "\n"))
      }
      
      for (j in (i+1):p)
      {
        zij <- Xc[,i]*Xc[,j] 
	s[i,j] <- sum(zij)*h1
        s[j,i] <- s[i,j]
        
        vs[i,j] <- var(zij)*h2
        vs[j,i] <- vs[i,j]	 
      }
      
    }
    if (verbose && p > 50) cat(paste(". ", i+1, "\n"))

    return(list(S=s, var.S=vs))
}

# standardized cov.shrink 
cor.shrink <- function(X, lambda, verbose=TRUE)
{
  return( cov2cor(cov.shrink(X, lambda, verbose)) )
}


# compute shrinkage covariance matrix
# 
# 
# S* = (1-lambda) S + lambda F
#
# S is the unbiased unstructured empirical covariance matrix
# the target F is the unbiased diagonal empirical covariance matrix
#
# if lambda is not specified then it is chosen to minimize the MSE

cov.shrink <- function(X, lambda, verbose=TRUE)
{
  
  vc <- varcov(X, type="unbiased", verbose)
  p <- dim(vc$S)[1]
  
  if (p == 1) return( vc$S )

  if (verbose && p > 50)
    cat("Computing shrinkage covariance matrix\n")
     
     
  # find optimal lambda (for p > 1)
  if (missing(lambda))
  {  
    #offdiagsum.sij.2 <- 0
    #offdiagsum.v.sij <- 0
    #for (i in 1:(p-1))
    #{
    #  for (j in (i+1):p)
    #  {
    #    offdiagsum.sij.2 <- offdiagsum.sij.2 + vc$S[i,j]*vc$S[i,j]
    #    offdiagsum.v.sij <- offdiagsum.v.sij + vc$var.S[i,j]
    #  }
    #}  
       
    offdiagsum.sij.2 <- sum(vc$S[lower.tri(vc$S)]^2)
    offdiagsum.v.sij <- sum(vc$var.S[lower.tri(vc$var.S)])
        
    lambda <- offdiagsum.v.sij/offdiagsum.sij.2
    if (verbose) cat(paste("Estimated shrinkage intensity lambda: ", round(lambda,4), "\n"))
  }
  
  if (lambda > 1)
  {
    warning(paste("Shrinkage intensity lambda set to 1 (allowed range: 0-1)"))
    lambda <- 1  
  }
  if (lambda < 0)
  {
     warning(paste("Shrinkage intensity lambda set to 0 (allowed range: 0-1)"))
     lambda <- 0  
  }

 
  # construct shrinkage estimator
  S.star <- (1-lambda)*vc$S
  diag(S.star) <- diag(vc$S)
  
  return( S.star )
}
