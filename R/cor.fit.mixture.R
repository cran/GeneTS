### cor.fit.mixture.R  (2004-01-15)
###
###    Fit mixture model to empirical distribution of (partial)
###    correlation coefficients.
###    
###
### Copyright 2003-04 Juliane Schaefer and Korbinian Strimmer
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



# fit mixture to empirical (partial) correlations
cor.fit.mixture <- function(r)
{
  # ML estimate based on mixture distribution
  
     logL.fun <- function(x)
     {
       kappa <- x[1]
       eta0 <- x[2]
       etaA <- 1-eta0
       
       logL <- sum(  log(eta0*dcor0(r,kappa)+0.5*etaA) ) # mixture distribution
       
       if (is.na(logL))
       {
         warning("log likelihood function produced NA or NaN!!")
       }
       
       logL
     }
     
     # find ML estimate 
     LARGESTKAPPA <- 170 # otherwise dcor0 produces NaNs..
     xstart <- c(10, 0.5)
     lo <- c(2, 0)
     up <- c(LARGESTKAPPA, 1)
     
     out <- optim(xstart, logL.fun, method="L-BFGS-B",
       lower=lo, upper=up, control=list(fnscale=-1))   
     
     m <- out$par[1]
     m1 <- floor(m)
     m2 <- ceiling(m)
     
     l1 <- logL.fun(c(m1, out$par[2]))
     l2 <- logL.fun(c(m2, out$par[2]))
     if (l1 > l2)
     {
      kappa <- m1
      logL <- l1
     }
     else
     {
      kappa <- m2
      logL <- l2
     }
     eta0 <- out$par[2]
    
  
  return( list(kappa=kappa, eta0=eta0, logL=logL) )
}


# posterior probability that true correlation is non-zero
cor.prob.nonzero <- function(r, kappa, eta0)
{
    p0 <- eta0*dcor0(r, kappa)
    pA <- (1-eta0)*0.5 # (1-eta0)*unif(r, -1, 1)
       
    prob <- pA/(p0+pA)
    
    return(prob) 
}

