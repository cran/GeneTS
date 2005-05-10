### cor.fit.mixture.R  (2005-05-11)
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
cor.fit.mixture <- function(r, MAXKAPPA=5000, fA.type=c("nonparametric", "uniform"))
{
  if ( any(r > 1) || any(r < -1) )
      stop("Data out of range: input correlations must be in [-1; 1]")
  
  fA.type <- match.arg(fA.type) 

  if (fA.type == "nonparametric")
  {
     require("locfdr")
     
     out <- locfdr(z.transform(r))
     eta0 <- as.double( out$fp0[3] ) # p0
     sigma <- as.double( out$fp0[2] ) # sig
     kappa <- 1/(sigma*sigma) + 2 # Fisher's rule
     logL <- NA
     prob.nonzero <- 1-out$fdr
  }


  if (fA.type == "uniform")
  {
  
    # ML estimate based on mixture distribution
  
    MINLOGL = -sqrt(.Machine$double.xmax)
     
    logL.fun <- function(x)
    {
      kappa <- x[1]
      eta0 <- x[2]
      
      if (kappa < 1 || kappa > MAXKAPPA || eta0 < 0 || eta0 > 1)
      {
        logL <- MINLOGL
      }
      else
      {
        logL <- sum(  log(eta0*dcor0(r,kappa)+0.5*(1-eta0)) ) # mixture distribution
      }
         
      return(logL)
    }
       
    # find ML estimate 
    kappa.guess <- cor0.estimate.kappa(r)
    xstart <- c(kappa.guess, 0.9)
            
    #out <- optim(xstart, logL.fun, method="BFGS",
    #    control=list(fnscale=-1))   
              
    #out <- optim(xstart, logL.fun, method="BFGS",
    #   control=list(fnscale=-1, parscale=c(1,0.05)))   
                
    out <- optim(xstart, logL.fun, method="Nelder-Mead",
        control=list(fnscale=-1))   
      
    kappa <- out$par[1]
    eta0 <- out$par[2]
    logL<- out$value
    
    if (abs(kappa - MAXKAPPA) < 0.1) warning("Estimated kappa close to given MAXKAPPA.")


    # compute empirical posterior probability that true correlation is non-zero
    p0 <- eta0*dcor0(r, kappa)
    pA <- (1-eta0)*0.5 # (1-eta0)*unif(r, -1, 1)
       
    prob.nonzero <- pA/(p0+pA)
  }
  
  
  return( list(kappa=kappa, eta0=eta0, logL=logL, prob.nonzero=prob.nonzero) )
}




