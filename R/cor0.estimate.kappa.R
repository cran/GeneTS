### cor0.estimate.kappa.R (2004-01-15)
###
###    Estimating the Degree of Freedom of the Distribution
###    of the Sample Correlation Coefficient (assuming rho=0)
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



# estimate the degree of freedom
cor0.estimate.kappa <- function(r, method=c("fisher", "likelihood", "robust"), w=1.0)
{
  method <- match.arg(method)
  
  z <- z.transform(r) 
  
  if(method == "robust") # Fisher's rule with robust estimate of variance
  {   
    v <- (hubers(z, mu=0, k=w)$s)^2 # robust estimate
    kappa <- round(1/v+2)
  }
  
  if(method == "fisher") # Fisher's rule
  {
    v <- sum(z*z)/(length(z)-1) # variance around 0
    #v <- var(z)
    kappa <- round(1/v+2)
  }
  
  if(method == "likelihood") # ML estimate based on null-distribution
  {
     logL.fun <- function(kappa)
     {
       sum(dcor0(r,kappa,log=TRUE))
     }
     
     # find ML estimate 
     LARGESTKAPPA <- 200
     out <- optimize(logL.fun, c(1, LARGESTKAPPA), maximum = TRUE)
          
     m <- out$maximum
     m1 <- floor(m)
     m2 <- ceiling(m)
     l1 <- logL.fun(m1)
     l2 <- logL.fun(m2)
     if (l1 > l2)
      kappa <- m1
     else
      kappa <- m2
  }
    
  return( kappa )
}

