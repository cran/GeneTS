### cor0.estimate.kappa.R (2004-09-15)
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
cor0.estimate.kappa <- function(r, method=c("fisher", "likelihood", "robust"), MAXKAPPA=5000, w=1.0)
{
  method <- match.arg(method)
  
  z <- z.transform(r) 
  
  if(method == "fisher") # Fisher's rule
  {
    v <- sum(z*z)/(length(z)-1) # variance around 0
    #v <- var(z)
    kappa <- 1/v +2
  }

  if(method == "robust") # Fisher's rule with robust estimate of variance
  {   
    require(MASS)
    v <- (hubers(z, mu=0, k=w)$s)^2 # robust estimate
    kappa <- 1/v +2
  }
  
  if(method == "likelihood") # ML estimate based on null-distribution
  {
     logL.fun <- function(kappa)
     {
       sum(dcor0(r,kappa,log=TRUE))
     }
     
     # find ML estimate 
     out <- optimize(logL.fun, c(1, MAXKAPPA), maximum = TRUE)
          
     kappa <- out$maximum
  }
    
  return( kappa )
}

