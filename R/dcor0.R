### dcor0.R  (2004-01-15)
###
###    Distribution of the Correlation Coefficient (rho=0) 
###    and Related Functions
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


# incomplete beta function
ibeta <- function(z, a, b)
{
  pbeta(z, a, b)*beta(a,b)
}


# exact likelihood for rho=0
dcor0 <- function(x, kappa, log=FALSE)
{
  r <- x
  if(log)
  {
    return(
      log(kappa - 1) - kappa*log(2) +
      (kappa - 3)*log(1 - r*r)/2 +
      lgamma(kappa) - 2*lgamma((1 + kappa)/2)
    )
  }
  else
  {
    return(      
      ((kappa-1)* (1-r*r)^((kappa-3)/2)*gamma(kappa))/
      ((2^kappa)*gamma((1+kappa)/2)^2) 
    )
  }
}


# distribution function for rho=0
pcor0 <- function(q, kappa, lower.tail=TRUE, log.p=FALSE)
{
  value <-
    0.5*(1 + (q*ibeta(q*q, 0.5, (-1 + kappa)/2)*gamma(kappa/2))/
    (sqrt(pi) * sqrt(q*q)*gamma((-1 + kappa)/2)))
  
  if (lower.tail == FALSE)
    value <- 1-value
    
  if (log.p == TRUE)
    value <- log(value)
  
  return(value)
}


# random number generator
rcor0 <- function(n, kappa)
{
  #If n is a vector, length(n) values will be generated
  if (length(n) > 1)
    num.values <- length(n)
  else
    num.values <- n
  
  cvec <- rep(NA, num.values)
  for (i in 1:num.values)
  {
     x1 <- rnorm(kappa+1)
     x2 <- rnorm(kappa+1)
     cvec[i] <- cor(x1,x2)
  }  
    
  return(cvec)
}


