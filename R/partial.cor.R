### partial.cor.R  (2004-09-25)
###
###    Partial Correlation computed by Inversion 
###    of the Covariance or Correlation Matrix
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


#
# partial correlation matrix
#
# input: covariance matrix or correlation matrix
# ouput: partial correlation matrix
#
cor2pcor <- function(m, exact.inversion=FALSE, ...)
{
  # standardize
  m <- cov2cor(m)
  
  # invert, then negate off-diagonal entries
  if (exact.inversion)
  {
    m <- -solve(m)
  }
  else
  {
    m <- -pseudoinverse(m, ...)
  }
  diag(m) <- -diag(m)

  # standardize and return  
  return(cov2cor(m))
}


#
# backtransformation to correlation matrix
#
# input: partial correlation matrix
# ouput: correlation matrix
pcor2cor <- function(m, exact.inversion=FALSE, ...)
{
  # standardize
  m <- cov2cor(m)

  # negate off-diagonal entries, then invert
  m <- -m
  diag(m) <- -diag(m)
  if (exact.inversion)
  {
    m <- solve(m)
  }
  else
  {
    m <- pseudoinverse(m, ...)
  }
  
  # standardize and return 
  return(cov2cor(m))
}


#
# compute inverse correlation matrix
#
# this is numerically equivalent to pseudoinverse(cor(x)) but much
# faster for n << p 
#
inverse.cor <- function (x, tol)
{
    n <- dim(x)[1]
    p <- dim(x)[2]
    
    xs <- scale(x) # standardize
   
    if (n < p)
    {
        # the following is *much* faster than inverting the
	# p x p cor matrix directly
         
        xsvd <- fast.svd(xs, tol)   # fast svd on "fat" matrix (using svd on n x n matrix)
        if (length(xsvd$d) == 0)
        {
           ic <- array(0, c(p,p))
        }
        else
        {
           ic <- xsvd$v %*% ((n-1)/xsvd$d^2 * t(xsvd$v))
        }
        
    }
    else
    {
	ic <- pseudoinverse(crossprod(xs)/(n-1), tol)   # invert p x p matrix using svd	
    }
      
    return(ic)
}


#
# compute partial correlations given the data x 
#

partial.cor <- function(x, tol)
{
  pc <- -inverse.cor(x, tol)
  diag(pc) <- -diag(pc)
  
  return(cov2cor(pc)) 
}

