### pcor.R  (2004-01-15)
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
  m <- standardize.cov(m)
  
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
  return(standardize.cov(m))
}


#
# backtransformation to correlation matrix
#
# input: partial correlation matrix
# ouput: correlation matrix
pcor2cor <- function(m, exact.inversion=FALSE, ...)
{
  # standardize
  m <- standardize.cov(m)

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
  return(standardize.cov(m))
}


#
# compute partial correlations given the data x 
#
pcor <- function(x, 
   use=c("all.obs", "complete.obs", "pairwise.complete.obs"),
   method=c("pearson", "kendall", "spearman"),
   exact.inversion=FALSE, ...)
{
  use <- match.arg(use)
  method <- match.arg(method)
  
  return( cor2pcor(cor(x, use=use, method=method), exact.inversion, ...) )
}
