### standardize.cov.R (2004-01-15)
###
###    Determine Correlation Matrix from Covariance Matrix, and
###    Rebuild Covariance Matrix from Correlation Matrix and Variances
###    
###
### Copyright 2003-04 Korbinian Strimmer
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
# standardize covariance matrix
#
# input:  covariance matrix    m(ij)
# output: correlation matrix   m(ij)/sqrt(m(ii)*m(jj))
#
standardize.cov <- function(m)
{
  d <- diag(m)
    
  if( sum(d == 1.0) == length(d) )
    return(m) # already standardized
    
    if (sum(d > 0) != length(d))
    {
       	warning("zero/negative diagonal elements present - check numerical accuracy")
	
	# workaround - be careful with results!
        d[d == 0] <- .Machine$double.eps  # make zero entries positive	
	d[d < 0] <- -d[d < 0] # take absolute value of negative entries
    }
    
  # standardize
  resid.sd <- 1/sqrt(diag(m))
  cr <- sweep(sweep(m, 1, resid.sd, "*"), 2, resid.sd, "*") 
  
  # correct numerical glitches
  cr[cr > 1] <- 1
  cr[cr < -1] <- -1
  
  return(cr)
}


#
# rebuild covariance matrix
#
# input:  correlation matrix           rho(ij)
#         vector with variances        var(i) 
# output: correlation matrix   rho(ij)*sqrt(var(i)*var(j))
#
rebuild.cov <- function(r, v)
{
  resid.sd <- sqrt(v)
  m <- sweep(sweep(r, 1, resid.sd, "*"), 2, resid.sd, "*") 
    
  return(m)
}


