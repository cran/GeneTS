### bagging.R  (2004-02-15)
###
###     Bagged estimators of cov, cor, and pcor
###
### Copyright 2003-04 Juliane Schaefer and Korbinian Strimmer
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




# bagged estimators

bagged.cov <- function(x, R=1000, ...)
{
  vec.out <- bag.fun(cov, x, R=R, diag=TRUE, ...)
  mat.out <- vec2sm(vec.out, diag=TRUE)
  
  return( mat.out )
}

bagged.cor <- function(x, R=1000, ...)
{
  vec.out <- bag.fun(cor, x, R=R, diag=FALSE, ...)
  mat.out <- vec2sm(vec.out, diag=FALSE)
  diag(mat.out) <- rep(1, dim(mat.out)[1]) # fill diagonal with 1
  
  return( mat.out )
}

bagged.pcor <- function(x, R=1000, ...)
{
  vec.out <- bag.fun(pcor, x, R=R, diag=FALSE, ...)
  mat.out <- vec2sm(vec.out, diag=FALSE)
  diag(mat.out) <- rep(1, dim(mat.out)[1]) # fill diagonal with 1
  
  return( mat.out )
}

# internal

bag.fun <- function(fun, data, R, diag, ...)
{
  # number of variables 
  p <- dim(data)[2]
  
  # index vector for lower triangle
  lo <- lower.tri(matrix(NA, nrow=p, ncol=p), diag=diag)

  # bootstrap function
  boot.fun <- function(data, i) 
  {
    vec <- as.vector( fun(data[i,], ...)[lo] )
      
    # if we get NAs flag result as being erroneous
    if (sum(is.na(vec)) > 0) class(vec) <- "try-error"

    return( vec )
  }   
     
  #bag variable 
  #boot.out <- boot(data=data, statistic=boot.fun, R=R)
  boot.out <- robust.boot(data=data, statistic=boot.fun, R=R)
  
  bag <- apply( boot.out$t, 2, mean)
    
  return( bag )
}


