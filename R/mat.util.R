### mat.util.R  (2004-09-25)
###
###     Some matrix utility functions
###
### Copyright 2003-04 Korbinian Strimmer
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


# checks whether a matrix is positive definite
is.positive.definite <- function (m, tol, method=c("eigen", "chol"))
{
    method <- match.arg(method)
    
    if (method=="eigen")
    {
        eval <- eigen(m, symmetric=TRUE, only.values = TRUE)$values

        if( missing(tol) )
            tol <- max(dim(m))*max(abs(eval))*.Machine$double.eps
   
        if (sum(eval > tol) == length(eval))
            return(TRUE)
        else
            return(FALSE)
    }
    
    if (method=="chol")
    {
	val <- try(chol(m), silent=TRUE)
  
        if (class(val) == "try-error")
            return(FALSE)
        else
            return(TRUE)    
    }
}


# Method by Higham 1988
make.positive.definite <- function(m, tol)
{
  # assumption: A is symmetric!
  d <- dim(m)[1]
  
  es <- eigen(m, symmetric=TRUE)
  esv <- es$values
  
  if (missing(tol))
      tol <- d*max(abs(esv))*.Machine$double.eps 
  delta <-  2*tol # factor to is just to make sure the resulting
                  # matrix passes all numerical tests of positive definitess
  
  tau <- max(0, delta - esv)
  dm <- es$vectors %*% diag(tau, d) %*% t(es$vectors)    
  
  #print(max(DA))
  #print(esv[1]/delta)
      
  return( m +  dm )
}




# rank and condition of a matrix 
rank.condition <- function (m, tol)
{
    d <- svd(m, nv=0, nu=0)$d # compute only singular values
    
    max.d <- d[1]
    min.d <- d[length(d)]
    
    if( missing(tol) ) 
        tol <- max(dim(m))*max.d*.Machine$double.eps
    
    r <- sum(d > tol) # rank: number of singular values larger than tol
    
    if (r < min(dim(m)) ) min.d <- 0 # if matrix is singular then set the  smallest
                                     # singular value to 0, and hence condition = INF
    
    c <- max.d/min.d
    
    return(list(rank = r, condition = c, tol=tol))
}


# checks whether a matrix is a square matrix
is.square <- function(m)
{
  if(nrow(m) == ncol(m))
    return(TRUE)
  else
   return(FALSE)
}

# checks whether a matrix is symmetric
is.symmetric <- function(m, eps = .Machine$double.eps)
{
  if ( is.square(m) == FALSE) return(FALSE)

  num.col = ncol(m)
  for (i in 1:(num.col-1))
    for (j in (i+1):num.col)
    {      
      if (  abs(m[i,j] - m[j,i]) >= eps  ) 
      {
	return(FALSE)
      }
    
    }
   return(TRUE)
}
