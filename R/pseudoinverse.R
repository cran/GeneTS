### pseudoinverse.R  (2004-09-15)
###
###    Computation of the Pseudoinverse of a Matrix
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



# pseudoinverse of a matrix
pseudoinverse <- function (m, tol)
{
    if (length(dim(m)) > 2 || !(is.numeric(m) || is.complex(m)))
        stop("m must be a numeric or complex matrix")
    if (!is.matrix(m))
        m <- as.matrix(m)
    
    msvd <- svd(m)
    if (is.complex(m))
        msvd$u <- Conj(msvd$u)
    
    if( missing(tol) )
        tol <- max(dim(m))*msvd$d[1]*.Machine$double.eps
     
    Positive <- msvd$d > tol  # use only singular values larger than tol
    if (all(Positive))
        return( 
            msvd$v %*% (1/msvd$d * t(msvd$u))
            )
    else if (!any(Positive)) 
        return(
	    array(0, dim(m)[2:1])
	    )
    else 
        return(
            msvd$v[, Positive, drop = FALSE] %*%
	    ((1/msvd$d[Positive]) * t(msvd$u[, Positive, drop = FALSE]))
	    )
}
