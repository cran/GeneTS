### mat.util.R  (2004-11-18)
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
