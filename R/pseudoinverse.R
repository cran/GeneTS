### pseudoinverse.R  (2004-01-15)
###
###    Computation of the Pseudoinverse of a Matrix
###
### Copyright 2003-04 Korbinian Strimmer
###
### This is essentially a proxy to "ginv" from the MASS package
### by B. Venables and B. Ripley.
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
pseudoinverse <- function (m, tol = sqrt(.Machine$double.eps))
{
    return( ginv(m, tol) )
}
