### kappa2N.R (2004-01-15)
###
###    Conversion of kappa to N and vice versaa 
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



# sample size corresponding to kappa and G
kappa2N <- function(kappa, G=2)
{
  return( kappa+G-1 )
}

# sample size corresponding to N and G
N2kappa <- function(N, G=2)
{
  return( N-G+1 )
}


