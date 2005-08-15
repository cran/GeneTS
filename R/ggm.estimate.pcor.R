### ggm.estimate.pcor  (2005-06-07)
###
###     Various small-samples estimators of GGM partial correlation coefficients
###
### Copyright 2003-05 Juliane Schaefer and Korbinian Strimmer
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


# estimate partial correlation coefficients
ggm.estimate.pcor <- function(x, method=c("shrinkage", "observed.pcor",
  "partial.bagged.cor", "bagged.pcor"), R=1000, ...)
{
  method <- match.arg(method)

  ########
  
  if (method == "shrinkage")
  {  
     return( pcor.shrink(x, ...) )  
  } 
 
  ########
  
  if (method == "observed.pcor")
  {    
    return( cor2pcor( cor(x), 
            exact.inversion=FALSE, check.eigenvalues=FALSE))
  } 

  ########
  
  if (method == "partial.bagged.cor")
  {
     cor.bag <- cor.bagged(x, R=R, ...)   
     return( cor2pcor(cor.bag, 
             exact.inversion=FALSE, check.eigenvalues=FALSE))
  } 
  
  ########

  if (method == "bagged.pcor")
  {  
     return( pcor.bagged(x, R=R, ...) )
  } 
}


