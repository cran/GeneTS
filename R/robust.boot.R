### robust.boot.R  (2004-02-15)
###
###     Reobust error resistant bootstrap algorithm
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


# simple bootstrap function (robust against errors)
robust.boot <- function(data, statistic, R)
{
  idx <- 1:dim(data)[1]
  
  # determine dimension of statistic
  repeat
  {
    bx <- sample(idx, replace=TRUE)
    val <- try(statistic(data, bx)) 
    
    if (class(val) != "try-error") break
  }
  dim.statistic <- length(val)
  output <- matrix(nrow=R, ncol=dim.statistic)
  
  replicate.count <- 0
  error.count <- 0
  while (replicate.count < R)
  {
    bx <- sample(idx, replace=TRUE)
    val <- try(statistic(data, bx)) 
    
    if (class(val) == "try-error") # if we get a numerical error we simply repeat the draw ..
    {
      error.count <- error.count+1
      #cat("Bootstrapping continues, drawing an alternative bootstrap sample ...\n")
      
      if (error.count > R) stop("Too many errors encountered during the bootstrap.")
    }
    else
    {
      replicate.count <- replicate.count+1
      output[replicate.count,] <- val
    }
  }
  
  if (error.count > 0) warning(paste(error.count, "out of", R, "bootstrap samples were repeated due to errors."))
  
  return(list(t=output))
} 

