### zzz.R  (2004-01-15)
###
###    Startup of GeneTS package
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


.First.lib <- function(libname, pkgname)
{
  # some startup stuff
  
  # we need the time series library
  library(ts)
 
  # and the MASS library
  library(MASS)

  # try to load the graph library
  cat("Loading the \"graph\" package ...\n")
  GRAPH.LIB <- TRUE
  if (class(try(library(graph), silent=TRUE)) == "try-error")
  {
    cat("The \"graph\" package is not installed, therefore network plotting is not available in GeneTS.\n")
    GRAPH.LIB <- FALSE   
  }

  # try to load the Rgraphviz library
  if (GRAPH.LIB)
  {
    cat("Loading the \"Rgraphviz\" package ...\n")    
    if (class(try(library(Rgraphviz), silent=TRUE)) == "try-error")
    {
      cat("The \"Rgraphviz\" package is not installed, therefore network plotting is not available in GeneTS.\n")
    } 
  }
}
