### zzz.R  (2004-03-15)
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
  
  # for R = 1.8.1: we need to load the "ts" and "modereg" packages (used in fdr.estimate.eta0)
  # from R 1.9 on "ts" and "modreg" are merged into the "stats" package 
  if (R.version$major == "1" &&  R.version$minor == "8.1")
  {
     library(ts)
     library(modreg)
  }
  else
  {
     library(stats)
  }
 
  # load the MASS package (for pseudoinverse and robust variance estimates)
  library(MASS)
  
  
  # try to load the Biobase and graph package
  cat("Loading the \"Biobase\" and \"graph\" packages ...\n")
  GRAPH.LIB <- TRUE
  if (class(try( {library(Biobase);library(graph)}, silent=TRUE)) == "try-error")
  {
    cat("The \"Biobase\" respectively the \"graph\" package from Bioconductor is not installed, therefore network plotting is unavailable in GeneTS.\n")
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
