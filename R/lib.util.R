### mat.util.R  (2004-03-15)
###
###     Library utility functions
###
### Copyright 2004 Korbinian Strimmer
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


# check Rgraphviz/BioC version
isBioC13 <- function()
{
  # if R = 1.8.1 we can't be running anything other than BioC 1.3
  if (R.version$major == "1" &&  R.version$minor == "8.1")
  {
     return(TRUE)
  }
  else
  {
     return(FALSE)
  }
}

# check whether graph package is loaded 
is.graph.loaded <- function()
{
 if (sum(.packages() == "graph") > 0)
   return(TRUE)
 else
   return(FALSE)
}

# check whether Rgraphviz package is loaded 
is.Rgraphviz.loaded <- function()
{
 if (sum(.packages() == "Rgraphviz") > 0)
   return(TRUE)
 else
   return(FALSE)
}

