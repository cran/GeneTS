### ggm.plot.graph  (2004-01-15)
###
###   Plotting the GGM network
###
### Copyright 2003-04 Juliane Schaefer and Korbinian Strimmer
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

# requires the installation of the "graph" library

# generate a graph object from and edge list
# (such as obtained from ggm.test.edges) 
ggm.make.graph <- function(edge.list, num.nodes)
{ 
  # create empty graph with no edges
  V <- as.character(1:num.nodes)
  edL <- vector("list", length=num.nodes)
  names(edL) <- V
  gR <- new("graphNEL", nodes=V, edgeL=edL)
 
  # add edges and edge weights (correlations)
  gX <- addEdge(as.character(edge.list[,2]),
                as.character(edge.list[,3]),
                gR,
                round(edge.list[,1], digits=2) )

  return(gX)
}

# requires installation of the "Rgraphviz" library

# plot network 
ggm.plot.graph <- function(gr, node.labels=NULL)
{
  elab <- weightLabels(gr)
  if (is.null(node.labels))
    plot(gr, "neato", edgeLabels=elab,
      fixedNodeSize=FALSE, nodeShape="ellipse")
  else
    plot(gr, "neato", edgeLabels=elab, nodeLabels=node.labels,
      fixedNodeSize=FALSE, nodeShape="ellipse")
}
