### ggm.plot.graph  (2004-03-15)
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
  if (!is.graph.loaded())
  {
    stop("This function requires the installation of the \"graph\" package from Bioconductor")
  }
  else
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
}

# print vector of edge weights
show.edge.weights <- function(gr)
{
  if (!is.graph.loaded())
  {
    stop("This function requires the installation of the \"graph\" package from Bioconductor")
  }
  else
  {   
    if(isBioC13())
    {
      edgeWeightVector(gr)
    }
    else
    {
      em <- edgeMatrix(gr)
      eWV(gr, em)           
    }
  }
}



# requires installation of the "Rgraphviz" library

# plot network 
ggm.plot.graph <- function(gr, node.labels=NULL, ...)
{
  if (!is.Rgraphviz.loaded())
  {
    stop("This function requires the installation of the \"Rgraphviz\" package from Bioconductor")
  }
  else
  {   
    if (isBioC13()) 
    {
      elab <- weightLabels(gr)
      if (is.null(node.labels))
        plot(gr, "neato", edgeLabels=elab,
          fixedNodeSize=FALSE, nodeShape="ellipse", ...)
      else
        plot(gr, "neato", edgeLabels=elab, nodeLabels=node.labels,
          fixedNodeSize=FALSE, nodeShape="ellipse", ...)
    }
    else
    {
      # general graph attributes
      gAttrs <- getDefaultAttrs("neato")
      gAttrs$edge$color <- "black"    
      gAttrs$node$shape <- "ellipse"
      gAttrs$node$fixedsize <- FALSE
      
      if (!is.null(node.labels))
      {
        # node attributes
        # node.labels are given by the user
        node.names <- nodes(gr)
      
        nAttrs <- list()
        nAttrs$label <- node.labels
        names(nAttrs$label) <- node.names
      }
  
      #  edge attributes
      em <- edgeMatrix(gr)
      emv <- eWV(gr, em, sep="~")
      edge.names <- names(emv)
      edge.labels <- as.character(emv)
      
      eAttrs <- list()
      eAttrs$label <- edge.labels
      names(eAttrs$label) <- edge.names
      
      # negative correlation edges in grey
      eAttrs$color <- rep("black", length(edge.labels))
      eAttrs$color[emv < 0] <- "lightgrey"
      names(eAttrs$color) <- edge.names
  
      if (is.null(node.labels))
      {
        plot(gr, "neato", attrs=gAttrs, edgeAttrs = eAttrs, ...)
      }
      else
      {
        plot(gr, "neato", attrs=gAttrs, nodeAttrs=nAttrs, edgeAttrs = eAttrs, ...)
      }
    }
   
  }
}

