### ggm.plot.graph  (2004-09-15)
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
    library(graph) # requires the "graph" package from BioC >= 1.5
  
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

# print vector of edge weights
show.edge.weights <- function(gr)
{
    library(graph) # requires the "graph" package from BioC >= 1.5
  
    em <- edgeMatrix(gr, duplicates=FALSE)
      
    return( eWV(gr, em) )           
   
}



# requires installation of the "Rgraphviz" library

# plot network 
ggm.plot.graph <- function(gr, node.labels=NULL, show.edge.labels=TRUE, col.pos="black", col.neg="grey", ...)
{
    library(Rgraphviz) # requires the "Rgraphviz" package from BioC >= 1.5
  
    # general graph attributes
    gAttrs <- getDefaultAttrs(layoutType="neato")
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
    
    if (show.edge.labels)
    {
      eAttrs$label <- edge.labels
      names(eAttrs$label) <- edge.names
    }
      
    # color edges according to positive and negative correlation
    eAttrs$color <- rep(col.pos, length(edge.labels))
    eAttrs$color[emv < 0] <- col.neg
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

