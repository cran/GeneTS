\name{ggm.plot.graph}
\alias{ggm.plot.graph}
\alias{ggm.make.graph}
\alias{show.edge.weights}

\title{Graphical Gaussian Models: Plotting the Network}

\usage{
ggm.make.graph(edge.list, num.nodes)
show.edge.weights(gr)
ggm.plot.graph(gr, node.labels=NULL, show.edge.labels=TRUE, col.pos="black", col.neg="grey", \dots)
}

\arguments{
  \item{edge.list}{a data frame, as obtained by \code{\link{ggm.test.edges}}, listing all
          edges to be included in the graph}
  \item{num.nodes}{the total number of nodes in the network}
  \item{gr}{a graph object}
  \item{node.labels}{a vector with labels for each node (optional)} 
  \item{show.edge.labels}{plot correlation values as edge labels (default: TRUE)}
  \item{col.pos}{edge color for positive correlation (default: "black")}
  \item{col.neg}{edge color for positive correlation (default: "grey")}
  \item{\dots}{options passed to \code{plot.graph}}
}
\description{
  \code{ggm.make.graph} converts an edge list as obtained by \code{\link{ggm.test.edges}}
  into a graph object.

  \code{show.edge.weights} summarizes a graph object by prints a vector of weights for all
  edges contained in a graph.
  This function is convenient to gain a first impression of the graph (in particular if
  the "Rgraphviz" library is not installed). 

  \code{ggm.plot.graph} visualizes the network structure of the graphical Gaussian model 
  using the Rgraphviz network plot package. The correlation coefficients are
  printed as edge labels.
}

\details{
  The network plotting functions require the installation of the "graph" and "Rgraphviz" R
  packages. These are available from the Bioconductor website
  (\url{http://www.bioconductor.org}).  Note that it is not necessary to install the
  complete set of Bioconductor packages, only "graph" and "Rgraphviz" are needed by the
  GeneTS package (however, these may in turn require additional packages from Bioconductor).

  \code{ggm.plot.graph} is a simple utility function to plots the graph in "neato" format
  with ellipsoid node shapes.  See the documentation of \code{plot.graph} in the "Rgraphviz"
  package for many other options.
 
}
\value{
  \code{ggm.make.graph} returns a graph object, suitable for plotting with functions from
  the "Rgraphviz" library.
  
  \code{show.edge.weights} returns a vector of weights for all edges contained in a graph.
  
  \code{ggm.plot.graph} plots  the network on the current graphic device.
}
\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\seealso{
\code{\link{ggm.test.edges}},\code{plot.graph}.
}

\examples{
# load GeneTS library
library(GeneTS)
 
# generate random network with 20 nodes and 10 percent edges (=19 edges)
true.pcor <- ggm.simulate.pcor(20, 0.1)

# convert to edge list 
test.results <- ggm.test.edges(true.pcor, eta0=0.9, kappa=1000)[1:19,]

# generate graph object 
# NOTE: this requires the installation of the "graph" package
# (in the following "try" is used to avoid an error if the library is not installed)
try( gr <- ggm.make.graph( test.results, 20) )
try( gr )
try( show.edge.weights(gr) )

# plot network
# NOTE: this requires the installation of the "Rgraphviz" library
try ( ggm.plot.graph(gr))
nlab <- c("A","B","C","D","E","F","G","H","I","J","K",
            "L","M","N","O","P","Q","R","S", "T")
try( ggm.plot.graph(gr, nlab) )
}
\keyword{hplot}
