\name{ggm.plot.graph}
\alias{ggm.plot.graph}
\alias{ggm.make.graph}


\title{Graphical Gaussian Models: Plotting the Network}

\usage{
ggm.make.graph(edge.list, num.nodes)
ggm.plot.graph(gr, node.labels=NULL)
}

\arguments{
  \item{edge.list}{a data frame, as obtained by \code{\link{ggm.test.edges}}, listing all
          edges to be included in the graph}
  \item{num.nodes}{the total number of nodes in the network}
  \item{gr}{a graph object}
  \item{node.labels}{a vector with labels for each node (optional)}
}
\description{
  \code{ggm.make.graph} converts an edge list as obtained by \code{\link{ggm.test.edges}}
  into a graph object.

  \code{ggm.plot.graph} plots this graph objects, with the correlation as edge labels.
}

\details{
  The network plotting functions require the installation of the "graph" and "Rgraphviz" R
  packages. These are available from the Bioconductor website
  (\url{http://www.bioconductor.org}).  Note that it is not necessary to install the
  complete set of Bioconductor packages, only "graph" and "Rgraphviz" are needed by the
  GeneTS package.

  \code{ggm.plot.graph} is a simple utility function to plots the graph in "neato" format
  with ellipsoid node shapes.  See the documentation of \code{plot.graph} in the "Rgraphviz"
  package for many other options.
 
}
\value{
  \code{ggm.make.graph} returns a graph object, suitable for plotting with functions from
  the "Rgraphviz" library.

  \code{ggm.plot.graph} plots the network on the current graphic device.
}
\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\seealso{
\code{\link{ggm.test.edges}}.
}

\examples{
# load GeneTS library
library(GeneTS)
 
# generate random network with 20 nodes and 10 percent edges (=19 edges)
true.pcor <- ggm.simulate.pcor(20, 0.1)

# convert to edge list 
test.results <- ggm.test.edges(true.pcor)[1:19,]

# generate graph object 
# NOTE: this requires the installation of the "graph" package
# (in the following "try" is used to avoid an error if the library is not installed)
try( gr <- ggm.make.graph( test.results, 20) )
try( gr )
try( edgeWeightVector(gr) )

# plot network
# NOTE: this requires the installation of the "Rgraphviz" library
try ( ggm.plot.graph(gr))
nlab <- c("A","B","C","D","E","F","G","H","I","J","K",
            "L","M","N","O","P","Q","R","S", "T")
try( ggm.plot.graph(gr, nlab) )
}
\keyword{hplot}
