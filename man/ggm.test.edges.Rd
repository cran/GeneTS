\name{ggm.test.edges}
\alias{ggm.test.edges}

\title{Graphical Gaussian Models: Testing Edges}

\usage{
ggm.test.edges(r.mat, MAXKAPPA=5000, kappa=NULL, eta0=NULL)
}

\arguments{
  \item{r.mat}{matrix of partial correlations}
  \item{kappa}{the degree of freedom of the null distribution (will be estimated if left unspecified)}
  \item{eta0}{the proportion of true null values (will be estimated if left unspecified)}
  \item{MAXKAPPA}{upper bound for the estimated kappa - see \code{\link{cor.fit.mixture}} (default: MAXKAPPA=5000)}
}
\description{
  \code{ggm.test.edges} assigns statistical significance to the edges in a GGM network by computing
  p-values, q-values and posterior probabilities for each potential edge.
}

\details{
  A mixture model is fitted to the partial correlations using \code{\link{cor.fit.mixture}}
  (this estimate can be overridden if values for both \code{kappa} and \code{eta0} are specified).
  Subsequently, two-sided p-values to test non-zero correlation are computed for each edge using 
  \code{\link{cor0.test}}. In addition, corresponding posterior probabilities are
  computed using \code{\link{cor.prob.nonzero}}.  Finally, to simplify multiple testing q-values
  are computed via  \code{\link{fdr.control}} whith the specified value of \code{eta0} taken
  into account.

  Theoretical details are explained in Schaefer and Strimmer (2003), along with a simulation
  study and an application to gene expression data.
}
\value{
  A sorted data frame with the following columns:
  
  \item{pcor}{partial correlation (from r.mat)}
  \item{node1}{first node connected to edge}
  \item{node2}{second node connected to edge}
  \item{pval}{p-value}
  \item{qval}{q-value}
  \item{prob}{probability that edge is nonzero}

  Each row in the data frame corresponds to one edge, and the rows are sorted
  according the absolute strength of the correlation (from strongest to weakest)
}
\author{
  Juliane Schaefer (\url{http://www.stat.uni-muenchen.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.stat.uni-muenchen.de/~strimmer/}).
}
\references{
  Schaefer, J., and Strimmer, K. (2003).  A practical approach to inferring
  large graphical models from sparse microarray data. Submitted to
  \emph{Bioinformatics} [preprint available online].
}
\seealso{
\code{\link{cor.fit.mixture}},
\code{\link{cor0.test}},
\code{\link{cor.prob.nonzero}},
\code{\link{fdr.control}},
\code{\link{ggm.estimate.pcor}}.
}

\examples{
# load GeneTS library
library(GeneTS)
 
# generate random network with 20 nodes and 5 percent edges
true.pcor <- ggm.simulate.pcor(20, 0.05)

# simulate data set of length 100
sim.dat <- ggm.simulate.data(100, true.pcor)

# estimate partial correlation matrix (simple estimator)
inferred.pcor <- ggm.estimate.pcor(sim.dat)

# p-values, q-values and posterior probabilities for each edge 
test.results <- ggm.test.edges(inferred.pcor)

# show best 20 edges
test.results[1:20,]

# how many are significant for Q=0.05 ?
num.significant <- sum(test.results$qval <= 0.05)
test.results[1:num.significant,]

# parameters of the mixture distribution used to compute p-values etc.
cor.fit.mixture(sm2vec(inferred.pcor))
}
\keyword{htest}
