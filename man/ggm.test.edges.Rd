\name{ggm.test.edges}
\alias{ggm.test.edges}

\title{Graphical Gaussian Models: Testing Edges}

\usage{
ggm.test.edges(r.mat, MAXKAPPA=5000, kappa=NULL, eta0=NULL,
fA.type=c("nonparametric", "uniform"), df=7, plot.locfdr=1)
}

\arguments{
  \item{r.mat}{matrix of partial correlations}
  \item{kappa}{the degree of freedom of the null distribution (will be estimated if left unspecified)}
  \item{eta0}{the proportion of true null values (will be estimated if left unspecified)}
  \item{fA.type}{assumed type of alternative distribution - see also \code{\link{cor.fit.mixture}}}
  \item{MAXKAPPA}{upper bound for the estimated \code{kappa} - see \code{\link{cor.fit.mixture}} (default: MAXKAPPA=5000)}
  \item{df}{degrees of freedom for the spline fitting the density (only if fA.type="nonparametric")}
  \item{plot.locfdr}{controls plot option in \code{locfdr} }
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
  computed (also using \code{\link{cor.fit.mixture}}.  Finally, to simplify multiple testing q-values
  are computed via  \code{\link{fdr.control}} whith the specified value of \code{eta0} taken
  into account.

  Theoretical details are explained in Schaefer and Strimmer (2005), along with a simulation
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
  Juliane Schaefer (\url{http://www.statistik.lmu.de/~schaefer/}) and
  Korbinian Strimmer (\url{http://www.statistik.lmu.de/~strimmer/}).
}
\references{
  Schaefer, J., and Strimmer, K. (2005).  An empirical Bayes approach to inferring
  large-scale gene association networks. \emph{Bioinformatics} \bold{21}:754-764.
}
\seealso{
\code{\link{cor.fit.mixture}},
\code{\link{cor0.test}},
\code{\link{fdr.control}},
\code{\link{ggm.estimate.pcor}}.
}

\examples{
# load GeneTS library
library("GeneTS")
 
# generate random network with 20 nodes and 5 percent edges
true.pcor <- ggm.simulate.pcor(20, 0.05)

# simulate data set of length 100
sim.dat <- ggm.simulate.data(100, true.pcor)

# estimate partial correlation matrix (simple estimator)
inferred.pcor <- ggm.estimate.pcor(sim.dat)

# p-values, q-values and posterior probabilities for each edge 
#
# try both options for fA!
#test.results <- ggm.test.edges(inferred.pcor, fA.type="nonparametric")
test.results <- ggm.test.edges(inferred.pcor, fA.type="uniform")

# show best 20 edges (strongest correlation)
test.results[1:20,]

# how many are significant based on FDR cutoff Q=0.05 ?
num.significant.1 <- sum(test.results$qval <= 0.05)
test.results[1:num.significant.1,]

# how many are significant based on "local fdr" cutoff (prob > 0.95) ?
num.significant.2 <- sum(test.results$prob > 0.95)
test.results[1:num.significant.2,]

# parameters of the mixture distribution used to compute p-values etc.
c <- cor.fit.mixture(sm2vec(inferred.pcor), fA.type="uniform")
c$eta0
c$kappa
}
\keyword{htest}
