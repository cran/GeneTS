### ggm.test.edges  (2004-01-15)
###
###   Compute p-values, q-values and posterior probabilities for GGM edges
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


##############################


# assign p-values, q-values and posterior probabilities to each edge
ggm.test.edges <- function(r.mat)
{
   pcor <- sm2vec(r.mat)
   indexes <- sm.indexes(r.mat)
   colnames(indexes) <- c("node1", "node2")

   mfit <- cor.fit.mixture(pcor)

   pval <- cor0.test(pcor, mfit$kappa)
   fdr.out <- fdr.control(pval)
   qval <- fdr.out$qvalues
   prob <- cor.prob.nonzero(pcor, mfit$kappa, mfit$eta0)

   result <- cbind(pcor, indexes, pval, qval, prob)

   # sort according to magnitude of correlation 
   sort.idx <- order(-abs(result[,1]))
   result <- result[sort.idx,]
   
   # return as data frame
   return(as.data.frame(result))
}


