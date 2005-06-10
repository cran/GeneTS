### ggm.test.edges  (2005-01-15)
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
ggm.test.edges <- function(r.mat, MAXKAPPA=5000, kappa=NULL, eta0=NULL, 
       fA.type=c("nonparametric", "uniform"), df=7)
{
   fA.type <- match.arg(fA.type) 

   pcor <- sm2vec(r.mat)
   indexes <- sm.indexes(r.mat)
   colnames(indexes) <- c("node1", "node2")
   
   mfit = NULL
   if (is.null(kappa) || is.null(eta0))
   {
     # estimate kappa and eta0
     mfit <- cor.fit.mixture(pcor, fA.type=fA.type, df=df)
     kappa <-  mfit$kappa
     eta0 <- mfit$eta0  
   }
      
   pval <- cor0.test(pcor, kappa)
   fdr.out <- fdr.control(pval, eta0=eta0)
   qval <- fdr.out$qvalues
   
   if (is.null(mfit))
   {
      prob <- rep(NA, length(pval))
   }
   else
   {
      prob <- mfit$prob.nonzero
   }

   result <- cbind(pcor, indexes, pval, qval, prob)

   # sort according to magnitude of correlation 
   sort.idx <- order(-abs(result[,1]))
   result <- result[sort.idx,]
   
   # return as data frame
   return(as.data.frame(result))
}

