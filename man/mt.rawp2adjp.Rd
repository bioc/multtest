\name{mt.rawp2adjp}
\alias{mt.rawp2adjp}
\title{Adjusted p-values for simple multiple testing procedures}
\description{
  This function computes adjusted \eqn{p}-values for simple
  multiple testing procedures from a vector of raw (unadjusted) \eqn{p}-values. The procedures include the Bonferroni, Holm (1979), Hochberg (1988), and Sidak procedures for strong control of the family-wise Type I error rate (FWER), and the Benjamini & Hochberg (1995) and Benjamini & Yekutieli (2001) procedures for (strong) control of the false discovery rate (FDR).
}
\usage{
mt.rawp2adjp(rawp, proc=c("Bonferroni", "Holm", "Hochberg", "SidakSS", "SidakSD", "BH"))
}
\arguments{
  \item{rawp}{A vector of raw (unadjusted) \eqn{p}-values for each hypothesis under consideration. These could be nominal \eqn{p}-values, for example, from t-tables, or permutation \eqn{p}-values as in given in \code{mt.maxT} and \code{mt.minP}. If the \code{mt.maxT} or \code{mt.minP} functions are used, raw \eqn{p}-values should given in the original data order, \code{rawp[order(index)]}.}
  \item{proc}{A vector of character strings containing the names of the multiple testing procedures for which adjusted \eqn{p}-values are to be computed. This vector should include any of the following: \code{"Bonferroni"}, \code{"Holm"}, \code{"Hochberg"}, \code{"SidakSS"}, \code{"SidakSD"}, \code{"BH"}, \code{"BY"}.
}
}
\details{
  Adjusted \eqn{p}-values are computed for simple FWER and FDR
  controlling procedures based on a vector of raw (unadjusted) \eqn{p}-values.\cr

\code{Bonferroni}: Bonferroni single-step adjusted \eqn{p}-values for strong control of the FWER.\cr
\code{Holm}: Holm (1979) step-down adjusted \eqn{p}-values for strong control of the FWER.\cr
\code{Hochberg}: Hochberg (1988) step-up adjusted \eqn{p}-values for
strong control of the FWER (for raw (unadjusted) \eqn{p}-values satisfying the Simes inequality).\cr
\code{SidakSS}: Sidak single-step adjusted \eqn{p}-values for strong control of the FWER (for positive orthant dependent test statistics).\cr
\code{SidakSD}: Sidak step-down adjusted \eqn{p}-values for strong control of the FWER (for positive orthant dependent test statistics).\cr
\code{BH}: adjusted \eqn{p}-values for the Benjamini & Hochberg (1995) step-up FDR controlling procedure (independent and positive regression dependent test statistics).\cr
\code{BY}: adjusted \eqn{p}-values for the Benjamini & Yekutieli (2001) step-up FDR controlling procedure (general dependency structures).\cr
}

\value{
A list with components
  \item{adjp}{A matrix of adjusted \eqn{p}-values, with rows
    corresponding to hypotheses and columns to multiple testing
    procedures. Hypotheses are sorted in increasing order of their raw (unadjusted) \eqn{p}-values.}
  \item{index}{A vector of row indices, between 1 and \code{length(rawp)}, where rows are sorted according to
    their raw (unadjusted) \eqn{p}-values. To obtain the adjusted \eqn{p}-values in the original data order, use \code{adjp[order(index),]}.
}
}
\references{
Y. Benjamini and Y. Hochberg (1995). Controlling the false discovery rate: a practical and powerful approach to multiple testing. \emph{J. R. Statist. Soc. B}. Vol. 57: 289-300.\cr

Y. Benjamini and D. Yekutieli (2001). The control of the false discovery rate in multiple hypothesis testing under dependency. \emph{Annals of Statistics}. Accepted.\cr

S. Dudoit and J. P. Shaffer. (2001). Multiple hypothesis testing in
microarray experiments. Technical report  *** NUMBER AND LINK ***, Division of Biostatistics, U.C. Berkeley.\cr

Y. Ge, S. Dudoit, and T. P. Speed (2001). Fast algorithm for
resampling-based \eqn{p}-value adjustment in multiple testing. Technical
report *** NUMBER AND LINK ***, Division of Biostatistics, U.C. Berkeley.\cr

Y. Hochberg (1988). A sharper Bonferroni procedure for multiple tests of significance, \emph{Biometrika}. Vol. 75: 800-802.\cr

S. Holm (1979). A simple sequentially rejective multiple test procedure. \emph{Scand. J. Statist.}. Vol. 6: 65-70.
}
 
\author{
  Sandrine Dudoit, \email{sandrine@stat.berkeley.edu},\cr
  Yongchao Ge, \email{gyc@stat.berkeley.edu}. 
}
\seealso{\code{\link{mt.maxT}}, \code{\link{mt.minP}},
  \code{\link{mt.plot}}, \code{\link{mt.reject}}.}

\examples{
library(multtest)

# Gene expression data from Golub et al. (1999)
data(golub)
smallgd<-golub[2:101,-1] # Skip the row labels, consider only the first 100 genes.
classlabel<-golub[1,-1]

# Permutation unadjusted p-values and adjusted p-values for maxT procedure
res1<-mt.maxT(smallgd,classlabel)
rawp<-res1$rawp[order(res1$index)]

# Permutation adjusted p-values for simple multiple testing procedures
procs<-c("Bonferroni","Holm","Hochberg","SidakSS","SidakSD","BH","BY")
res2<-mt.rawp2adjp(rawp,procs)


}

\keyword{htest}





