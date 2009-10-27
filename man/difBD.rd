\name{difBD}
\alias{difBD}
\alias{print.BD}
\alias{plot.BD}

\title{Breslow-Day DIF method}

\description{
  Performs DIF detection using Breslow-Day method. 
 }

\usage{
 difBD(Data, group, focal.name, alpha=0.05, purify=FALSE, 
 nrIter=10)
 \method{print}{BD}(x, ...)
 \method{plot}{BD}(x, pch=8, number=TRUE, col="red", ...)
 }

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{Data}) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is \code{FALSE}).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.}
 \item{x}{the result from a BD class object.}
 \item{pch, col}{type of usual \code{pch} and \code{col} graphical options.}
 \item{number}{logical: should the item number identification be printed (default is \code{TRUE}).}
 \item{...}{other generic parameters for the \code{plot} or the \code{print} functions.}
}
 
\value{
A list of class "BD" with the following arguments:
  \item{BD}{a matrix with one row per item and three columns: the first one contains the Breslow-Day statistic value, the second column indicates 
   the degrees of freedom, and the last column displays the asymptotic \emph{p}-values.}
  \item{alpha}{the significance level for DIF detection.}
  \item{DIFitems}{either the column indicators of the items which were detected as DIF items, or "No DIF item detected".}
  \item{purification}{the value of \code{purify} option.} 
  \item{nrPur}{the number of iterations in the item purification process. Returned only if \code{purify} is \code{TRUE}.}
  \item{difPur}{a binary matrix with one row per iteration in the item purification process and one column per item. Zeros and ones in the \emph{i}-th 
   row refer to items which were classified respectively as non-DIF and DIF items at the (\emph{i}-1)-th step. The first row corresponds to the initial
   classification of the items. Returned only if \code{purify} is \code{TRUE}.}
  \item{convergence}{logical indicating whether the iterative item purification process stopped before the maximal number \code{nrIter} of allowed iterations. 
   Returned only if \code{purify} is \code{TRUE}.}
  \item{names}{the names of the items.}
 }

\details{
 The method of Breslow-Day (1980) allows for detecting non-uniform differential item functioning 
 without requiring an item response model approach.
 
 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which 
 corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise,
 \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership must hold only two different values, either as numeric or character. The focal group is defined by
 the value of the argument \code{focal.name}. 
 
 The threshold (or cut-score) for classifying items as DIF is computed as the quantile of the chi-square distribution with lower-tail
 probability of one minus \code{alpha}, and the degrees of freedom depend on the number of partial tables taken into account 
 (Aguerri \emph{et al.}, in press; Penfield, 2003).
 
 Item purification can be performed by setting \code{purify} to \code{TRUE}. Purification works as follows: if at least one item was detected as functioning 
 differently at the first step of the process, then the data set of the next step consists in all items that are currently anchor (DIF free) items, plus the 
 tested item (if necessary). The process stops when either two successive applications of the method yield the same classifications of the items (Clauser and Mazor, 1998),
 or when \code{nrIter} iterations are run without obtaining two successive identical classifications. In the latter case a warning message is printed. 
}

\references{
 Aguerri, M.E., Galibert, M.S., Attorresi, H.F. and Maranon, P.P. (in press). Erroneous detection of nonuniform DIF using the Breslow-Day test in a short test. \emph{Quality and Quantity}. 

 Breslow, N.E. and Day, N.E. (1980). \emph{Statistical methods in cancer research, vol. I: The analysis of case-control studies}. Scientific Publication N° 32. International Agency for Research on Cancer, Lyon.

 Clauser, B.E. and Mazor, K.M. (1998). Using statistical procedures to identify differential item functioning test items. \emph{Educational Measurement: Issues and Practice, 17}, 31-44. 

 Penfield, R.D. (2003). Application of the Breslow-Day test of trend in odds ratio heterogeneity to the detection of nonuniform DIF. \emph{Alberta Journal of Educational Research, 49}, 231-243.
}

\author{
    Sebastien Beland \cr
    Centre sur les Applications des Modeles de Reponses aux Items (CAMRI) \cr
    Universite du Quebec a Montreal \cr
    \email{sebastien.beland.1@hotmail.com} \cr
    David Magis \cr
    Research Group of Quantitative Psychology and Individual Differences \cr
    Katholieke Universiteit Leuven \cr
    \email{David.Magis@psy.kuleuven.be}, \url{http://ppw.kuleuven.be/okp/home/} \cr
    Gilles Raiche \cr
    Centre sur les Applications des Modeles de Reponses aux Items (CAMRI) \cr
    Universite du Quebec a Montreal \cr
    \email{raiche.gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/} \cr 
 }

\seealso{
 \code{\link{breslowDay}}, \code{\link{dichoDif}}
 }

\examples{

# Loading of the verbal data
data(verbal)

# Excluding the "Anger" variable
verbal<-verbal[colnames(verbal)!="Anger"]

# Three equivalent settings of the data matrix and the group membership
difBD(verbal, group=25, focal.name=1)
difBD(verbal, group="Gender", focal.name=1)
difBD(verbal[,1:24], group=verbal[,25], focal.name=1)

# With item purification (remove #)

# difBD(verbal, group="Gender", focal.name=1, purify=TRUE)
# difBD(verbal, group="Gender", focal.name=1, purify=TRUE, nrIter=5)
}
