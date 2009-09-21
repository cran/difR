\name{difLogistic}
\alias{difLogistic}
\alias{print.Logistic}
\alias{plot.Logistic}

\title{Logistic regression DIF method}

\description{
  Performs DIF detection using logistic regression method.
 }

\usage{
 difLogistic(Data, group, focal.name, alpha=0.05, purify=FALSE, 
 nrIter=10)
 \method{print}{Logistic}(x, ...)
 \method{plot}{Logistic}(x, pch=8, number=TRUE, col="red", ...)
 }
 
\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{data}) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.}
 \item{x}{the result from a \code{Logistik} class object.}
 \item{pch, col}{type of usual \code{pch} and \code{col} graphical options.}
 \item{number}{logical: should the item number identification be printed (default is \code{TRUE}).}
 \item{...}{other generic parameters for the \code{plot} or the \code{print} functions.}
}


\value{
A list of class "Logistik" with the following arguments:
  \item{Logistik}{the values of the logistic regression statistics.}
  \item{alpha}{the value of \code{alpha} argument.}
  \item{thr}{the threshold (cut-score) for DIF detection.}
  \item{DIFitems}{either the column indicators for the items which were detected as DIF items, or "No DIF item detected".}
  \item{purification}{the value of \code{purify} option.} 
  \item{nrPur}{the number of iterations in the item purification process. Returned only if \code{purify} is \code{TRUE}.}
  \item{difPur}{a binary matrix with one row per iteration in the item purification process and one column per item. Zeros and ones in the \emph{i}-th 
  row refer to items which were classified respectively as non-DIF and DIF items at the (\emph{i}-1)-th step. The first row corresponds to the initial
  classification of the items. Returned only if \code{purify} is \code{TRUE}.}
  \item{convergence}{logical indicating whether the iterative item purification process stopped before the maximal number of \code{nrItem} allowed iterations. 
  Returned only if \code{purify} is \code{TRUE}.}
  \item{names}{the names of the items.}
 }


\details{
 The logistic regression method (Swaminathan and Rogers, 1990) allows for detecting both uniform and non-uniform differential item functioning 
 without requiring an item response model approach. It consists in fitting a logistic model with the test score,
 the group membership and an interaction between both as covariates. The statistical significance of the parameters
 related to group membership and the group-score interaction is then evaluated by means of the usual likelihood-ratio
 test.
 
 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which 
 corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise,
 \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership must hold only two different values, either as numeric or character. The focal group is defined by
 the value of the argument \code{focal.name}. 
 
 The threshold (or cut-score) for classifying items as DIF is computed as the quantile of the chi-square distribution with lower-tail
 probability of one minus \code{alpha} and with two degrees of freedom.
 
 Item purification can be performed by setting \code{purify} to \code{TRUE}. Purification works as follows: if at least one item is detected as functioning 
 differently at the first step of the process, then the data set of the next step consists in all items that are currently anchor (DIF free) items, plus the 
 tested item (if necessary). The process stops when either two successive applications of the method yield the same classifications of the items (Clauser and Mazor, 1998),
 or when \code{nrIter} iterations are run without obtaining two successive identical classifications. In the latter case a warning message is printed. 
}

\references{
 Clauser, B.E. and Mazor, K.M. (1998). Using statistical procedures to identify differential item functioning test items. \emph{Educational Measurement: Issues and Practice, 17}, 31-44. 

 Finch, W.H. and French, B. (2007). Detection of crossing differential item functioning: a comparison of four methods. \emph{Educational and Psychological Measurement, 67}, 565-582. 
 
 Hidalgo, M. D. and Lopez-Pina, J.A. (2004). Differential item functioning detection and effect size: a comparison between logistic regression and Mantel-Haenszel procedures. \emph{Educational and Psychological Measurement, 64}, 903-915. 
 
 Swaminathan, H. and Rogers, H. J. (1990). Detecting differential item functioning using logistic regression procedures. \emph{Journal of Educational Measurement, 27}, 361-370.
 
 Zumbo, B.D. (1999). \emph{A handbook on the theory and methods of differential item functioning (DIF): logistic regression modelling as a unitary framework for binary and Likert-type (ordinal) item scores}. Ottawa, ON: Directorate of Human Resources Research and Evaluation, Department of National Defense. 
 }
 
\author{
    Sebastien Beland \cr
    Centre sur les Applications des Modeles de Reponses aux Items (CAMRI) \cr
    Universite du Quebec a Montreal \cr
    \email{sebastien.beland.1@hotmail.com} \cr
    David Magis \cr
    Research Group of Quantitative Psychology and Individual Differences \cr
    Katholieke Universiteit Leuven \cr
    \email{David.Magis@psy.kuleuven.be}, \url{http://ppw.kuleuven.be/okp/home/}
    Gilles Raiche \cr
    Centre sur les Applications des Modeles de Reponses aux Items (CAMRI) \cr
    Universite du Quebec a Montreal \cr
    \email{raiche.gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/} \cr 
    }

\seealso{
 \code{\link{Logistik}}, \code{\link{dichoDif}}
}

\examples{
# Loading of the verbal data
data(verbal)

# Excluding the "Anger" variable
verbal<-verbal[colnames(verbal)!="Anger"]

# Three equivalent settings of the data matrix and the group membership
difLogistic(verbal, group=25, focal.name=1)
difLogistic(verbal, group="Gender", focal.name=1)
difLogistic(verbal[,1:24], group=verbal[,25], focal.name=1)

# With item purification
difLogistic(verbal, group="Gender", focal.name=1, purify=TRUE)
difLogistic(verbal, group="Gender", focal.name=1, purify=TRUE, nrIter=5)
}
