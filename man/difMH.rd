\name{difMH}
\alias{difMH}
\alias{print.MH}
\alias{plot.MH}

\title{Mantel-Haenszel DIF method}

\description{
 Performs DIF detection using Mantel-Haenszel method. 
 }

\usage{
 difMH(Data, group, focal.name , MHstat="MHChisq",
 	correct=TRUE, alpha=0.05, purify=FALSE, nrIter=10)
 \method{print}{MH}(x, ...)
 \method{plot}{MH}(x, pch=8, number=TRUE, col="red", ...)
 }

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{data}) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{MHstat}{character: specifies the DIF statistic to be used for DIF identification. Possible values are \code{"MHChisq"} (default) and \code{"logOR"}. See \bold{Details }.}
 \item{correct}{logical: should the continuity correction be used? (default is \code{TRUE})}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.}
 \item{x}{the result from a \code{MH} class object.}
 \item{pch, col}{type of usual \code{pch} and \code{col} graphical options.}
 \item{number}{logical: should the item number identification be printed (default is \code{TRUE}).}
 \item{...}{other generic parameters for the \code{plot} or the \code{print} functions.}
 }

\value{
A list of class "MH" with the following arguments:
  \item{MH}{the values of the Mantel-Haenszel DIF statistics.}
  \item{alphaMH}{the values of the mantel-Haenszel estimates of common odds ratios.}
  \item{varLambda}{the values of the variances of the log odds-ratio statistics.}
  \item{MHstat}{the value of the \code{MHstat} argument.}
  \item{alpha}{the value of \code{alpha} argument.}
  \item{thr}{the threshold (cut-score) for DIF detection.}
  \item{DIFitems}{either the column indicators of the items which were detected as DIF items, or "No DIF item detected".}
  \item{correct}{the value of \code{correct} option.}
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
 The method of Mantel-Haenszel (1959) allows for detecting uniform differential item functioning 
 without requiring an item response model approach. 
 
 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which 
 corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise,
 \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership must hold only two different values, either as numeric or character. The focal group is defined by
 the value of the argument \code{focal.name}. 
 
 The DIF statistic is specified by the \code{MHstat} argument. By default, \code{MHstat} takes the value \code{"MHChisq"} and the Mantel-Haenszel chi-square
 statistic is used. The other optional value is \code{"logOR"}, and the log odds-ratio statistic (that is, the log of \code{alphaMH} divided by the square root
 of \code{varLambda}) is used. See Penfield and Camilli (2007), Philips and Holland (1987) and \code{\link{mantelHaenszel}} help file.
 
 The threshold (or cut-score) for classifying items as DIF depends on the DIF statistic. With the Mantel-Haenszel chi-square statistic (\code{MHstat=="MHChisq"}),
 it is computed as the quantile of the chi-square distribution with lower-tail probability of one minus \code{alpha} and with one degree of freedom. With 
 the log odds-ratio statistic (\code{MHstat=="logOR"}), it is computed as the quantile of the standard normal distribution with lower-tail probability of
 one minus \code{alpha} over two. 
 
 By default, the continuity correction factor -0.5 is used (Holland and Thayer, 1988). One can nevertheless remove it by specifying \code{correct=FALSE}.
 
 In addition, the Mantel-Haenszel estimates of the common odds ratios \eqn{\alpha_{MH}} are used to measure the effect sizes of the items. These are obtained by
 \eqn{\Delta_{MH} = -2.35 \log \alpha_{MH}} (Holland and Thayer, 1985). According to the ETS delta scale, the effect size of an item is classified as negligible
 if \eqn{|\Delta_{MH}| \leq 1}, moderate  if \eqn{1 \leq |\Delta_{MH}| \leq 1.5}, and large if \eqn{|\Delta_{MH}| \geq 1.5}. The values of the effect sizes, together with
 the ETS classsification, are printed with the output.

 Item purification can be performed by setting \code{purify} to \code{TRUE}. Purification works as follows: if at least one item was detected as functioning 
 differently at some step of the process, then the data set of the next step consists in all items that are currently anchor (DIF free) items, plus the 
 tested item (if necessary). The process stops when either two successive applications of the method yield the same classifications of the items (Clauser and Mazor, 1998),
 or when \code{nrIter} iterations are run without obtaining two successive identical classifications. In the latter case a warning message is printed. 
}


\references{
 Holland, P. W. and Thayer, D. T. (1985). An alternative definition of the ETS delta scale of item difficulty. \emph{Research Report RR-85-43}. Princeton, New-Jersey:
 Educational Testing Service.

 Holland, P. W. and Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Ed.), \emph{Test validity}. Hillsdale, New Jersey: Lawrence Erlbaum Associates.
 
 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (in press). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods}.

 Mantel, N. and Haenszel, W. (1959). Statistical aspects of the analysis of data from retrospective studies of disease. \emph{Journal of the National Cancer Institute, 22}, 719-748.
 
 Penfield, R. D., and Camilli, G. (2007). Differential item functioning and item bias. In C. R. Rao and S. Sinharray (Eds.), \emph{Handbook of Statistics 26: Psychometrics}
 (pp. 125-167). Amsterdam, The Netherlands: Elsevier.

 Philips, A., and Holland, P. W. (1987). Estimators of the Mantel-Haenszel log odds-ratio estimate. \emph{Biometrics, 43}, 425-431.

 Raju, N. S., Bode, R. K. and Larsen, V. S. (1989). An empirical assessment of the Mantel-Haenszel statistic to detect differential item functioning. \emph{Applied Measurement in Education, 2}, 1-13.
 
 Uttaro, T. and Millsap, R. E. (1994). Factors influencing the Mantel-Haenszel procedure in the detection of differential item functioning. \emph{Applied Psychological Measurement, 18}, 15-25.
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
 \code{\link{mantelHaenszel}}, \code{\link{dichoDif}}
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)

 # Excluding the "Anger" variable
 verbal <- verbal[colnames(verbal)!="Anger"]

 # Three equivalent settings of the data matrix and the group membership
 r <- difMH(verbal, group=25, focal.name=1)
 difMH(verbal, group="Gender", focal.name=1)
 difMH(verbal[,1:24], group=verbal[,25], focal.name=1)

 # With log odds-ratio statistic
 r2 <- difMH(verbal, group=25, focal.name=1, MHstat = "logOR")

 # With item purification
 difMH(verbal, group="Gender", focal.name=1, purify=TRUE)
 difMH(verbal, group="Gender", focal.name=1, purify=TRUE, nrIter=5)

 # Without continuity correction and with 0.01 significance level
 difMH(verbal, group="Gender", focal.name=1, alpha=0.01, correct=FALSE)

 # Plotting results
 plot(r)
 plot(r2)
}
 }



