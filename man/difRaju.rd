\name{difRaju}
\alias{difRaju}
\alias{plot.Raj}
\alias{print.Raj}

\title{Raju's area DIF method}

\description{
  Performs DIF detection using Raju's area method. 
 }

\usage{
 difRaju(Data, group, focal.name, model, c=NULL, engine="ltm", 
 irtParam=NULL,  same.scale=TRUE, alpha=0.05, purify=FALSE, 
 nrIter=10)
 \method{print}{Raj}(x, ...)
 \method{plot}{Raj}(x, pch=8, number=TRUE, col="red", ...)
 }

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{data}) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{model}{character: the IRT model to be fitted (either \code{"1PL"}, \code{"2PL"} or \code{"3PL"}).}
 \item{c}{optional numeric value or vector giving the values of the constrained pseudo-guessing parameters. See \bold{Details}.}
 \item{engine}{character: the engine for estimating the 1PL model, either \code{"ltm"} (default) or \code{"lme4"}.}
 \item{irtParam}{matrix with \emph{2J} rows (where \emph{J} is the number of items) and at most 9 columns containing item parameters estimates. See \bold{Details}.}
 \item{same.scale}{logical: are the item parameters of the \code{irtParam} matrix on the same scale? (default is "TRUE"). See \bold{Details}.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.} 
 \item{x}{the result from a \code{Raj} class object.}
 \item{pch, col}{type of usual \code{pch} and \code{col} graphical options.}
 \item{number}{logical: should the item number identification be printed (default is \code{TRUE}).}
 \item{...}{other generic parameters for the \code{plot} or the \code{print} functions.}
 }

\value{
A list of class "Raj" with the following arguments:
  \item{RajuZ}{the values of the Raju's statistics.}
  \item{alpha}{the value of \code{alpha} argument.}
  \item{thr}{the threshold (cut-score) for DIF detection.}
  \item{DIFitems}{either the column indicators of the items which were detected as DIF items, or "No DIF item detected".}
  \item{purification}{the value of \code{purify} option.} 
  \item{nrPur}{the number of iterations in the item purification process. Returned only if \code{purify} is \code{TRUE}.}
  \item{difPur}{a binary matrix with one row per iteration in the item purification process and one column per item. Zeros and ones in the \emph{i}-th 
   row refer to items which were classified respectively as non-DIF and DIF items at the (\emph{i}-1)-th step. The first row corresponds to the initial
   classification of the items. Returned only if \code{purify} is \code{TRUE}.}
  \item{convergence}{logical indicating whether the iterative item purification process stopped before the maximal number \code{nrIter}of allowed iterations. 
  Returned only if \code{purify} is \code{TRUE}.}
  \item{model}{the value of \code{model} argument.}
  \item{c}{The value of the \code{c} argument.}
  \item{engine}{The value of the \code{engine} argument.}
  \item{itemParInit}{the matrix of initial parameter estimates,with the same format as \code{irtParam} either provided by the user (through \code{irtParam}) or estimated from the data
   (and displayed without rescaling).}
  \item{itemParFinal}{the matrix of final parameter estimates, with the same format as \code{irtParam}, obtained after item purification. Returned 
   only if \code{purify} is \code{TRUE}.}
  \item{estPar}{a logical value indicating whether the item parameters were estimated (\code{TRUE}) or provided by the user (\code{FALSE}).}
  \item{names}{the names of the items.}
}
 
\details{
 Raju's area method (Raju, 1988, 1990) allows for detecting uniform or non-uniform differential item functioning 
 by setting an appropriate item response model. The input can be of two kinds: either by displaying the full data,
 the group membership and the model, or by giving the item parameter estimates (with the option \code{irtParam}).
 Both can be supplied, but in this case only the parameters in \code{irtParam} are used for computing Raju's statistic.

 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which 
 corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise, \code{group} must 
 be a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership�must hold only two different values, either as numeric or character. The focal group is defined by
 the value of the argument \code{focal.name}. 
 
 If the model is not the 1PL model, or if \code{engine} is equal to \code{"ltm"}, the selected IRT model is fitted using marginal maximum likelihood
 by means of the functions from the \code{ltm} package (Rizopoulos, 2006). Otherwise, the 1PL model is fitted as a generalized 
 linear mixed model, by means of the \code{glmer} function of the \code{lme4} package (Bates and Maechler, 2009).
 The 3PL model can be fitted either unconstrained (by setting \code{c} to \code{NULL}) or by fixing the pseudo-guessing values. In the latter 
 case, the argument \code{c} holds either a numeric vector of same length of the number of items, with one value per item pseudo-guessing parameter, 
 or a single value which is duplicated for all the items. If \code{c} is different from \code{NULL} then the 3PL model is always fitted (whatever the value of \code{model}).

 The \code{irtParam} matrix has a number of rows equal to twice the number of items in the data set. The first \emph{J} rows refer to 
 the item parameter estimates in the reference group, while the last \emph{J} ones correspond to the same items in the focal group. 
 The number of columns depends on the selected IRT model: 2 for the 1PL model, 5 for the 2PL model, 6 for the constrained 3PL model
 and 9 for the unconstrained 3PL model. The columns of \code{irtParam} have to follow the same structure as the output of
 \code{itemParEst} command (the latter can actually be used to create the \code{irtParam} matrix).

 In addition to the matrix of parameter estimates, one has to specify whether items in the focal group were rescaled to those of the 
 reference group. If not, rescaling is performed by equal means anchoring (Cook and Eignor, 1991). Argument \code{same.scale} is used for 
 this choice (default option is \code{TRUE} and assumes therefore that the parameters are already placed on the same scale).  

 The threshold (or cut-score) for classifying items as DIF is computed as the quantile of the normal distribution with lower-tail
 probability of one minus \code{alpha}/2.
 
 Item purification can be performed by setting \code{purify} to \code{TRUE}. In this case, the purification occurs in the equal means anchoring process. Items 
 detected as DIF are iteratively removed from the set of items used for equal means anchoring, and the procedure is repeated until either the same items
 are identified twice as functioning differently, or when \code{nrIter} iterations have been performed. In the latter case a warning message is printed.
 See Candell and Drasgow (1988) for further details.
}


\references{
 Bates, D. and Maechler, M. (2009). lme4: Linear mixed-effects models using S4 classes. R package version 0.999375-31. http://CRAN.R-project.org/package=lme4

 Candell, G.L. and Drasgow, F. (1988). An iterative procedure for linking metrics and assessing item bias in item response theory. 
 \emph{Applied Psychological Measurement, 12}, 253-260. 

 Cook, L. L. and Eignor, D. R. (1991). An NCME instructional module on IRT equating methods. \emph{Educational Measurement: Issues and Practice, 10}, 37-45.
 
 Raju, N.S. (1988). The area between two item characteristic curves. \emph{Psychometrika, 53}, 495-502. 

 Raju, N. S. (1990). Determining the significance of estimated signed and unsigned areas between two item response functions. \emph{Applied Psychological Measurement, 14}, 197-207.
 
 Rizopoulos, D. (2006). ltm: An R package for latent variable modelling and item response theory analyses. \emph{Journal of Statistical Software, 17}, 1-25. URL: http://www.jstatsoft.org/v17/i05/
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
 \code{\link{RajuZ}}, \code{\link{itemParEst}}, \code{\link{dichoDif}}
 }

\examples{
# Loading of the verbal data
data(verbal)
attach(verbal)

# Excluding the "Anger" variable
verbal<-verbal[colnames(verbal)!="Anger"]

# Three equivalent settings of the data matrix and the group membership 
# (1PL model, "ltm" engine)
difRaju(verbal, group=25, focal.name=1, model="1PL")
difRaju(verbal, group="Gender", focal.name=1, model="1PL")
difRaju(verbal[,1:24], group=verbal[,25], focal.name=1, model="1PL")

# (1PL model, "lme4" engine)
difRaju(verbal, group="Gender", focal.name=1, model="1PL", 
engine="lme4")

# 2PL model
difRaju(verbal, group="Gender", focal.name=1, model="2PL")

# 3PL model with all pseudo-guessing parameters constrained to 0.05
difRaju(verbal, group="Gender", focal.name=1, model="3PL", c=0.05)

# Same models, with item purification
difRaju(verbal, group="Gender", focal.name=1, model="1PL", purify=TRUE)
difRaju(verbal, group="Gender", focal.name=1, model="2PL", purify=TRUE)
difRaju(verbal, group="Gender", focal.name=1, model="3PL", c=0.05, 
purify=TRUE)

## Splitting the data into reference and focal groups
nF<-sum(Gender)
nR<-nrow(verbal)-nF
data.ref<-verbal[,1:24][order(Gender),][1:nR,]
data.focal<-verbal[,1:24][order(Gender),][(nR+1):(nR+nF),]

## Pre-estimation of the item parameters (1PL model, "ltm" engine)
item.1PL<-rbind(itemParEst(data.ref,model="1PL"),
itemParEst(data.focal,model="1PL"))
difRaju(irtParam=item.1PL,same.scale=FALSE)

## Pre-estimation of the item parameters (1PL model, "lme4" engine)
item.1PL<-rbind(itemParEst(data.ref, model="1PL", engine="lme4"),
itemParEst(data.focal, model="1PL", engine="lme4"))
difRaju(irtParam=item.1PL, same.scale=FALSE)

## Pre-estimation of the item parameters (2PL model)
item.2PL<-rbind(itemParEst(data.ref, model="2PL"),
itemParEst(data.focal, model="2PL"))
difRaju(irtParam=item.2PL, same.scale=FALSE)

## Pre-estimation of the item parameters (constrained 3PL model)
item.3PL<-rbind(itemParEst(data.ref, model="3PL", c=0.05),
itemParEst(data.focal, model="3PL", c=0.05))
difRaju(irtParam=item.3PL, same.scale=FALSE)
}
