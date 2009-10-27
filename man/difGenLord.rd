\name{difGenLord}
\alias{difGenLord}
\alias{plot.GenLord}
\alias{print.GenLord}

\title{Generalized Lord's chi-square DIF method}

\description{
 Performs DIF detection among multiple groups using generalized Lord's chi-square method. 
 }

\usage{
 difGenLord(Data, group, focal.names, model, c=NULL, engine="ltm", 
 irtParam=NULL, nrFocal=2, same.scale=TRUE, alpha=0.05, 
 purify=FALSE, nrIter=10)
 \method{print}{GenLord}(x, ...)
 \method{plot}{GenLord}(x, pch=8, number=TRUE, col="red", ...)
 }

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{Data}) of group membership. See \bold{Details}.}
 \item{focal.names}{numeric or character vector indicating the levels of \code{group} which correspond to the focal groups.}
 \item{model}{character: the IRT model to be fitted (either \code{"1PL"}, \code{"2PL"} or \code{"3PL"}).}
 \item{c}{optional numeric value or vector giving the values of the constrained pseudo-guessing parameters. See \bold{Details}.}
 \item{engine}{character: the engine for estimating the 1PL model, either \code{"ltm"} (default) or \code{"lme4"}.}
 \item{irtParam}{matrix with \emph{2J} rows (where \emph{J} is the number of items) and at most 9 columns containing item parameters estimates. See \bold{Details}.}
 \item{nrFocal}{numeric: the number of focal groups (default is 2).}
 \item{same.scale}{logical: are the item parameters of the \code{irtParam} matrix on the same scale? (default is \code{TRUE}). See \bold{Details}.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.} 
 \item{x}{the result from a \code{GenLord} class object.}
 \item{pch, col}{type of usual \code{pch} and \code{col} graphical options.}
 \item{number}{logical: should the item number identification be printed (default is \code{TRUE}).}
 \item{...}{other generic parameters for the \code{plot} or the \code{print} functions.}
 }

\value{
A list of class "GenLord" with the following arguments:
  \item{genLordChi}{the values of the generalized Lord's chi-square statistics.}
  \item{alpha}{the value of \code{alpha} argument.}
  \item{thr}{the threshold (cut-score) for DIF detection.}
  \item{df}{the degrees of freedom of the asymptotic null distribution of the statistics.}
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
 The generalized Lord's chi-square method (Kim, Cohen and Park, 1995), also referred to as \emph{Qj} statistic, allows for detecting uniform or non-uniform
 differential item functioning among multiple groups by setting an appropriate item response model. The input can be of two kinds: either by displaying 
 the full data, the group membership, the focal groups and the model, or by giving the item parameter estimates (with the option \code{irtParam}).
 Both can be supplied, but in this case only the parameters in \code{irtParam} are used for computing generalized Lord's chi-square statistic.

 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which 
 corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise, \code{group} must 
 be a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership must hold only at least three different values, either as numeric or character. The focal groups are defined by
 the values of the argument \code{focal.names}. 
 
 If the model is not the 1PL model, or if \code{engine} is equal to \code{"ltm"}, the selected IRT model is fitted using marginal maximum likelihood
 by means of the functions from the \code{ltm} package (Rizopoulos, 2006). Otherwise, the 1PL model is fitted as a generalized 
 linear mixed model, by means of the \code{glmer} function of the \code{lme4} package (Bates and Maechler, 2009).
 The 3PL model can be fitted either unconstrained (by setting \code{c} to \code{NULL}) or by fixing the pseudo-guessing values. In the latter 
 case, the argument \code{c} is either a numeric vector of same length of the number of items, with one value per item pseudo-guessing parameter, 
 or a single value which is duplicated for all the items. If \code{c} is different from \code{NULL} then the 3PL model is always fitted (whatever the value of \code{model}).

 The \code{irtParam} matrix has a number of rows equal to the number of groups (reference and focal ones) times the number of items \emph{J}. The first \emph{J} 
 rows refer to the item parameter estimates in the reference group, while the next sets of \emph{J} rows correspond to the same items in each of 
 the focal groups. The number of columns depends on the selected IRT model: 2 for the 1PL model, 5 for the 2PL model, 6 for the constrained 3PL model
 and 9 for the unconstrained 3PL model. The columns of \code{irtParam} have to follow the same structure as the output of
 \code{itemParEst} command (the latter can actually be used to create the \code{irtParam} matrix). The number of focal groups has to be specified with
 argument \code{nrFocal} (default value is 2).

 In addition to the matrix of parameter estimates, one has to specify whether items in the focal groups were rescaled to those of the 
 reference group. If not, rescaling is performed by equal means anchoring (Cook and Eignor, 1991). Argument \code{same.scale} is used for 
 this choice (default option is \code{TRUE} and assumes therefore that the parameters are already placed on a same scale). 

 The threshold (or cut-score) for classifying items as DIF is computed as the quantile of the chi-square distribution with lower-tail
 probability of one minus \code{alpha} and \emph{p} degrees of freedom. The value of \emph{p} is the product of the number of focal groups by the number
 of item parameters to be tested (1 for the 1PL model, 2 for the 2PL model or the constrained 3PL model, and 3 for the unconstrained 3PL model).
 
 Item purification can be performed by setting \code{purify} to \code{TRUE}. In this case, the purification occurs in the equal means anchoring process: items 
 detected as DIF are iteratively removed from the set of items used for equal means anchoring, and the procedure is repeated until either the same items
 are identified twice as functioning differently, or when \code{nrIter} iterations have been performed. In the latter case a warning message is printed.
 See Candell and Drasgow (1988) for further details.
}


\references{
 Bates, D. and Maechler, M. (2009). lme4: Linear mixed-effects models using S4 classes. R package version 0.999375-31. http://CRAN.R-project.org/package=lme4

 Candell, G.L. and Drasgow, F. (1988). An iterative procedure for linking metrics and assessing item bias in item response theory. 
 \emph{Applied Psychological Measurement, 12}, 253-260. 

 Cook, L. L. and Eignor, D. R. (1991). An NCME instructional module on IRT equating methods. \emph{Educational Measurement: Issues and Practice, 10}, 37-45.
 
 Kim, S.-H., Cohen, A.S. and Park, T.-H. (1995). Detection of differential item functioning in multiple groups. \emph{Journal of Educational Measurement, 32}, 261-276. 

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
    \email{David.Magis@psy.kuleuven.be}, \url{http://ppw.kuleuven.be/okp/home/} \cr
    Gilles Raiche \cr
    Centre sur les Applications des Modeles de Reponses aux Items (CAMRI) \cr
    Universite du Quebec a Montreal \cr
    \email{raiche.gilles@uqam.ca}, \url{http://www.er.uqam.ca/nobel/r17165/} \cr 
    }

\seealso{
 \code{\link{itemParEst}}
}

\examples{
# Loading of the verbal data
data(verbal)
attach(verbal)

# Creating four groups according to gender ("Man" or "Woman") and trait 
# anger score ("Low" or "High")
group<-rep("WomanLow",nrow(verbal))
group[Anger>20 & Gender==0]<-"WomanHigh"
group[Anger<=20 & Gender==1]<-"ManLow"
group[Anger>20 & Gender==1]<-"ManHigh"

# New data set
Verbal<-cbind(verbal[,1:24],group)

# Reference group: "WomanLow"
names<-c("WomanHigh","ManLow","ManHigh")

# Three equivalent settings of the data matrix and the group membership
# 1PL model, "ltm" engine (remove #)

# difGenLord(Verbal, group=25, focal.names=names, model="1PL")
# difGenLord(Verbal, group="group", focal.name=names, model="1PL")
difGenLord(Verbal[,1:24], group=Verbal[,25], focal.names=names, 
model="1PL")

# 1PL model, "lme4" engine (remove #)

# difGenLord(Verbal, group="group", focal.name=names, model="1PL", 
# engine="lme4")

# With item purification (remove #)

# difGenLord(Verbal, group=25, focal.names=names, model="1PL", purify=TRUE)

# Splitting the data into the four subsets according to "group"
data0<-data1<-data2<-data3<-NULL
for (i in 1:nrow(verbal)){
if (group[i]=="WomanLow") data0<-rbind(data0,as.numeric(verbal[i,1:24]))
if (group[i]=="WomanHigh") data1<-rbind(data1,as.numeric(verbal[i,1:24]))
if (group[i]=="ManLow") data2<-rbind(data2,as.numeric(verbal[i,1:24]))
if (group[i]=="ManHigh") data3<-rbind(data3,as.numeric(verbal[i,1:24]))
}

# Estimation of the item parameters (1PL model)
m0.1PL<-itemParEst(data0, model="1PL")
m1.1PL<-itemParEst(data1, model="1PL")
m2.1PL<-itemParEst(data2, model="1PL")
m3.1PL<-itemParEst(data3, model="1PL")

# Merging the item parameters WITHOUT rescaling
irt.noscale<-rbind(m0.1PL,m1.1PL,m2.1PL,m3.1PL)
rownames(irt.noscale)<-rep(colnames(verbal[,1:24]),4)

# merging the item parameters WITH rescaling
irt.scale<-rbind(m0.1PL, itemRescale(m0.1PL,m1.1PL),
itemRescale(m0.1PL,m2.1PL) ,itemRescale(m0.1PL,m3.1PL))
rownames(irt.scale)<-rep(colnames(verbal[,1:24]),4)

# Equivalent calculations
difGenLord(irtParam=irt.noscale, nrFocal=3, same.scale=FALSE)
difGenLord(irtParam=irt.scale, nrFocal=3, same.scale=TRUE)

# With item purification
difGenLord(irtParam=irt.noscale, nrFocal=3, same.scale=FALSE, purify=TRUE)
}
