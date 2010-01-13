\name{dichoDif}
\alias{dichoDif}
\alias{print.dichoDif}

\title{Comparison of DIF detection methods}

\description{
 This function compares the specified DIF detection methods with respect to the detected items. 
 }

\usage{
 dichoDif(Data, group, focal.name, method, alpha=0.05, 
 correct=TRUE, thr=0.1, type="both", model="2PL", c=NULL,
 engine="ltm", irtParam=NULL, same.scale=TRUE,
 purify=FALSE, nrIter=10)
 \method{print}{dichoDif}(x, ...)
 }
 
\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within \code{Data}) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{method}{character vector specifying the different methods to be compared. See \bold{Details}.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{correct}{logical: should the Mantel-Haenszel continuity correction be used? (default is TRUE).}
 \item{thr}{numeric: the threshold (cut-score) for standardized P-DIF statistic (default is 0.10).}
 \item{type}{a character string specifying which DIF effects must be tested (default is \code{"both"}). See \bold{Details}.}
 \item{model}{character: the IRT model to be fitted (either \code{"1PL"}, \code{"2PL"} or \code{"3PL"}). Default is \code{"2PL"}.}
 \item{c}{optional numeric value or vector giving the values of the constrained pseudo-guessing parameters. See \bold{Details}.}
 \item{engine}{character: the engine for estimating the 1PL model, either \code{"ltm"} (default) or \code{"lme4"}.}
 \item{irtParam}{matrix with \emph{2J} rows (where \emph{J} is the number of items) and at most 9 columns containing item parameters estimates. See \bold{Details}.}
 \item{same.scale}{logical: are the item parameters of the \code{irtParam} matrix on the same scale? (default is "TRUE"). See \bold{Details}.}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.}
 \item{x}{result from a \code{dichoDif} class object.}
 \item{...}{other generic parameters for the \code{print} function.}
}

\value{
Either the output of one of the DIF detection methods, or a list of class "dichoDif" with the following arguments:
  \item{DIF}{a character matrix with one row per item and whose columns refer to the different specified detection methods. See \bold{Details}.}
  \item{correct}{the value of \code{correct} option.}
  \item{alpha}{the significance level \code{alpha}.}
  \item{thr}{the value of \code{thr} option.}
  \item{model}{the value of \code{model} option.}
  \item{c}{the value of \code{c} option.}
  \item{irtParam}{the value of \code{irtParam} option.}
  \item{same.scale}{the value of \code{same.scale} option.}
  \item{purification}{the value of \code{purify} option.} 
  \item{nrPur}{an integer vector (of length equal to the number of methods) with the number of iterations in the purification process. 
   Returned only if \code{purify} is TRUE.}
  \item{convergence}{a logical vector (of length equal to the number of methods) indicating whether the iterative purification process converged. 
   Returned only if \code{purify} is TRUE.}
 }

\details{
 \code{dichoDif} is a generic function which calls one or several DIF detection methods and summarize their output. The possible methods are: \code{"MH"} for mantel-Haenszel 
 (Holland and Thayer, 1988), \code{"Std"} for standardization (Dorans and Kullick, 1986), \code{"Logistic"} for logistic regression (Swaminathan and Rogers, 1990), 
 \code{"BD"} for Breslow-Day method (Penfield, 2003), \code{"Lord"} for Lord's chi-square test (Lord, 1980), \code{"Raju"} for Raju's area method (Raju, 1990), 
 and \code{"LRT"} for likelihood-ratio test method (Thissen, Steinberg and Wainer, 1988).

 If \code{method} has a single component, the output of \code{dichoDif} is exactly the one provided by the method itself. Otherwise, the main  output is a matrix with one row 
 per item and one column per method. For each specified method and related arguments, items detected as DIF and non-DIF are respectively encoded as \code{"DIF"} and
 \code{"NoDIF"}. When printing the output an additional column is added, counting the number of times each item was detected as functioning differently (Note: this is just an
 informative summary, since the methods are obviously not independent for the detection of DIF items).

 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.In addition, \code{Data} can hold the vector of group 
 membership. If so, \code{group} indicates the column of \code{Data} which corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise,
 \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership must hold only two different values, either as numeric or character. The focal group is defined by the value of the argument 
 \code{focal.name}. 

 For Lord and Raju methods, one can specify either the IRT model to be fitted (by means of \code{model}, \code{c} and \code{engine} arguments), 
 or the item parameter estimates with arguments \code{irtParam} and \code{same.scale}. See \code{\link{difLord}} and \code{\link{difRaju}} 
 for further details. 

 The threshold for detecting DIF items depends on the method. For standardization it has to be fully specified (through the \code{thr} argument),
 while for the other methods it is depending on the significance level set by \code{alpha}.

 Option \code{correct} specifies whether the continuity correction has to be applied to Mantel-Haenszel statistic (see \code{\link{difMH}}).

 For logistic regression, the argument \code{type} permits to test either both uniform and nonuniform effects simultaneously (\code{type="both"}), only uniform
 DIF effect (\code{type="udif"}) or only nonuniform DIF effect (\code{type="nudif"}). See \code{\link{Logistik}} for further details.

 Item purification can be requested by specifying \code{purify} option to \code{TRUE}. Recall that item purification process is slightly different 
 for IRT and for non-IRT based methods. See the corresponding methods for further information.
}
 
\references{
 Dorans, N. J. and Kullick, E. (1986). Demonstrating the utility of the standardization approach to assessing unexpected differential item performance on the Scholastic Aptitude Test. \emph{Journal of Educational Measurement, 23}, 355-368.

 Holland, P. W. and Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Dirs.), \emph{Test validity}. Hillsdale, New Jersey: Lawrence Erlbaum Associates.

 Lord, F. (1980). \emph{Applications of item response theory to practical testing problems}. Hillsdale, NJ: Lawrence Erlbaum Associates.

 Penfield, R.D. (2003). Application of the Breslow-Day test of trend in odds ratio heterogeneity to the detection of nonuniform DIF. \emph{Alberta Journal of Educational Research, 49}, 231-243.

 Raju, N. S. (1990). Determining the significance of estimated signed and unsigned areas between two item response functions. \emph{Applied Psychological Measurement, 14}, 197-207.
 
 Swaminathan, H. and Rogers, H. J. (1990). Detecting differential item functioning using logistic regression procedures. \emph{Journal of Educational Measurement, 27}, 361-370.

 Thissen, D., Steinberg, L. and Wainer, H. (1988). Use of item response theory in the study of group difference in trace lines. 
 In H. Wainer and H. Braun (Eds.), \emph{Test validity}. Hillsdale, NJ: Lawrence Erlbaum Associates.
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
 \code{\link{difMH}}, \code{\link{difStd}}, \code{\link{difBD}}, \code{\link{difLogistic}}, \code{\link{difLord}}, \code{\link{difRaju}}, \code{\link{difLRT}}
 }

\examples{

 \dontrun{
 # Loading of the verbal data
 data(verbal)
 attach(verbal)

 # Excluding the "Anger" variable
 verbal<-verbal[colnames(verbal)!="Anger"]

 # Comparing Mantel-Haenszel, standardization and logistic regression
 # Standardization threshold of 0.08, no continutiy correction,
 # with item purification 
 dichoDif(verbal, group=25, focal.name=1, method=c("MH","Std","Logistic"),
 correct=FALSE, thr=0.08, purify=TRUE)

 # Comparing Lord and Raju results with 2PL model and
 # with item purification 
 dichoDif(verbal, group=25, focal.name=1, method=c("Lord","Raju"),
 model="2PL", purify=TRUE)
 }
 }
