\name{selectDif}
\alias{selectDif}


\title{Selection of one of the DIF detection methods}

\description{
 This function performs DIF detection for one pre-specified method. 
 }

\usage{
 selectDif(Data, group, focal.name, method, alpha=0.05, 
  MHstat="MHChisq", correct=TRUE, stdWeight="focal", 
  thr=0.1, BDstat="BD",type="both", criterion="LRT",
  model="2PL", c=NULL, engine="ltm", irtParam=NULL,
  same.scale=TRUE, purify=FALSE, nrIter=10,
  save.output=FALSE, output=c("out","default"))
 }
 
\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within data) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{method}{character: the name of the selected method. See \bold{Details}.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{MHstat}{character: specifies the DIF statistic to be used for DIF identification. Possible values are \code{"MHChisq"} (default) and \code{"logOR"}. See \bold{Details }.}
 \item{correct}{logical: should the continuity correction be used? (default is TRUE).}
 \item{stdWeight}{character: the type of weights used for the standardized P-DIF statistic. Possible values are \code{"focal"} (default),
                  \code{"reference"} and \code{"total"}. See \bold{Details}.}
 \item{thr}{numeric: the threshold (cut-score) for standardized P-DIF statistic (default is 0.10).}
 \item{BDstat}{character specifying the DIF statistic to be used. Possible values are \code{"BD"} (default) and \code{"trend"}. See \bold{Details}.}
 \item{type}{a character string specifying which DIF effects must be tested. Possible values are \code{"both"} (default), \code{"udif"} and \code{"nudif"}. See \bold{Details}.}
 \item{criterion}{a character string specifying which DIF statistic is computed. Possible values are \code{"LRT"} (default) or \code{"Wald"}. See \bold{Details}.}
 \item{model}{character: the IRT model to be fitted (either \code{"1PL"}, \code{"2PL"} or \code{"3PL"}). Default is \code{"2PL"}.}
 \item{c}{optional numeric value or vector giving the values of the constrained pseudo-guessing parameters. See \bold{Details}.}
 \item{engine}{character: the engine for estimating the 1PL model, either \code{"ltm"} (default) or \code{"lme4"}.}
 \item{irtParam}{matrix with \emph{2J} rows (where \emph{J} is the number of items) and at most 9 columns containing item parameters estimates. See \bold{Details}.}
 \item{same.scale}{logical: are the item parameters of the \code{irtParam} matrix on the same scale? (default is "TRUE"). See \bold{Details}.}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.}
 \item{save.output}{logical: should the output be saved into a text file? (Default is \code{FALSE}).}
 \item{output}{character: a vector of two components. The first component is the name of the output file, the second component is either the file path or \code{"default"} (default value). See \bold{Details}.} 
}

\value{
 The output of the selected DIF detection method.
 }

\details{
 This is a generic function which calls one of the DIF detection methods and displays its output. It is mainly used as a routine for \code{\link{dichoDif}} command.

 The possible methods are: \code{"MH"} for mantel-Haenszel (Holland and Thayer, 1988), \code{"Std"} for standardization (Dorans and Kullick, 1986), \code{"Logistic"} 
 for logistic regression (Swaminathan and Rogers, 1990), \code{"BD"} for Breslow-Day method (Penfield, 2003), \code{"Lord"} for Lord's chi-square test (Lord, 1980), 
 \code{"Raju"} for Raju's area method (Raju, 1990), and \code{"LRT"} for likelihoo-ratio test method (Thissen, Steinberg and Wainer, 1988).

 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which 
 corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise,
 \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership must hold only two different values, either as numeric or character. The focal group is defined by
 of the argument \code{focal.name}. 

 For Lord and Raju methods, one can specify either the IRT model to be fitted (by means of \code{model}, \code{c} and \code{engine} arguments), 
 or the item parameter estimates with arguments \code{irtParam} and \code{same.scale}. See \code{\link{difLord}} and \code{\link{difRaju}} 
 for further details. 

 The threshold for detecting DIF items depends on the method. For standardization it has to be fully specified (with the \code{thr} argument),
 while for the other methods it is depending on the significance level set by \code{alpha}.

 For Mantel-Haenszel method, the DIF statistic can be either the Mantel-Haenszel chi-square statistic or the log odds-ratio statistic. The method is
 specified by the argument \code{MHstat}, and the default value is \code{"MHChisq"} for the chi-square statistic. Moreover, the option \code{correct}
 specifies whether the continuity correction has to be applied to Mantel-Haenszel statistic. See \code{\link{difMH}} for further details.

 The weights for computing the standardized P-DIF statistics are defined through the argument \code{stdWeight}, with possible values
 \code{"focal"} (default value), \code{"reference"} and \code{"total"}. See \code{\link{stdPDIF}} for further details. 

 For Breslow-Day method, two test statistics are available: the usual Breslow-Day statistic for testing homogeneous association (Aguerri, Galibert, Attorresi and Maranon, 2009)
 and the trend test statistic for assessing some monotonic trend in the odss ratios (Penfield, 2003). The DIF statistic is supplied by the \code{BDstat} argument, 
 with values \code{"BD"} (default) for the usual statistic and \code{"trend"} for the trend test statistic.
 
 For logistic regression, the argument \code{type} permits to test either both uniform and nonuniform effects simultaneously (\code{type="both"}), only uniform
 DIF effect (\code{type="udif"}) or only nonuniform DIF effect (\code{type="nudif"}). The \code{criterion} argument specifies the DIF statistic
 to be computed, either the likelihood ratio test statistic (with \code{criterion="LRT"}) or the Wald test (with \code{criterion="Wald"}).
 See \code{\link{Logistik}} for further details.

 Item purification can be requested by specifying \code{purify} option to \code{TRUE}. Recall that item purification is slightly different 
 for IRT and for non-IRT based methods. See the corresponding methods for further information.

 The output of the selected method can be stored in a text file by fixing \code{save.output} and \code{output} appropriately. See the help file of the corresponding
 method for further information.
}
 
\references{
 Aguerri, M.E., Galibert, M.S., Attorresi, H.F. and Maranon, P.P. (2009). Erroneous detection of nonuniform DIF using the Breslow-Day test in a short test. \emph{Quality and Quantity, 43}, 35-44. 

 Dorans, N. J. and Kullick, E. (1986). Demonstrating the utility of the standardization approach to assessing unexpected differential item performance on the Scholastic Aptitude Test. \emph{Journal of Educational Measurement, 23}, 355-368.

 Holland, P. W. and Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Dirs.), \emph{Test validity}. Hillsdale, New Jersey: Lawrence Erlbaum Associates.

 Lord, F. (1980). \emph{Applications of item response theory to practical testing problems}. Hillsdale, NJ: Lawrence Erlbaum Associates.

 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

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
 \code{\link{difMH}}, \code{\link{difStd}}, \code{\link{difBD}}, \code{\link{difLogistic}}, \code{\link{difLord}}, \code{\link{difRaju}}, \code{\link{difLRT}}, \code{\link{dichoDif}}
 
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)
 attach(verbal)

 # Excluding the "Anger" variable
 verbal<-verbal[colnames(verbal)!="Anger"]

 # Calling Mantel-Haenszel 
 selectDif(verbal, group=25, focal.name=1, method="MH")

 # Calling Mantel-Haenszel and saving output in 'MH.txt' file
 selectDif(verbal, group=25, focal.name=1, method="MH", 
    save.output=TRUE,output=c("MH","default"))

 # Calling Lord method
 # 2PL model, with item purification
 selectDif(verbal, group=25, focal.name=1, method="Lord",model="2PL",
 purify=TRUE)
 }
 }
