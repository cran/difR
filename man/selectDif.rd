\name{selectDif}
\alias{selectDif}


\title{Selection of one of the DIF detection methods}

\description{
 This function performs DIF detection for one pre-specified method. 
 }

\usage{
selectDif(Data, group, focal.name, method, anchor = NULL, props = NULL, 
 	thrTID = 1.5, alpha = 0.05, MHstat = "MHChisq", correct = TRUE, 
 	exact = FALSE, stdWeight = "focal", thrSTD = 0.1, BDstat = "BD", 
 	member.type = "group", match = "score", type = "both", criterion = "LRT", 
 	model = "2PL", c = NULL, engine = "ltm", discr = 1, irtParam = NULL, 
 	same.scale = TRUE, signed = FALSE, purify = FALSE, nrIter = 10, 
 	p.adjust.method = NULL, save.output = FALSE, output = c("out", "default"))
 }
 
\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within data) of group membership. See \bold{Details}.}
 \item{focal.name}{numeric or character indicating the level of \code{group} which corresponds to the focal group.}
 \item{method}{character: the name of the selected method. See \bold{Details}.}
\item{anchor}{either \code{NULL} (default) or a vector of item names (or identifiers) to specify the anchor items. See \bold{Details}.}
 \item{props}{either \code{NULL} (default) or a two-column matrix with proportions of success in the reference group and the focal group. See \bold{Details }.}
 \item{thrTID}{numeric: the threshold for detecting DIF items with TID method (default is 1.5).}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{MHstat}{character: specifies the DIF statistic to be used for DIF identification. Possible values are \code{"MHChisq"} (default) and \code{"logOR"}.
              See \bold{Details }.}
 \item{correct}{logical: should the continuity correction be used? (default is TRUE).}
 \item{exact}{logical: should an exact test be computed? (default is \code{FALSE}).}
 \item{stdWeight}{character: the type of weights used for the standardized P-DIF statistic. Possible values are \code{"focal"} (default),
                  \code{"reference"} and \code{"total"}. See \bold{Details}.}
 \item{thrSTD}{numeric: the threshold (cut-score) for standardized P-DIF statistic (default is 0.10).}
 \item{BDstat}{character specifying the DIF statistic to be used. Possible values are \code{"BD"} (default) and \code{"trend"}. See \bold{Details}.}
 \item{member.type}{character: either \code{"group"} (default) to specify that group membership is made of two groups, or \code{"cont"} to indicate that group membership is based on a  continuous criterion. See \bold{Details}.}
 \item{match}{specifies the type of matching criterion. Can be either \code{"score"} (default) to compute the test score, or any continuous or discrete variable with the same length as the number of rows of \code{Data}. See \bold{Details}.}
 \item{type}{a character string specifying which DIF effects must be tested. Possible values are \code{"both"} (default), \code{"udif"} and \code{"nudif"}.
            See \bold{Details}.}
 \item{criterion}{a character string specifying which DIF statistic is computed. Possible values are \code{"LRT"} (default) or \code{"Wald"}. See \bold{Details}.}
 \item{model}{character: the IRT model to be fitted (either \code{"1PL"}, \code{"2PL"} or \code{"3PL"}). Default is \code{"2PL"}.}
 \item{c}{optional numeric value or vector giving the values of the constrained pseudo-guessing parameters. See \bold{Details}.}
 \item{engine}{character: the engine for estimating the 1PL model, either \code{"ltm"} (default) or \code{"lme4"}.}
 \item{discr}{either \code{NULL} or a real positive value for the common discrimination parameter (default is 1). Used onlky if \code{model} is \code{"1PL"} and
             \code{engine} is \code{"ltm"}. See \bold{Details}.} 
 \item{irtParam}{matrix with \emph{2J} rows (where \emph{J} is the number of items) and at most 9 columns containing item parameters estimates. See \bold{Details}.}
 \item{same.scale}{logical: are the item parameters of the \code{irtParam} matrix on the same scale? (default is "TRUE"). See \bold{Details}.}
 \item{signed}{logical: should the Raju's statistics be computed using the signed (\code{TRUE}) or unsigned (\code{FALSE}, default)
               area? See \bold{Details}.}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process (default is 10).}
\item{p.adjust.method}{either \code{NULL} (default) or the acronym of the method for p-value adjustment for multiple comparisons. See \bold{Details}.}
 \item{save.output}{logical: should the output be saved into a text file? (Default is \code{FALSE}).}
 \item{output}{character: a vector of two components. The first component is the name of the output file, the second component is either the file path or 
              \code{"default"} (default value). See \bold{Details}.} 
}

\value{
 The output of the selected DIF detection method.
 }

\details{
 This is a generic function which calls one of the DIF detection methods and displays its output. It is mainly used as a routine for \code{\link{dichoDif}} command.

 The possible methods are: \code{"TID"} for Transformed Item Difficulties (TID) method (Angoff and Ford, 1973), \code{"MH"} for mantel-Haenszel (Holland and Thayer, 1988), \code{"Std"} for standardization (Dorans and Kulick, 1986), \code{"Logistic"} for logistic regression (Swaminathan and Rogers, 1990), \code{"BD"} for Breslow-Day method (Penfield, 2003), \code{"Lord"} for Lord's chi-square test (Lord, 1980), \code{"Raju"} for Raju's area method (Raju, 1990), and \code{"LRT"} for likelihood-ratio test method (Thissen, Steinberg and Wainer, 1988).

 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise, \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 Missing values are allowed for item responses (not for group membership) but must be coded as \code{NA} values. They are discarded from either the computation of the sum-scores, the fitting of the logistic models or the IRT models (according to the method).

 The vector of group membership must hold only two different values, either as numeric or character. The focal group is defined by the argument \code{focal.name}. 

 With the TID method, one can alternatively provide the matrix of proportions of success in for each item in each group. This matrix must have the same format as that provided to the \code{\link{trItemDiff}} function; see the corresponding help file for further details.

 For Lord and Raju methods, one can specify either the IRT model to be fitted (by means of \code{model}, \code{c}, \code{engine} and \code{discr} arguments), or the item parameter estimates with arguments \code{irtParam} and \code{same.scale}. See \code{\link{difLord}} and \code{\link{difRaju}} for further details. 

 The threshold for detecting DIF items depends on the method. For standardization it has to be fully specified (with the \code{thr} argument), as well as for the TID method (through the \code{thrTID} argument). For the other methods it is depending on the significance level set by \code{alpha}.

 For Mantel-Haenszel method, the DIF statistic can be either the Mantel-Haenszel chi-square statistic or the log odds-ratio statistic. The method is specified by the argument \code{MHstat}, and the default value is \code{"MHChisq"} for the chi-square statistic. Moreover, the option \code{correct} specifies whether the continuity correction has to be applied to Mantel-Haenszel statistic. See \code{\link{difMH}} for further details.

 By default, the asymptotic Mantel-Haenszel statistic is computed. However, the exact statistics and related P-values can
 be obtained by specifying the logical argument \code{exact} to \code{TRUE}. See Agresti (1990, 1992) for further details about exact inference.

 The weights for computing the standardized P-DIF statistics are defined through the argument \code{stdWeight}, with possible values \code{"focal"} (default value), \code{"reference"} and \code{"total"}. See \code{\link{stdPDIF}} for further details. 

 For Breslow-Day method, two test statistics are available: the usual Breslow-Day statistic for testing homogeneous association (Aguerri, Galibert, Attorresi and Maranon, 2009) and the trend test statistic for assessing some monotonic trend in the odds ratios (Penfield, 2003). The DIF statistic is supplied by the 
 \code{BDstat} argument, with values \code{"BD"} (default) for the usual statistic and \code{"trend"} for the trend test statistic.
 
 For logistic regression, the argument \code{type} permits to test either both uniform and nonuniform effects simultaneously (\code{type="both"}), only uniform DIF effect (\code{type="udif"}) or only nonuniform DIF effect (\code{type="nudif"}). The \code{criterion} argument specifies the DIF statistic to be computed, either the likelihood ratio test statistic (with \code{criterion="LRT"}) or the Wald test (with \code{criterion="Wald"}). Moreover, the group membership can be either a vector of two distinct values, one for the reference group and one for the focal group, or a continuous or discrete variable that acts as the "group" membership variable. In the former case, the \code{member.type} argument is set to \code{"group"} and the \code{focal.name} defines which value in the \code{group} variable stands for the focal group. In the latter case, \code{member.type} is set to \code{"cont"}, \code{focal.name} is ignored and each value of the \code{group} represents one "group" of data (that is, the DIF effects are investigated among participants relying on different values of some discrete or continuous trait). Finally, the matching criterion can be either the test score or any other continuous or discrete variable to be passed in the \code{Logistik} function. This is specified by the \code{match} argument. By default, it takes the value \code{"score"} and the test score (i.e. raw score) is computed. The second option is to assign to \code{match} a vector of continuous or discrete numeric values, which acts as the matching criterion. Note that for consistency this vector should not belong to the \code{Data} matrix. See \code{\link{Logistik}} for further details.

 For Raju's method, the type of area (signed or unsigned) is fixed by the logical \code{signed} argument, with default value \code{FALSE} (i.e. unsigned areas). See \code{\link{RajuZ}} for further details.

 Item purification can be requested by specifying \code{purify} option to \code{TRUE}. Recall that item purification is slightly different for IRT and for non-IRT based methods. See the corresponding methods for further information.

Adjustment for multiple comparisons is possible with the argument \code{p.adjust.method}. See the corresponding methods for further information.

A pre-specified set of anchor items can be provided through the \code{anchor} argument. For non-IRT methods, anchor items are used to compute the test score (as matching criterion). For IRT methods, anchor items are used to rescale the item parameters on a common metric. See the corresponding methods for further information. Note that \code{anchor} argument is not working with \code{"LRT"} method.

The output of the selected method can be stored in a text file by fixing \code{save.output} and \code{output} appropriately. See the help file of the corresponding method for further information.
}
 
\references{
 Agresti, A. (1990). \emph{Categorical data analysis}. New York: Wiley.

 Agresti, A. (1992). A survey of exact inference for contingency tables. \emph{Statistical Science, 7}, 131-177.

 Aguerri, M.E., Galibert, M.S., Attorresi, H.F. and Maranon, P.P. (2009). Erroneous detection of nonuniform DIF using the Breslow-Day test in a short test. 
 \emph{Quality and Quantity, 43}, 35-44. 

 Angoff, W. H., and Ford, S. F. (1973). Item-race interaction on a test of scholastic aptitude. \emph{Journal of Educational Measurement, 2}, 95-106.

 Dorans, N. J. and Kulick, E. (1986). Demonstrating the utility of the standardization approach to assessing unexpected differential item performance on the 
 Scholastic Aptitude Test. \emph{Journal of Educational Measurement, 23}, 355-368.

 Holland, P. W. and Thayer, D. T. (1988). Differential item performance and the Mantel-Haenszel procedure. In H. Wainer and H. I. Braun (Dirs.), 
 \emph{Test validity}. Hillsdale, NJ: Lawrence Erlbaum Associates.

 Lord, F. (1980). \emph{Applications of item response theory to practical testing problems}. Hillsdale, NJ: Lawrence Erlbaum Associates.

 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

 Penfield, R.D. (2003). Application of the Breslow-Day test of trend in odds ratio heterogeneity to the detection of nonuniform DIF. \emph{Alberta Journal of 
 Educational Research, 49}, 231-243.

 Raju, N. S. (1990). Determining the significance of estimated signed and unsigned areas between two item response functions. \emph{Applied Psychological 
 Measurement, 14}, 197-207.
 
 Swaminathan, H. and Rogers, H. J. (1990). Detecting differential item functioning using logistic regression procedures. \emph{Journal of Educational 
 Measurement, 27}, 361-370.

 Thissen, D., Steinberg, L. and Wainer, H. (1988). Use of item response theory in the study of group difference in trace lines. 
 In H. Wainer and H. Braun (Eds.), \emph{Test validity}. Hillsdale, NJ: Lawrence Erlbaum Associates.
 }
 
\author{
    Sebastien Beland \cr
    Collectif pour le Developpement et les Applications en Mesure et Evaluation (Cdame) \cr
    Universite du Quebec a Montreal \cr
    \email{sebastien.beland.1@hotmail.com}, \url{http://www.cdame.uqam.ca/} \cr
    David Magis \cr
    Department of Education, University of Liege \cr
    Research Group of Quantitative Psychology and Individual Differences, KU Leuven \cr
    \email{David.Magis@ulg.ac.be}, \url{http://ppw.kuleuven.be/okp/home/} \cr
    Gilles Raiche \cr
    Collectif pour le Developpement et les Applications en Mesure et Evaluation (Cdame) \cr
    Universite du Quebec a Montreal \cr
    \email{raiche.gilles@uqam.ca}, \url{http://www.cdame.uqam.ca/} \cr 
 }


\seealso{
 \code{\link{difTID}}, \code{\link{difMH}}, \code{\link{difStd}}, \code{\link{difBD}}, \code{\link{difLogistic}}, \code{\link{difLord}}, \code{\link{difRaju}},
 \code{\link{difLRT}}, \code{\link{dichoDif}}
 
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)
 attach(verbal)

 # Excluding the "Anger" variable
 verbal <- verbal[colnames(verbal)!="Anger"]

 # Calling Mantel-Haenszel 
 selectDif(verbal, group = 25, focal.name = 1, method = "MH")

 # Calling Mantel-Haenszel and saving output in 'MH.txt' file
 selectDif(verbal, group = 25, focal.name = 1, method = "MH", 
    save.output = TRUE, output = c("MH", "default"))

 # Calling Lord method
 # 2PL model, with item purification
 selectDif(verbal, group = 25, focal.name = 1, method = "Lord", model = "2PL", 
           purify = TRUE)
 }
 }
