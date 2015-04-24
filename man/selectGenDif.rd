\name{selectGenDif}
\alias{selectGenDif}


\title{Selection of one of the DIF detection methods among multiple groups}

\description{
 This function performs DIF detection among multiple groups for one pre-specified method. 
 }

\usage{
selectGenDif(Data, group, focal.names, method, anchor = NULL, match = "score", 
 	type = "both", criterion = "LRT", alpha = 0.05, model = "2PL", c = NULL, 
 	engine = "ltm", discr = 1, irtParam = NULL, nrFocal = 2, same.scale = TRUE, 
 	purify = FALSE, nrIter = 10, save.output = FALSE, 
 	output = c("out", "default"))	
 }
 

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within data) of group membership. See \bold{Details}.}
 \item{focal.names}{numeric or character vector indicating the levels of \code{group} which correspond to the focal groups.}
 \item{method}{character: the name of the selected method. See \bold{Details}.}
\item{anchor}{either \code{NULL} (default) or a vector of item names (or identifiers) to specify the anchor items. See \bold{Details}.}
 \item{match}{specifies the type of matching criterion. Can be either \code{"score"} (default) to compute the test score, or any continuous or discrete variable with the same length as the number of rows of \code{Data}. See \bold{Details}.}
 \item{type}{a character string specifying which DIF effects must be tested. Possible values are \code{"both"} (default), \code{"udif"} and \code{"nudif"}. See \bold{Details}.}
 \item{criterion}{character: the type of test statistic used to detect DIF items with generalized logistic regression. Possible values are \code{"LRT"} (default) and \code{"Wald"}. See \bold{Details}.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{model}{character: the IRT model to be fitted (either \code{"1PL"}, \code{"2PL"} or \code{"3PL"}). Default is \code{"2PL"}.}
 \item{c}{optional numeric value or vector giving the values of the constrained pseudo-guessing parameters. See \bold{Details}.}
 \item{engine}{character: the engine for estimating the 1PL model, either \code{"ltm"} (default) or \code{"lme4"}.}
 \item{discr}{either \code{NULL} or a real positive value for the common discrimination parameter (default is 1). Used onlky if \code{model} is \code{"1PL"} and \code{engine} is \code{"ltm"}. See \bold{Details}.}
 \item{irtParam}{matrix with \emph{2J} rows (where \emph{J} is the number of items) and at most 9 columns containing item parameters estimates. See \bold{Details}.}
 \item{nrFocal}{numeric: the number of focal groups (default is 2).}
 \item{same.scale}{logical: are the item parameters of the \code{irtParam} matrix on the same scale? (default is "TRUE"). See \bold{Details}.}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process (default is 10).} 
 \item{save.output}{logical: should the output be saved into a text file? (Default is \code{FALSE}).}
 \item{output}{character: a vector of two components. The first component is the name of the output file, the second component is either the file path or \code{"default"} (default value). See \bold{Details}.} 
}

\value{
 The output of the selected DIF detection method.
 }

\details{
 This is a generic function which calls one of the DIF detection methods for multiple groups, and displays its output. It is mainly used as a routine for \code{\link{genDichoDif}} command.

 There are three possible methods currently implemented: \code{"GMH"} for Generalized Mantel-Haenszel (Penfield, 2001), \code{"genLogistic"} for generalized logistic regression (Magis, Raiche, Beland and Gerard, 2010) and \code{"genLord"} for generalized Lord's chi-square test (Kim, Cohen and Park, 1995).

 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise, \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 Missing values are allowed for item responses (not for group membership) but must be coded as \code{NA} values. They are discarded from either the computation of the sum-scores, the fitting of the logistic models or the IRT models (according to the method).

 The vector of group membership must hold at least three different values, either as numeric or character. The focal groups are defined by the values of the argument \code{focal.names}. 

 For the generalized logistic regression method, the argument \code{type} permits to test either both uniform and nonuniform effects simultaneously (with \code{type="both"}), only uniform DIF effect (with \code{type="udif"}) or only nonuniform DIF effect (with \code{type="nudif"}). Furthermore, the argument \code{criterion} defines which test must be used, either the Wald test (\code{"Wald"}) or the likelihood ratio test
 (\code{"LRT"}).

 For generalized Lord method, one can specify either the IRT model to be fitted (by means of \code{model}, \code{c}, \code{engine} and \code{discr} arguments), or the item parameter estimates with arguments \code{irtParam}, \code{nrFocal} and \code{same.scale}. Moreover, the matching criterion can be either the test score or any other continuous or discrete variable to be passed in the \code{Logistik} function. This is specified by the \code{match} argument. By default, it takes the value \code{"score"} and the test score (i.e. raw score) is computed. The second option is to assign to \code{match} a vector of continuous or discrete numeric values, which acts as the matching criterion. Note that for consistency this vector should not belong to the \code{Data} matrix. See \code{\link{difGenLord}} for further details. 

 The threshold for detecting DIF items depends on the method and is depending on the significance level set by \code{alpha}.

 Item purification can be requested by specifying \code{purify} option to \code{TRUE}. Recall that item purification is slightly different for IRT and for non-IRT based methods. See the corresponding methods for further information.

A pre-specified set of anchor items can be provided through the \code{anchor} argument. For non-IRT methods, anchor items are used to compute the test score (as matching criterion). For IRT methods, anchor items are used to rescale the item parameters on a common metric. See the corresponding methods for further information. 

 The output of the selected method can be stored in a text file by fixing \code{save.output} and \code{output} appropriately. See the help file of the corresponding method for further information.
}
 
\references{
 Kim, S.-H., Cohen, A.S. and Park, T.-H. (1995). Detection of differential item functioning in multiple groups. \emph{Journal of Educational Measurement, 32},
 261-276. 
 
 Magis, D., Beland, S., Tuerlinckx, F. and De Boeck, P. (2010). A general framework and an R package for the detection
 of dichotomous differential item functioning. \emph{Behavior Research Methods, 42}, 847-862.

 Magis, D., Raiche, G., Beland, S. and Gerard, P. (2010). A logistic regression procedure to detect differential item functioning among multiple groups. Unpublished 
 manuscript.

 Penfield, R. D. (2001). Assessing differential item functioning among multiple groups: a comparison of three Mantel-Haenszel procedures. \emph{Applied Measurement
 in Education, 14}, 235-259. 
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
 \code{\link{difGMH}}, \code{\link{difGenLogistic}}, \code{\link{difGenLord}} 
 }

\examples{
\dontrun{

 # Loading of the verbal data
 data(verbal)
 attach(verbal)

 # Creating four groups according to gender ("Man" or "Woman") and trait
 # anger score ("Low" or "High")
 group <- rep("WomanLow", nrow(verbal))
 group[Anger>20 & Gender==0] <- "WomanHigh"
 group[Anger<=20 & Gender==1] <- "ManLow"
 group[Anger>20 & Gender==1] <- "ManHigh"

 # New data set
 Verbal <- cbind(verbal[,1:24], group)

 # Reference group: "WomanLow"
 names <- c("WomanHigh", "ManLow", "ManHigh")

 # Calling generalized Mantel-Haenszel
 selectGenDif(Verbal, group = 25, focal.names = names, method = "GMH")

 # Calling generalized Mantel-Haenszel and saving output in 'GMH.txt' file
 selectGenDif(Verbal, group = 25, focal.name = names, method = "GMH", 
              save.output = TRUE, output = c("GMH", "default"))

 # Calling generalized logistic regression
 selectGenDif(Verbal, group = 25, focal.names = names, method = "genLogistic")

 # Calling generalized Lord method (2PL model)
 selectGenDif(Verbal, group = 25, focal.names = names, method = "genLord", 
              model = "2PL")
  }
}
