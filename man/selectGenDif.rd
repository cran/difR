\name{selectGenDif}
\alias{selectGenDif}


\title{Selection of one of the DIF detection methods among multiple groups}

\description{
 This function performs DIF detection among multiple groups for one pre-specified method. 
 }

\usage{
 selectGenDif(Data, group, focal.names, method, type="both", 
 alpha=0.05, model="2PL", c=NULL, engine = "ltm", irtParam=NULL,
 nrFocal=2, same.scale=TRUE, purify=FALSE, nrIter=10)
 }
 

\arguments{
 \item{Data}{numeric: either the data matrix only, or the data matrix plus the vector of group membership. See \bold{Details}.}
 \item{group}{numeric or character: either the vector of group membership or the column indicator (within data) of group membership. See \bold{Details}.}
 \item{focal.names}{numeric or character vector indicating the levels of \code{group} which correspond to the focal groups.}
 \item{method}{character: the name of the selected method. See \bold{Details}.}
 \item{type}{a character string specifying which DIF effects must be tested (default is \code{"both"}). See \bold{Details}.}
 \item{alpha}{numeric: significance level (default is 0.05).}
 \item{model}{character: the IRT model to be fitted (either \code{"1PL"}, \code{"2PL"} or \code{"3PL"}). Default is \code{"2PL"}.}
 \item{c}{optional numeric value or vector giving the values of the constrained pseudo-guessing parameters. See \bold{Details}.}
 \item{engine}{character: the engine for estimating the 1PL model, either \code{"ltm"} (default) or \code{"lme4"}.}
 \item{irtParam}{matrix with \emph{2J} rows (where \emph{J} is the number of items) and at most 9 columns containing item parameters estimates. See \bold{Details}.}
 \item{nrFocal}{numeric: the number of focal groups (default is 2).}
 \item{same.scale}{logical: are the item parameters of the \code{irtParam} matrix on the same scale? (default is "TRUE"). See \bold{Details}.}
 \item{purify}{logical: should the method be used iteratively to purify the set of anchor items? (default is FALSE).}
 \item{nrIter}{numeric: the maximal number of iterations in the item purification process. Default is 10.} 
}

\value{
 The output of the selected DIF detection method.
 }

\details{
 This is a generic function which calls one of the DIF detection methods for multiple groups,
 and displays its output. It is mainly used as a routine for \code{\link{genDichoDif}} command.

 There are three possible methods currently implemented: \code{"GMH"} for Generalized Mantel-Haenszel (Penfield, 2001), \code{"genLogistic"} 
 for generalized logistic regression (Magis, Raiche and Beland, 2009) and \code{"genLord"} for generalized Lord's chi-square
 test (Kim, Cohen and Park, 1995).

 The \code{Data} is a matrix whose rows correspond to the subjects and columns to the items. Missing values are not allowed.
 In addition, \code{Data} can hold the vector of group membership. If so, \code{group} indicates the column of \code{Data} which 
 corresponds to the group membership, either by specifying its name or by giving the column number. Otherwise,
 \code{group} must be a vector of same length as \code{nrow(Data)}.
 
 The vector of group membership must hold only at least three different values, either as numeric or character. The focal groups are defined by
 the values of the argument \code{focal.names}. 

 For the generalized logistic regression method, the argument \code{type} permits to test either both uniform and nonuniform effects
 simultaneously (\code{type="both"}), only uniform DIF effect (\code{type="udif"}) or only nonuniform DIF effect (\code{type="nudif"}).

 For generalized Lord method, one can specify either the IRT model to be fitted (by means of \code{model}, \code{c} and \code{engine} arguments), 
 or the item parameter estimates with arguments \code{irtParam}, \code{nrFocal} and \code{same.scale}. See \code{\link{difGenLord}}
 for further details. 

 The threshold for detecting DIF items depends on the method and is depending on the significance level set by \code{alpha}.

 Item purification can be requested by specifying \code{purify} option to \code{TRUE}. Recall that item purification is slightly different 
 for IRT and for non-IRT based methods. See the corresponding methods for further information.
}
 
\references{
 Kim, S.-H., Cohen, A.S. and Park, T.-H. (1995). Detection of differential item functioning in multiple groups. \emph{Journal of Educational Measurement, 32}, 261-276. 
 
 Magis, D., Raiche, G. and Beland, S. (2009). A logistic regression procedure to detect differential item functioning among multiple groups. Unpublished 
 manuscript.

 Penfield, R. D. (2001). Assessing differential item functioning among multiple groups: a comparison of three Mantel-Haenszel procedures. \emph{Applied Measurement in Education, 14}, 235-259. 
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
 \code{\link{difGMH}}, \code{\link{difGenLogistic}}, \code{\link{difGenLord}} 
 }

\examples{
 \dontrun{
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

 # Calling generalized Mantel-Haenszel
 selectGenDif(Verbal, group=25, focal.names=names, method="GMH")

 # Calling generalized logistic regression
 selectGenDif(Verbal, group=25, focal.names=names, method="genLogistic")

 # Calling generalized Lord method (2PL model)
 selectGenDif(Verbal, group=25, focal.names=names, method="genLord",
 model="2PL")
 }
}
