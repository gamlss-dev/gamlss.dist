\name{gamlss.family}
\alias{gamlss.family}
\alias{as.gamlss.family}
\alias{print.gamlss.family}
\alias{gamlss.family.default}
\alias{as.family}
\title{ Family Objects for fitting a GAMLSS model}
\description{
GAMLSS families are the current available distributions that can be fitted using the \code{gamlss()} function.    
}

\usage{
gamlss.family(object,...)
as.gamlss.family(object)
as.family(object)
\method{print}{gamlss.family}(x,...)
}

\arguments{
  \item{object}{a gamlss family object e.g. \code{BCT} }
  \item{x}{a gamlss family object e.g. \code{BCT}}
  \item{...}{further arguments passed to or from other methods.}
}

\details{
 There are several distributions available for the response variable in the \code{gamlss} function. 
 The following table display their names and their abbreviations in \code{R}. Note that the different distributions can be fitted 
 using their \code{R} abbreviations 
 (and optionally excluding the brackets) i.e. family=BI(), family=BI are equivalent. 
 \tabular{lll}{ 
  \bold{Distributions}         \tab \bold{R names}                       \tab \bold{No of parameters} \cr
  Beta                         \tab \code{\link{BE}()}                   \tab  2  \cr
  Beta Binomial                \tab \code{\link{BB}()}                   \tab  2  \cr
  Beta one inflated            \tab \code{\link{BEOI}()}    \tab  3  \cr
  Beta zero inflated           \tab \code{\link{BEZI}()}    \tab  3  \cr
  Beta inflated                \tab \code{\link{BEINF}()}                \tab  4  \cr
  Binomial                     \tab \code{\link{BI}()}                   \tab  1  \cr
  Box-Cox Cole and Green       \tab \code{\link{BCCG}()}                 \tab  3  \cr
  Box-Cox Power Exponential    \tab \code{\link{BCPE}()}                 \tab  4  \cr
  Box-Cox-t                    \tab \code{\link{BCT}()}                  \tab  4  \cr
  Delaport                     \tab \code{\link{DEL}()}     \tab  3  \cr
  Exponential                  \tab \code{\link{EXP}()}     \tab  1  \cr
  Exponential Gaussian         \tab \code{\link{exGAUS}()}  \tab  3  \cr
  Exponential generalized Beta type 2 \tab \code{\link{EGB2}()} \tab  4  \cr
  Gamma                        \tab \code{\link{GA}()}                   \tab  2  \cr
  Generalized Beta  type 1     \tab \code{\link{GB1}()}     \tab  4  \cr
  Generalized Beta  type 2     \tab \code{\link{GB2}()}     \tab  4  \cr
  Generalized Gamma            \tab \code{\link{GG}()}      \tab  3  \cr
  Generalized Inverse Gaussian \tab \code{\link{GIG}()}     \tab  3  \cr
  Generalized t                \tab \code{\link{GT}()}      \tab  4  \cr
  Geometric                    \tab \code{\link{GEOM}()}                 \tab  1  \cr
  Gumbel                       \tab \code{\link{GU}()}                   \tab  2  \cr
  Inverse Gamma                \tab \code{\link{IGAMMA}()}               \tab  2  \cr
  Inverse Gaussian             \tab \code{\link{IG}()}                   \tab  2  \cr
  Johnson's SU                 \tab \code{\link{JSU}()}                  \tab  4  \cr   
  Logarithmic                  \tab \code{\link{LG}()}     \tab  1  \cr
  Logistic                     \tab \code{\link{LO}()}                   \tab  2  \cr
  log-Normal                   \tab \code{\link{LOGNO}()}                \tab  2  \cr
  log-Normal  (Box-Cox)        \tab \code{\link{LNO}()}                  \tab  3 (1 fixed)  \cr
  Negative Binomial type I     \tab \code{\link{NBI}()}                  \tab  2  \cr
  Negative Binomial type II    \tab \code{\link{NBII}()}                 \tab  2  \cr
  Normal Exponential \emph{t}   \tab \code{\link{NET}()}                  \tab  4 (2 fixed)  \cr
  Normal                       \tab \code{\link{NO}()}                   \tab  2  \cr
  Normal Family                \tab \code{\link{NOF}()}     \tab  3 (1 fixed)  \cr
  Pareto type 2                \tab \code{\link{PARETO2}()}              \tab  2  \cr
  Pareto type 2  original      \tab \code{\link{PARETO2o}()}              \tab  2  \cr
  Power Exponential            \tab \code{\link{PE}()}                   \tab  3  \cr
  Power Exponential  type 2    \tab \code{\link{PE2}()}                  \tab  3  \cr
  Poison                       \tab \code{\link{PO}()}                   \tab  1  \cr
  Poisson inverse Gaussian     \tab \code{\link{PIG}()}                  \tab  2  \cr
  Reverse generalized extreme  \tab \code{\link{RGE}()}     \tab  3  \cr
  Reverse Gumbel               \tab \code{\link{RG}()}                   \tab  2  \cr 
  Skew Power Exponential type 1 \tab \code{\link{SEP1}()}   \tab  4  \cr 
  Skew Power Exponential type 2 \tab \code{\link{SEP2}()}   \tab  4  \cr 
  Skew Power Exponential type 3 \tab \code{\link{SEP3}()}   \tab  4  \cr 
  Skew Power Exponential type 4 \tab \code{\link{SEP4}()}   \tab  4  \cr 
  Shash                        \tab \code{\link{SHASH}()}   \tab  4  \cr 
  Shash  original              \tab \code{\link{SHASHo}()}   \tab  4  \cr
  Shash  original 2            \tab \code{\link{SHASH}()}   \tab  4  \cr
  Sichel  (original)           \tab \code{\link{SI}()}      \tab  3  \cr
  Sichel  (mu as the maen)     \tab \code{\link{SICHEL}()}  \tab  3  \cr
  Skew t  type 1               \tab \code{\link{ST1}()}     \tab  3  \cr 
  Skew t  type 2               \tab \code{\link{ST2}()}     \tab  3  \cr 
  Skew t  type 3               \tab \code{\link{ST3}()}     \tab  3  \cr 
  Skew t  type 4               \tab \code{\link{ST4}()}     \tab  3  \cr 
  Skew t  type 5               \tab \code{\link{ST5}()}     \tab  3  \cr 
  t-distribution               \tab \code{\link{TF}()}      \tab  3  \cr
  Waring                       \tab \code{\link{WARING}()} \tab  1  \cr
  Weibull                      \tab \code{\link{WEI}()}     \tab  2  \cr
  Weibull(PH parameterization) \tab \code{\link{WEI2}()}    \tab  2  \cr
  Weibull (mu as mean)         \tab \code{\link{WEI3}()}    \tab  2  \cr
  Yule                         \tab \code{\link{YULE}()}    \tab  1  \cr
  Zero adjusted binomial       \tab \code{\link{ZABI}()}    \tab  2   \cr 
  Zero inflated binomial       \tab \code{\link{ZIBI}()}    \tab  2   \cr 
  Zero adjusted logarithmic    \tab \code{\link{ZALG}()}    \tab  2   \cr 
  Zero inflated poisson        \tab \code{\link{ZIP}()}     \tab  2  \cr
  Zero inf. poiss.(mu as mean) \tab \code{\link{ZIP2}()}    \tab  2  \cr
  Zero adjusted poisson        \tab \code{\link{ZAP}()}     \tab  2   \cr 
  Zero adjusted IG             \tab \code{\link{ZAIG}()}    \tab  2  \cr
  }
 Note that some of the distributions are in the package \code{gamlss.dist}.
 The parameters of the distributions are in order, \bold{\code{mu}} for location, \bold{\code{sigma}} for scale (or dispersion), 
 and \bold{\code{nu}}  and \bold{\code{tau}} for shape.
 More specifically for the \code{BCCG} family \code{mu} is the median, \code{sigma} approximately the coefficient of variation, and \code{nu} the skewness parameter.
 The parameters for \code{BCPE} distribution have the same interpretation with the extra fourth parameter \code{tau} modelling 
 the kurtosis of the distribution. The parameters for BCT have the same interpretation except that 
 \eqn{\sigma [(\tau/(\tau-2))^{0.5}]}{sigma*((tau/(tau-2))^0.5)} is 
 approximately the coefficient of variation.
 
 All of the distribution in the above list are also provided with the corresponding \code{d},  \code{p}, \code{q} and \code{r} functions 
 for  density (pdf), distribution function (cdf), quantile function and random  generation function respectively, (see individual distribution for details). 
}

\value{
  The above GAMLSS families return an object which is of type \code{gamlss.family}. This object is used to define the family in the \code{gamlss()} fit. 
}
\references{Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk} and Calliope Akantziliotou}


\note{More distributions will be documented in later GAMLSS releases. Further user defined distributions can 
      be incorporate relatively easy, see, for example, the help documentation accompanying the gamlss library.}


\seealso{ \code{\link{BE}},\code{\link{BB}},\code{\link{BEINF}},\code{\link{BI}},\code{\link{LNO}},\code{\link{BCT}},
          \code{\link{BCPE}},\code{\link{BCCG}},
          \code{\link{GA}},\code{\link{GU}},\code{\link{JSU}},\code{\link{IG}},\code{\link{LO}},
          \code{\link{NBI}},\code{\link{NBII}},\code{\link{NO}},\code{\link{PE}},\code{\link{PO}},
          \code{\link{RG}},\code{\link{PIG}},\code{\link{TF}},\code{\link{WEI}},\code{\link{WEI2}},
          \code{\link{ZIP}}}

\examples{
 normal<-NO(mu.link="log", sigma.link="log")
 normal
}
\keyword{regression}% 
