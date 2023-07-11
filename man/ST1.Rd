\name{ST1}
\alias{ST1}
\alias{dST1}
\alias{pST1}
\alias{qST1}
\alias{rST1}
\alias{ST2}
\alias{dST2}
\alias{pST2}
\alias{qST2}
\alias{rST2}
\alias{ST3}
\alias{dST3}
\alias{pST3}
\alias{qST3}
\alias{rST3}
\alias{ST3C}
\alias{dST3C}
\alias{pST3C}
\alias{qST3C}
\alias{rST3C}
\alias{ST4}
\alias{dST4}
\alias{pST4}
\alias{qST4}
\alias{rST4}
\alias{ST5}
\alias{dST5}
\alias{pST5}
\alias{qST5}
\alias{rST5}
\alias{SST}
\alias{dSST}
\alias{pSST}
\alias{qSST}
\alias{rSST}

\title{The skew t distributions, type 1 to 5 }

\description{There are 5 different skew t distributions implemented in GAMLSS.

The Skew t type 1 distribution,  \code{ST1}, is based on Azzalini (1986), see pp. 411-412 of Rigby et al. (2019).

The skew t type 2 distribution, \code{ST2}, is based on Azzalini and Capitanio (2003) see pp. 412-414 of Rigby et al. (2019). 

The skew t type 3 , \code{ST3} and  \code{ST3C}, distribution is based Fernande and Steel (1998) see pp 413-415 of Rigby et al. (2019).
The difference betwwen the  \code{ST3} and \code{ST3C} is that the first is written entirely in  \code{R} while the second is in \code{C}.

The skew t type 4 distribution , \code{ST4}, is a spliced-shape distribution see 
see pp 413-415 of Rigby et al. (2019).


The skew t type 5 distribution , \code{ST5}, is  Jones and Faddy (2003).

The \code{SST} is a reparametrised version of  \code{ST3} where \code{sigma} is the standard deviation of the distribution.
}
\usage{
ST1(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link="log")
dST1(x, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE)
pST1(q, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, log.p = FALSE)
qST1(p, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, log.p = FALSE)
rST1(n, mu = 0, sigma = 1, nu = 0, tau = 2)

ST2(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")
dST2(x, mu = 0, sigma = 1, nu = 0, tau = 2, log = FALSE)
pST2(q, mu = 0, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, log.p = FALSE)
qST2(p, mu = 1, sigma = 1, nu = 0, tau = 2, lower.tail = TRUE, log.p = FALSE)
rST2(n, mu = 0, sigma = 1, nu = 0, tau = 2)

ST3(mu.link = "identity", sigma.link = "log", nu.link = "log", tau.link = "log")
dST3(x, mu = 0, sigma = 1, nu = 1, tau = 10, log = FALSE)
pST3(q, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
qST3(p, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
rST3(n, mu = 0, sigma = 1, nu = 1, tau = 10)

ST3C(mu.link = "identity", sigma.link = "log", nu.link = "log", tau.link = "log")
dST3C(x, mu = 0, sigma = 1, nu = 1, tau = 10, log = FALSE)
pST3C(q, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
qST3C(p, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
rST3C(n, mu = 0, sigma = 1, nu = 1, tau = 10)

SST(mu.link = "identity", sigma.link = "log", nu.link = "log", 
   tau.link = "logshiftto2")
dSST(x, mu = 0, sigma = 1, nu = 0.8, tau = 7, log = FALSE)
pSST(q, mu = 0, sigma = 1, nu = 0.8, tau = 7, lower.tail = TRUE, log.p = FALSE)
qSST(p, mu = 0, sigma = 1, nu = 0.8, tau = 7, lower.tail = TRUE, log.p = FALSE)
rSST(n, mu = 0, sigma = 1, nu = 0.8, tau = 7)

ST4(mu.link = "identity", sigma.link = "log", nu.link = "log", tau.link = "log")
dST4(x, mu = 0, sigma = 1, nu = 1, tau = 10, log = FALSE)
pST4(q, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
qST4(p, mu = 0, sigma = 1, nu = 1, tau = 10, lower.tail = TRUE, log.p = FALSE)
rST4(n, mu = 0, sigma = 1, nu = 1, tau = 10)

ST5(mu.link = "identity", sigma.link = "log", nu.link = "identity", tau.link = "log")
dST5(x, mu = 0, sigma = 1, nu = 0, tau = 1, log = FALSE)
pST5(q, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, log.p = FALSE)
qST5(p, mu = 0, sigma = 1, nu = 0, tau = 1, lower.tail = TRUE, log.p = FALSE)
rST5(n, mu = 0, sigma = 1, nu = 0, tau = 1)
}

\arguments{
 \item{mu.link}{ Defines the \code{mu.link}, with "identity" link as the default for the \code{mu} parameter. 
                  Other links are "\eqn{1/mu^2}" and "log"}
  \item{sigma.link}{ Defines the  \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter. 
                      Other links are "inverse" and "identity"}
  \item{nu.link}{Defines the  \code{nu.link}, with "identity" link as the default for the \code{nu} parameter. 
                  Other links are "\eqn{1/mu^2}" and "log"}
   \item{tau.link}{Defines the  \code{nu.link}, with "log" link as the default for the \code{nu} parameter. 
                   Other links are "inverse", "identity" }                
   \item{x,q}{vector of quantiles}
  \item{mu}{vector of \code{mu} parameter values }
  \item{sigma}{vector of scale parameter values}
  \item{nu}{vector of \code{nu} parameter values}
   \item{tau}{vector of \code{tau} parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is taken to be the number required}
}
\details{
The definitions of all Skew \emph{t} distributions is given in pp.409-420 of  of Rigby et al. (2019). 



}
\references{

Azzalini A. (1986) Futher results on a class of distributions which includes the normal ones, \emph{Statistica},
\bold{46}, pp. 199-208.

Azzalini A. and Capitanio, A. Distributions generated by perturbation of symmetry with emphasis on a multivariate skew t-distribution, \emph{Journal of the Royal Statistical Society: Series B (Statistical Methodology)}, \bold{65},
  pp. 367-389.

Jones, M.C. and Faddy, M. J. (2003) A skew extension of the t distribution, with applications. 
\emph{Journal of the Royal Statistical Society}, Series B, \bold{65}, pp 159-174. 

Fernandez, C. and Steel, M. F. (1998) On Bayesian modeling of fat tails and skewness. \emph{Journal of the American Statistical Association}, \bold{93}, pp. 359-371.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion),  \emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.


Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.


Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.
\doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).

 }
\author{Bob Rigby and Mikis Stasinopoulos  }
\note{ The mean of the ex-Gaussian is \eqn{\mu+\nu}{mu+nu} and the variance is \eqn{\sigma^2+\nu^2}{sigma^2+nu^2}.
}
\seealso{ \code{\link{gamlss.family}}, \code{\link{SEP1}}, \code{\link{SHASH}}}
\examples{ 
y<- rST5(200, mu=5, sigma=1, nu=.1)
hist(y)
curve(dST5(x, mu=30 ,sigma=5,nu=-1), -50, 50, main = "The ST5  density mu=30 ,sigma=5,nu=1")
# library(gamlss)
# m1<-gamlss(y~1, family=ST1)
# m2<-gamlss(y~1, family=ST2)
# m3<-gamlss(y~1, family=ST3)
# m4<-gamlss(y~1, family=ST4)
# m5<-gamlss(y~1, family=ST5) 
# GAIC(m1,m2,m3,m4,m5)
}
\keyword{distribution}% 
\keyword{regression}% 
