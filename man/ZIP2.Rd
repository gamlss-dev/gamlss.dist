\name{ZIP2}
\alias{ZIP2}
\alias{dZIP2}
\alias{pZIP2}
\alias{qZIP2}
\alias{rZIP2}
\alias{ZIP3}
\alias{ZIP3}
\alias{dZIP3}
\alias{pZIP3}
\alias{qZIP3}
\alias{rZIP3}


\title{Zero inflated poisson distribution for fitting a GAMLSS model }
\description{

The functions \code{ZIP2} and  \code{ZIP3} defines the zero inflated Poisson type 2 and type 3 distributions,  two parameter distributions, for a \code{gamlss.family} object to be used in GAMLSS fitting  using the function \code{gamlss()}. 

The functions \code{dZIP2}, \code{pZIP2}, \code{qZIP2} and \code{rZIP2} and the functions \code{dZIP3}, \code{pZIP3}, \code{qZIP3} and \code{rZIP3} define the density, distribution function, quantile function  and random generation for the inflated poisson, \code{ZIP2()}, and \code{ZIP3()}, distributions.

The \code{ZIP2} and  \code{ZIP3} are different parameterization of the \code{ZIP} distribution of Lambert (1992). In the \code{ZIP2} the \eqn{\mu} is the mean of the distribution. Similarly in the  \code{ZIP2} parameterization the  \eqn{\mu} is the mean of the distribution but additionally the variance has the form \eqn{Var(Y)=\mu(1 + \sigma)}. Acording  to Pereira \emph{et al.} (2026) "the primary appeal of the ZIP3 parameterization is in the interpretation of the regression parameters".
}
\usage{
ZIP2(mu.link = "log", sigma.link = "logit")
dZIP2(x, mu = 5, sigma = 0.1, log = FALSE)
pZIP2(q, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
qZIP2(p, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
rZIP2(n, mu = 5, sigma = 0.1)

ZIP3(mu.link = "log", sigma.link = "log")
dZIP3(x, mu = 5, sigma = 1, log = FALSE)
pZIP3(q, mu = 5, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qZIP3(p, mu = 5, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rZIP3(n, mu = 5, sigma = 1)
}

\arguments{
  \item{mu.link}{defines the \code{mu.link}, with "log" link as the default for the \code{mu} parameter}
  \item{sigma.link}{defines the \code{sigma.link}, with "logit" link as the default for the sigma parameter which in this case is the probability at zero. 
                   Other links are "probit" and "cloglog"'(complementary log-log)}
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive means}
  \item{sigma}{vector of probabilities at zero}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
}
\details{
The parametrization used for the \code{ZIP2}  version of the zero inflated Poison distribution   can be found  in pp 500-501 of Rigby \emph{et al.} (2019).  Note that  the mean of the distribution in this parameterization is \eqn{\mu}.

The parametrization used for the  version of the zero inflated Poison distribution \code{ZIP3} can be found Pereira \emph{et al}. (2026). Note that the mean of the distribution in this parameterization is \eqn{\mu} and the variance is  \eqn{Var(Y)=\mu(1 + \sigma)}.

}
\value{
  returns a \code{gamlss.family} object which can be us  ed to fit a zero inflated poisson distribution in the \code{gamlss()} function.  
}
\references{ 

Lambert, D. (1992), Zero-inflated Poisson Regression with an application to defects in Manufacturing,
    \emph{Technometrics},  \bold{34}, pp 1-14.


Pereira, G.H., Leao, J., Santos-Neto, M. and Cai, J., 2026. A more interpretable regression model for count data with excess of zeros. \emph{Statistical Methods in Medical Research}.


Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.


Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).

}
\author{ Bob Rigby, Gillian Heller and Mikis Stasinopoulos }



\seealso{ \code{\link{gamlss.family}}, \code{\link{ZIP}} }
\examples{
ZIP2()# gives information about the default links for the normal distribution
# creating data and plotting them 
dat<-rZIP2(1000, mu=5, sigma=.1)
r <- barplot(table(dat), col='lightblue')
dat<-rZIP3(1000, mu=5, sigma=.1)
r <- barplot(table(dat), col='lightblue')
plot(dZIP3(0:10, mu = 5, sigma = 1), type="h")
plot(pZIP3(0:10, mu = 5, sigma = 1), type="h")
# fit the disteibution
# library(gamlss) 
# mod1<-gamlss(dat~1, family=ZIP2)# fits a constant for mu and sigma 
# fitted(mod1)[1]
# fitted(mod1,"sigma")[1]
}
\keyword{distribution}
\keyword{regression}
