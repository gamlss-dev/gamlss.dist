\name{MN3}
\alias{MN3}
\alias{MN4}
\alias{MN5}
\alias{dMN3}
\alias{dMN4}
\alias{dMN5}
\alias{pMN3}
\alias{pMN4}
\alias{pMN5}
\alias{qMN3}
\alias{qMN4}
\alias{qMN5}
\alias{rMN3}
\alias{rMN4}
\alias{rMN5}
\alias{MULTIN}
\alias{fittedMN}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Multinomial distribution in GAMLSS
}
\description{
The set of function presented here is useful for fitting multinomial regression within gamlss.}
\usage{
MN3(mu.link = "log", sigma.link = "log")
MN4(mu.link = "log", sigma.link = "log", nu.link = "log")
MN5(mu.link = "log", sigma.link = "log", nu.link = "log", tau.link = "log")
MULTIN(type = "3")
fittedMN(model)

dMN3(x, mu = 1, sigma = 1, log = FALSE)
dMN4(x, mu = 1, sigma = 1, nu = 1, log = FALSE)
dMN5(x, mu = 1, sigma = 1, nu = 1, tau = 1, log = FALSE)

pMN3(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
pMN4(q, mu = 1, sigma = 1, nu = 1, lower.tail = TRUE, log.p = FALSE)
pMN5(q, mu = 1, sigma = 1, nu = 1, tau = 1, lower.tail = TRUE, log.p = FALSE)

qMN3(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qMN4(p, mu = 1, sigma = 1, nu = 1, lower.tail = TRUE, log.p = FALSE)
qMN5(p, mu = 1, sigma = 1, nu = 1, tau = 1, lower.tail = TRUE, log.p = FALSE)

rMN3(n, mu = 1, sigma = 1)
rMN4(n, mu = 1, sigma = 1, nu = 1)
rMN5(n, mu = 1, sigma = 1, nu = 1, tau = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{the link function for mu}
  \item{sigma.link}{the link function for sigma}
  \item{nu.link}{the link function for nu}
  \item{tau.link}{the link function for tau}
  \item{x}{the x variable}
  \item{q}{vector of quantiles}
  \item{p}{vector of probabilities}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x] otherwise, P[X > x].}
  \item{log.p}{logical; if TRUE, probabilities p are given as log(p).}
  \item{log}{logical; if TRUE, probabilities p are given as log(p).}
  \item{n}{the number of observations}
  \item{mu}{the mu parameter}
  \item{sigma}{the sigma parameter}
  \item{nu}{the nu parameter}
  \item{tau}{the tau parameter}
  \item{type}{permitted values are 2 (Binomial), 3, 4, and 5}
  \item{model}{a gamlss multinomial fitted model}
}
\details{GAMLSS is in general not suitable for multinomial regression. Nevertheless multinomial regression can be fitted within GAMLSS if the response variable y has less than five categories. The function here provide the facilities to do so. The functions \code{MN3()}, \code{MN4()} and \code{MN5()} fit multinomial responses with 3, 4 and 5 categories respectively. 
The function \code{MULTIN()} can be used instead of \code{MN3()}, \code{MN4()} and \code{MN5()} by specifying the number of levels of the response. Note that \code{MULTIN(2)} will produce a binomial fit. 
}
\value{
returns a \code{gamlss.family} object which can be used to fit a binomial distribution in the \code{gamlss()} function.   
}
\references{
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
\author{Mikis Stasinopoulos, Bob Rigby and Vlasios Voudouris}

\seealso{\code{\link{gamlss.family}}, \code{\link{BI}}}
\examples{
 dMN3(3)
 pMN3(2)
 qMN3(.6)
 rMN3(10)
  }
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}% 