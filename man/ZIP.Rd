\name{ZIP}
\alias{ZIP}
\alias{dZIP}
\alias{pZIP}
\alias{qZIP}
\alias{rZIP}


\title{Zero inflated poisson distribution for fitting a GAMLSS model }
\description{
The function \code{ZIP} defines the zero inflated Poisson distribution, a two parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}. The functions \code{dZIP}, \code{pZIP}, \code{qZIP} and \code{rZIP} define the density, distribution function, quantile function 
and random generation for the inflated poisson, \code{ZIP()}, distribution. 
}
\usage{
ZIP(mu.link = "log", sigma.link = "logit")
dZIP(x, mu = 5, sigma = 0.1, log = FALSE)
pZIP(q, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
qZIP(p, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
rZIP(n, mu = 5, sigma = 0.1)
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
  Let \eqn{Y=0} with probability \eqn{\sigma} and \eqn{Y \sim Po(\mu)} with probability \eqn{(1-\sigma)} the Y has
a Zero inflated Poisson Distribution given by

 \deqn{f(y)=\sigma +(1-\sigma)e^{-\mu}}{sigma+(1-sigma)e^(-mu)} if (y=0)
 \deqn{f(y)=(1-\sigma)\frac{e^{-\mu} \mu^y}{y!}}{f(y)=(1-sigma)e^-mu mu^y/y!} if (y>0)
for \eqn{y=0,1,...,}.
}
\value{
  returns a \code{gamlss.family} object which can be used to fit a zero inflated poisson distribution in the \code{gamlss()} function.  
}
\references{ 
Lambert, D. (1992), Zero-inflated Poisson Regression with an application to defects in Manufacturing,
    \emph{Technometrics},  \bold{34}, pp 1-14.

Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).  

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{ Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk}, Bob Rigby \email{r.rigby@londonmet.ac.uk}}

\seealso{ \code{\link{gamlss.family}}, \code{\link{PO}}, \code{\link{ZIP2}} }
\examples{
ZIP()# gives information about the default links for the normal distribution
# creating data and plotting them 
dat<-rZIP(1000, mu=5, sigma=.1)
r <- barplot(table(dat), col='lightblue')
# library(gamlss)
# fit the distribution 
# mod1<-gamlss(dat~1, family=ZIP)# fits a constant for mu and sigma 
# fitted(mod1)[1]
# fitted(mod1,"sigma")[1]
}
\keyword{distribution}
\keyword{regression}
