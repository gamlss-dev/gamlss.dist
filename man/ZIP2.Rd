\name{ZIP2}
\alias{ZIP2}
\alias{dZIP2}
\alias{pZIP2}
\alias{qZIP2}
\alias{rZIP2}


\title{Zero inflated poisson distribution for fitting a GAMLSS model }
\description{
The function \code{ZIP2} defines the zero inflated Poisson type 2 distribution, a two parameter distribution, for a \code{gamlss.family} object to be used in GAMLSS fitting 
using the function \code{gamlss()}. The functions \code{dZIP2}, \code{pZIP2}, \code{qZIP2} and \code{rZIP2} define the density, distribution function, quantile function 
and random generation for the inflated poisson, \code{ZIP2()}, distribution. 
The ZIP2 is a different parameterization of the ZIP distribution. In the ZIP2 the \code{mu} is the mean of the distribution.
}
\usage{
ZIP2(mu.link = "log", sigma.link = "logit")
dZIP2(x, mu = 5, sigma = 0.1, log = FALSE)
pZIP2(q, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
qZIP2(p, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
rZIP2(n, mu = 5, sigma = 0.1)
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
  Let \eqn{Y=0} with probability \eqn{\sigma} and \eqn{Y \sim Po(\mu/\left[1-\sigma \right])}{Po(mu/(1-sigma))} 
  with probability \eqn{(1-\sigma)} then Y has
a Zero inflated Poisson type 2 distribution given by

\deqn{f(y|\mu,\sigma)=\sigma +(1-\sigma)e^{-\mu/(1-\sigma)} \hspace{2mm} \mbox{if $y=0$} }{sigma+(1-sigma)e^(-(mu/(1-sigma))) if y=0}
\deqn{f(y|\mu,\sigma)=(1-\sigma)\frac{e^{-\mu/(1-\sigma)} \left[\mu/(1-\sigma)\right]^y}{y!}  \hspace{2mm} \mbox{if $y=1,2,3,\ldots$}}{f(y)=(1-sigma)exp(-(mu/(1-sigma)))* (mu/(1-sigma))^y/y!  if y=0,1,2,...}


The mean of the distribution in this parameterization is \code{mu}.
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
\author{ Bob Rigby, Gillian Heller and Mikis Stasinopoulos }



\seealso{ \code{\link{gamlss.family}}, \code{\link{ZIP}} }
\examples{
ZIP2()# gives information about the default links for the normal distribution
# creating data and plotting them 
dat<-rZIP2(1000, mu=5, sigma=.1)
r <- barplot(table(dat), col='lightblue')
# fit the disteibution
# library(gamlss) 
# mod1<-gamlss(dat~1, family=ZIP2)# fits a constant for mu and sigma 
# fitted(mod1)[1]
# fitted(mod1,"sigma")[1]
}
\keyword{distribution}
\keyword{regression}
