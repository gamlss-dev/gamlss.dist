\name{ZABB}
\alias{ZABB}
\alias{dZABB}
\alias{pZABB}
\alias{qZABB}
\alias{rZABB}

\alias{ZIBB}
\alias{dZIBB}
\alias{pZIBB}
\alias{qZIBB}
\alias{rZIBB}


\title{
Zero inflated and zero adjusted Binomial distribution for fitting in GAMLSS
}
\description{
The function \code{ZIBB} defines the zero inflated  beta binomial distribution, a three  parameter distribution, 
for a  \code{gamlss.family} object to be used in GAMLSS fitting  using the function \code{gamlss()}. 
The functions \code{dZIBB}, \code{pZIBB}, \code{qZIBB} and \code{rZINN} define the 
density, distribution function, quantile function 
and random generation for the zero inflated beta binomial, \code{ZIBB}, distribution. 

The function \code{ZABB} defines the zero adjusted beta binomial distribution, a three  parameter distribution, for a 
\code{gamlss.family} object to be used in GAMLSS fitting  using the function \code{gamlss()}. The functions \code{dZABB}, \code{pZABB}, 
\code{qZABB} and \code{rZABB} define the 
density, distribution function, quantile function 
and random generation for the zero inflated beta binomial, \code{ZABB()}, distribution.
 }

\usage{
ZABB(mu.link = "logit", sigma.link = "log", nu.link = "logit")
ZIBB(mu.link = "logit", sigma.link = "log", nu.link = "logit")

dZIBB(x, mu = 0.5, sigma = 0.5, nu = 0.1, bd = 1, log = FALSE)
dZABB(x, mu = 0.5, sigma = 0.1, nu = 0.1, bd = 1, log = FALSE)

pZIBB(q, mu = 0.5, sigma = 0.5, nu = 0.1, bd = 1, lower.tail = TRUE, log.p = FALSE)
pZABB(q, mu = 0.5, sigma = 0.1, nu = 0.1, bd = 1, lower.tail = TRUE, log.p = FALSE)

qZIBB(p, mu = 0.5, sigma = 0.5, nu = 0.1, bd = 1, lower.tail = TRUE, log.p = FALSE)
qZABB(p, mu = 0.5, sigma = 0.1, nu = 0.1, bd = 1, lower.tail = TRUE, log.p = FALSE)

rZIBB(n, mu = 0.5, sigma = 0.5, nu = 0.1, bd = 1)
rZABB(n, mu = 0.5, sigma = 0.1, nu = 0.1, bd = 1)
}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "logit" link as the default for the \code{mu} parameter.  
                 Other links are "probit" and "cloglog"'(complementary log-log)}
   \item{sigma.link}{Defines the \code{sigma.link}, with "log" link as the default for the \code{sigma} parameter.}  
   \item{nu.link}{Defines the \code{sigma.link}, with "logit" link as the default for the \code{mu} parameter.  
                 Other links are "probit" and "cloglog"'(complementary log-log)}             
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive probabilities}
  \item{sigma}{vector of positive dispertion parameter}
  \item{nu}{vector of positive probabilities}
  \item{bd}{vector of binomial denominators}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
}
\details{
The  definition of the zero adjusted beta binomial distribution, \code{ZABB} and  the the zero inflated beta binomial  distribution, \code{ZIBB}, are given in p. 527 and p. 528 of  of Rigby \emph{et al.} (2019), respectively.
}
\value{The functions \code{ZIBB} and \code{ZABB} return a \code{gamlss.family} object which can be used to fit a
zero inflated or zero adjusted beta binomial distribution respectively in the \code{gamlss()} function.   }


\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	Distributions for modeling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC. An older version can be found in \url{https://www.gamlss.com/}.


Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}..

 
Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  
Chapman and Hall/CRC. \doi{10.1201/b21973}  

(see also \url{https://www.gamlss.com/}).
}

\author{Mikis Stasinopoulos, Bob Rigby}




\seealso{\code{\link{gamlss.family}}, \code{\link{NBI}}, \code{\link{NBII}}}
\examples{
ZIBB() 
ZABB()
# creating data and plotting them 
 dat <- rZIBB(1000, mu=.5, sigma=.5, nu=0.1, bd=10)
   r <- barplot(table(dat), col='lightblue')
dat1 <- rZABB(1000, mu=.5, sigma=.2, nu=0.1, bd=10)
   r1 <- barplot(table(dat1), col='lightblue')
}

\keyword{distribution}
\keyword{regression}
