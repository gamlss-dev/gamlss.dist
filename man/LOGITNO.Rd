\name{LOGITNO}
\alias{LOGITNO}
\alias{dLOGITNO}
\alias{pLOGITNO}
\alias{qLOGITNO}
\alias{rLOGITNO}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Logit Normal distribution for fitting in GAMLSS }
\description{
The functions \code{dLOGITNO}, \code{pLOGITNO}, \code{qLOGITNO} and \code{rLOGITNO} define the density, distribution function, quantile function and random
generation for the logit-normal distribution.
The function \code{LOGITNO} can be used for fitting the distribution in \code{gamlss()}.
}
\usage{
LOGITNO(mu.link = "logit", sigma.link = "log")
dLOGITNO(x, mu = 0.5, sigma = 1, log = FALSE)
pLOGITNO(q, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE)
qLOGITNO(p, mu = 0.5, sigma = 1, lower.tail = TRUE, log.p = FALSE)
rLOGITNO(n, mu = 0.5, sigma = 1)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{mu.link}{the link function for mu}
  \item{sigma.link}{the link function for sigma}
 \item{x,q}{vector of quantiles}
  \item{mu}{vector of location parameter values}
  \item{sigma}{vector of scale parameter values}
  \item{log, log.p}{ logical; if TRUE, probabilities p are given as log(p).}
   \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x] }
  \item{p}{vector of probabilities. }
  \item{n}{ number of observations. If \code{length(n) > 1}, the length is
          taken to be the number required}
}
\details{
  The probability density function in \code{LOGITNO} is defined as  
\deqn{f(y|\mu,\sigma)=\frac{1}{y (1-y) \sqrt{2\pi}\sigma} \exp [-\frac{1}{2 \sigma^2}(log(y/(1-y))-log(\mu/(1-\mu))^2 ] }{f(y|mu,sigma)=(1/(y*(1-y)*sqrt(2*pi)*sigma))*exp(-0.5*((log(y/(1-y))-log(mu/(1-mu))/(sigma))^2)}  
for \eqn{0<y>1}, \eqn{\mu=(0, 1}{mu=(0,1)} and \eqn{\sigma>0}.
}
\value{
\code{LOGITNO()} returns a \code{gamlss.family} object which can be used to fit a logit-normal distribution in the \code{gamlss()} function.
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{Mikis Stasinopoulos, Bob Rigby}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gamlss.family}}, \code{\link{LOGNO}}
}
\examples{
# plotting the d, p, q, and r functions
op<-par(mfrow=c(2,2))
curve(dLOGITNO(x), 0, 1)
curve(pLOGITNO(x), 0, 1)
curve(qLOGITNO(x), 0, 1)
Y<- rLOGITNO(200)
hist(Y)
par(op)

# plotting the d, p, q, and r functions
# sigma 3
op<-par(mfrow=c(2,2))
curve(dLOGITNO(x, sigma=3), 0, 1)
curve(pLOGITNO(x, sigma=3), 0, 1)
curve(qLOGITNO(x, sigma=3), 0, 1)
Y<- rLOGITNO(200, sigma=3)
hist(Y)
par(op)
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}%
