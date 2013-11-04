\name{ZABI}
\alias{ZABI}
\alias{dZABI}
\alias{pZABI}
\alias{qZABI}
\alias{rZABI}
\alias{ZIBI}
\alias{dZIBI}
\alias{pZIBI}
\alias{qZIBI}
\alias{rZIBI}


%- Also NEED an '\alias' for EACH other topic documented here.
\title{Zero inflated and zero adjusted Binomial distribution for fitting in GAMLSS}
\description{
The \code{ZABI()} function defines the zero adjusted binomial distribution, a two parameter  distribution, 
for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dZABI}, \code{pZABI}, \code{qZABI} and \code{rZABI} define the density, distribution function, quantile function and random
generation for the zero adjusted binomial, \code{ZABI()}, distribution. 

The \code{ZIBI()} function defines the zero inflated binomial distribution, a two parameter  distribution, 
for a \code{gamlss.family} object to be used 
in GAMLSS fitting using the function \code{gamlss()}.    
The functions \code{dZIBI}, \code{pZIBI}, \code{qZIBI} and \code{rZIBI} define the density, distribution function, quantile function and random
generation for the zero inflated binomial, \code{ZIBI()}, distribution. 
}
\usage{
 ZABI(mu.link = "logit", sigma.link = "logit")
dZABI(x, bd = 1, mu = 0.5, sigma = 0.1, log = FALSE)
pZABI(q, bd = 1, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
qZABI(p, bd = 1, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
rZABI(n, bd = 1, mu = 0.5, sigma = 0.1)

 ZIBI(mu.link = "logit", sigma.link = "logit")
dZIBI(x, bd = 1, mu = 0.5, sigma = 0.1, log = FALSE)
pZIBI(q, bd = 1, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
qZIBI(p, bd = 1, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
rZIBI(n, bd = 1, mu = 0.5, sigma = 0.1)

}

\arguments{
  \item{mu.link}{ Defines the \code{mu.link}, with "logit" link as the default for the \code{mu} parameter.  
                 Other links are "probit" and "cloglog"'(complementary log-log)}
   \item{sigma.link}{ Defines the \code{sigma.link}, with "logit" link as the default for the \code{mu} parameter.  
                 Other links are "probit" and "cloglog"'(complementary log-log)}               
  \item{x}{ vector of (non-negative integer) quantiles}
  \item{mu}{vector of positive probabilities}
  \item{sigma}{vector of positive probabilities}
  \item{bd}{vector of binomial denominators}
  \item{p}{vector of probabilities}
  \item{q}{vector of quantiles}
  \item{n}{number of random values to return}
  \item{log, log.p}{logical; if TRUE, probabilities p are given as log(p)}
  \item{lower.tail}{logical; if TRUE (default), probabilities are P[X <= x],
          otherwise, P[X > x]}  
}

\details{
For the definition of the distributions see   Rigby and Stasinopoulos (2010) below.
}



\value{
 The functions \code{ZABI} and \code{ZIBI} return a \code{gamlss.family} object which 
 can be used to fit a binomial distribution in the \code{gamlss()} function. 
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.com/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.

Rigby, R. A.  and Stasinopoulos D. M. (2010) The gamlss.family distributions, (distributed with this package or see 
\url{http://www.gamlss.com/}) 
}
\author{Mikis Stasinopoulos, Bob Rigby}
\note{
The response variable should be a matrix containing two columns, the first with the count of successes and the 
second with the count of failures.
}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{\code{\link{gamlss.family}}, \code{\link{BI}}}

\examples{
ZABI() 
curve(dZABI(x, mu = .5, bd=10), from=0, to=10, n=10+1, type="h")
tN <- table(Ni <- rZABI(1000, mu=.2, sigma=.3, bd=10))
r <- barplot(tN, col='lightblue')

ZIBI() 
curve(dZIBI(x, mu = .5, bd=10), from=0, to=10, n=10+1, type="h")
tN <- table(Ni <- rZIBI(1000, mu=.2, sigma=.3, bd=10))
r <- barplot(tN, col='lightblue')
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}% 
