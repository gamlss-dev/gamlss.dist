\name{ZAP}
\alias{ZAP}
\alias{dZAP}
\alias{pZAP}
\alias{qZAP}
\alias{rZAP}



\title{Zero adjusted poisson distribution for fitting a GAMLSS model}
\description{
The function \code{ZAP} defines the zero adjusted Poisson distribution, a two parameter distribution, for a \code{gamlss.family} object to be 
used in GAMLSS fitting  using the function \code{gamlss()}. The functions \code{dZAP}, \code{pZAP}, \code{qZAP} and \code{rZAP} define the 
density, distribution function, quantile function 
and random generation for the inflated poisson, \code{ZAP()}, distribution. 
}
\usage{
ZAP(mu.link = "log", sigma.link = "logit")
dZAP(x, mu = 5, sigma = 0.1, log = FALSE)
pZAP(q, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
qZAP(p, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
rZAP(n, mu = 5, sigma = 0.1)
}

\arguments{
  \item{mu.link}{defines the \code{mu.link}, with "log" link as the default for the \code{mu} parameter}
  \item{sigma.link}{defines the \code{sigma.link}, with "logit" link as the default for the sigma parameter which in this case is the probability at zero. 
                   Other links are "probit" and "cloglog"'(complementary log-log)}
  \item{x}{ vector of (non-negative integer)}
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
Details about  the zero adjusted Poison, \code{ZAP} can be found  pp 494-496 of Rigby \emph{et al.} (2019).


}
\value{
The function \code{ZAP} returns a \code{gamlss.family} object which can be used to fit a zero inflated poisson distribution in the \code{gamlss()} function.  
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

(see also \url{https://www.gamlss.com/})..
}
\author{
Mikis Stasinopoulos, Bob Rigby}
%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{gamlss.family}}, \code{\link{PO}}, \code{\link{ZIP}}, \code{\link{ZIP2}}, \code{\link{ZALG}}
}
\examples{
ZAP()
# creating data and plotting them 
dat<-rZAP(1000, mu=5, sigma=.1)
r <- barplot(table(dat), col='lightblue')
}
\keyword{distribution}
\keyword{regression}
