\name{gen.Family}
\alias{gen.Family}
\alias{Family}
\alias{Family.d}
\alias{Family.p}
\alias{Family.q}
\alias{Family.r}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Functions to generate log and logit distributions from existing continuous gamlss.family distributions
}
\description{
There are five functions here. Only the functions \code{Family} and \code{gen.Family} should be used (see details). 
}
\usage{
Family.d(family = "NO", type = c("log", "logit"), ...)
Family.p(family = "NO", type = c("log", "logit"), ...)
Family.q(family = "NO", type = c("log", "logit"), ...)
Family.r(family = "NO", type = c("log", "logit"), ...)
Family(family = "NO", type = c("log", "logit"), local = TRUE, ...)
gen.Family(family = "NO", type = c("log", "logit"), ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{family}{a continuous \code{gamlss.family} distribution}
  \item{type}{the type of transformation only "log" and "logit" are allowed}
  \item{local}{It is TRUE if is called within \code{gamlss()} otherwise is FALSE}
  \item{\dots}{for passing extra arguments}
}
\details{
The function \code{gen.Family} creates the standard \code{d,p,q,r} functions for the distribution plus the fitting \code{gamlss.family}. For example \code{gen.Family("NO", "logit")} will generate the functions
\code{dlogitNO()}, \code{plogitNO()} , \code{qlogitNO()} , \code{rlogitNO()}  and \code{dlogitNO()}.
The latest function can be used in \code{family}  argument of  \code{gamlss()} to fit a logic-Normal distribution i.e. \code{family=logitNO}.
The same fitting can be achieved by using \code{family=Family("NO", "logit")}. Here the required \code{dlogitNO()}, \code{plogitNO()} and 
\code{logitNO()} functions are generated locally within the \code{gamlss()} environment. 
}
\value{
 The function \code{gen.Family} returns  the \code{d},  \code{p}, \code{q}  \code{r} functions plus the fitting function.  
}
\references{
 Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Stasinopoulos D. M., Rigby R.A. and Akantziliotou C. (2006) Instructions on how to use the GAMLSS package in R.
Accompanying documentation in the current GAMLSS  help files, (see also  \url{http://www.gamlss.org/}).

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \url{http://www.jstatsoft.org/v23/i07}.
}
\author{
Mikis Stasinopoulos \email{d.stasinopoulos@londonmet.ac.uk} and Bob Rigby \email{r.rigby@londonmet.ac.uk}
}


\examples{
# generating a log t distribution
gen.Family("TF")
# plotting the d, p, q, and r functions
op<-par(mfrow=c(2,2))
curve(dlogTF(x, mu=0), 0, 10)
curve(plogTF(x, mu=0), 0, 10)
curve(qlogTF(x, mu=0), 0, 1)
Y<- rlogTF(200)
hist(Y)
par(op)


# different mu
curve(dlogTF(x, mu=-1, sigma=1, nu=10), 0, 5, ylim=c(0,1))
curve(dlogTF(x, mu=0, sigma=1, nu=10), 0,  5, add=TRUE, col="red", lty=2)
curve(dlogTF(x, mu=1, sigma=1, nu=10), 0, 5, add=TRUE, col="blue", lty=3)

# different sigma
curve(dlogTF(x, mu=0, sigma=.5, nu=10), 0, 5, ylim=c(0,1))
curve(dlogTF(x, mu=0, sigma=1, nu=10), 0, 5, add=TRUE, col="red", lty=2)
curve(dlogTF(x, mu=0, sigma=2, nu=10), 0, 5, add=TRUE, col="blue", lty=3)

# different degrees of freedom nu
curve(dlogTF(x,  mu=0, sigma=1, nu=1), 0, 5, ylim=c(0,.8), n = 1001)
curve(dlogTF(x,  mu=0, sigma=1, nu=2), 0, 5, add=TRUE, col="red", lty=2)
curve(dlogTF(x,  mu=0, sigma=1, nu=5), 0, 5, add=TRUE, col="blue", lty=3)


# generating a logit t distribution
gen.Family("TF", "logit")
# plotting the d, p, q, and r functions
op<-par(mfrow=c(2,2))
curve(dlogitTF(x, mu=0), 0, 1)
curve(plogitTF(x, mu=0), 0, 1)
curve(qlogitTF(x, mu=0), 0, 1)
abline(v=1)
Y<- rlogitTF(200)
hist(Y)
par(op)


# different mu
curve(dlogitTF(x, mu=-2, sigma=1, nu=10), 0, 1, ylim=c(0,5))
curve(dlogitTF(x, mu=0, sigma=1, nu=10), 0, 1, add=TRUE, col="red", lty=2)
curve(dlogitTF(x, mu=2, sigma=1, nu=10), 0, 1, add=TRUE, col="blue", lty=3)

# different sigma
curve(dlogitTF(x, mu=0, sigma=1, nu=10), 0, 1, ylim=c(0,2.5))
curve(dlogitTF(x, mu=0, sigma=2, nu=10), 0, 1, add=TRUE, col="red", lty=2)
curve(dlogitTF(x, mu=0, sigma=.7, nu=10), 0, 1, add=TRUE, col="blue", lty=3)

# different degrees of freedom nu
curve(dlogitTF(x,  mu=0, sigma=1, nu=1), 0, 1, ylim=c(0,1.6))
curve(dlogitTF(x,  mu=0, sigma=1, nu=2), 0, 1, add=TRUE, col="red", lty=2)
curve(dlogitTF(x,  mu=0, sigma=1, nu=5), 0, 1, add=TRUE, col="blue", lty=3)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}