\name{hazardFun}
\alias{hazardFun}
\alias{gen.hazard}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{Hazard functions for gamlss.family distributions}
\description{The function \code{hazardFun()} takes as an argument a \code{gamlss.family} object and creates the hazard function for it. The function \code{gen.hazard()} generates a hazard function called \code{hNAME} where \code{NAME} is a 
\code{gamlss.family} i.e. \code{hGA()}.
}
\usage{
hazardFun(family = "NO", ...)
gen.hazard(family = "NO", ...)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{family}{a \code{gamlss.family} object}
  \item{\dots}{for passing extra arguments}
}

\value{
A hazard function. 
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	Distributions for modeling location, scale, and shape: Using GAMLSS in R, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}. 

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC. 
\doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopoulos, Bob Rigby and Vlasios Voudouris}


\seealso{\code{\link{gamlss.family}}}
\examples{
gen.hazard("WEI2")
y<-seq(0,10,by=0.01)
plot(hWEI2(y, mu=1, sigma=1)~y, type="l", col="black", ylab="h(y)", ylim=c(0,2.5))
lines(hWEI2(y, mu=1, sigma=1.2)~y, col="red",lt=2,lw=2)
lines(hWEI2(y, mu=1, sigma=.5)~y, col="blue",lt=3,lw=2)

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}% 
