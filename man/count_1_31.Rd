\name{count_1_31}
\alias{count_1_31}
\alias{count_1_22}
\alias{count_2_32}
\alias{count_2_32R}
\alias{count_2_33}
\alias{count_3_32}
\alias{count_3_33}
\alias{binom_1_31}
\alias{binom_2_33}
\alias{binom_3_33}
\alias{contR_2_12}
\alias{contR_3_11}
\alias{contR_4_13}
\alias{contRplus_2_11}
\alias{contRplus_3_13}
\alias{contRplus_4_33}
\alias{contR01_2_13}
\alias{contR01_4_33}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A set of functions to plot gamlss.family distributions
}
\description{
Those functions are used in the distribution book of gamlss, see Rigby et. al 2019.
}
\usage{

binom_1_31(family = BI, mu = c(0.1, 0.5, 0.7), bd = NULL, miny = 0,
           maxy = 20, cex.axis = 1.2, cex.all = 1.5)
           
binom_2_33(family = BB, mu = c(0.1, 0.5, 0.8), sigma = c(0.5, 1, 2), 
           bd = NULL, miny = 0, maxy = 10, cex.y.axis = 1.5, 
           cex.all = 1.5)  
           
binom_3_33(family = ZIBB, mu = c(0.1, 0.5, 0.8), sigma = c(0.5, 1, 2),
           nu = c(0.01, 0.3), bd = NULL, miny = 0, maxy = 10, 
           cex.axis = 1.5, cex.all = 1.5, cols = c("darkgray", "black"), 
           spacing = 0.3, legend.cex=1, legend.x="topright",
          legend.where=c("left","right", "center"))

contR_2_12(family = "NO", mu = c(0, -1, 1), sigma = c(1, 0.5, 2), 
          cols=c(gray(.1),gray(.2),gray(.3)), 
          ltype = c(1, 2, 3), maxy = 7, 
          no.points = 201, y.axis.lim = 1.1,  
          cex.axis = 1.5, cex.all = 1.5,
          legend.cex=1, legend.x="topleft" )
          
contR_3_11(family = "PE", mu = 0, sigma = 1, nu = c(1, 2, 3), 
          cols=c(gray(.1),gray(.2),gray(.3)), maxy = 7, no.points = 201, 
          ltype = c(1, 2, 3), y.axis.lim = 1.1, cex.axis = 1.5, 
          cex.all = 1.5, legend.cex=1, legend.x="topleft")          

contR_4_13(family = "SEP3", mu = 0, sigma = 1, nu = c(0.5, 1, 2), 
         tau = c(1, 2, 5), cols=c(gray(.1),gray(.2),gray(.3)), maxy = 7, 
         no.points = 201, ltype = c(1, 2, 3), 
         y.axis.lim = 1.1, cex.axis = 1.5, cex.all = 1.5,
         legend.cex=1, legend.x="topleft", 
          legend.where=c("left","right"))
         
contRplus_2_11(family = GA, mu = 1, sigma = c(0.1, 0.6, 1), 
          cols=c(gray(.1),gray(.2),gray(.3)), 
          maxy = 4, no.points = 201, 
          y.axis.lim = 1.1, ltype = c(1, 2, 3),
           cex.axis = 1.5, cex.all = 1.5,
          legend.cex=1, legend.x="topright")         

contRplus_3_13(family = "BCCG", mu = 1, sigma = c(0.15, 0.2, 0.5), 
          nu = c(-2, 0, 4), 
          cols=c(gray(.1),gray(.2),gray(.3)), 
          maxy = 4, ltype = c(1, 2, 3), 
          no.points = 201, y.axis.lim = 1.1,
          cex.axis = 1.5, cex.all = 1.5,
          legend.cex=1, legend.x="topright",
          legend.where=c("left","right"))
          
contRplus_4_33(family = BCT, mu = 1, sigma = c(0.15, 0.2, 0.5), 
          nu = c(-4, 0, 2), tau = c(100, 5, 1), 
          cols=c(gray(.1),gray(.2),gray(.3)), 
          maxy = 4, ltype = c(1, 2, 3), 
          no.points = 201, y.axis.lim = 1.1, 
          cex.axis = 1.5, cex.all = 1.5,
          legend.cex=1, legend.x="topright",
          legend.where=c("left","right"))

contR01_2_13(family = "BE", mu = c(0.2, 0.5, 0.8), sigma = c(0.2, 0.5, 0.8), 
            cols=c(gray(.1),gray(.2),gray(.3)), 
            ltype = c(1, 2, 3), maxy = 7, no.points = 201, 
            y.axis.lim = 1.1, maxYlim = 10,  
            cex.axis = 1.5, cex.all = 1.5,
            legend.cex=1, legend.x="topright",
            legend.where=c("left","right", "center"))

contR01_4_33(family = GB1, mu = c(0.5), sigma = c(0.2, 0.5, 0.7),
            nu = c(1, 2, 5), tau = c(0.5, 1, 2), 
             cols=c(gray(.1),gray(.2),gray(.3)),  
             maxy = 0.999, ltype = c(1, 2, 3), 
             no.points = 201, y.axis.lim = 1.1, 
             maxYlim = 10,cex.axis = 1.5, cex.all = 1.5,
              legend.cex=1, legend.x="topright",
              legend.where=c("left","right", "center"))

count_1_31(family = PO, mu = c(1, 2, 5), miny = 0, maxy = 10, 
           cex.y.axis = 1.2, cex.all = 1.5)
           
count_1_22(family = PO, mu = c(1, 2, 5, 10), miny = 0, 
           maxy = 20, cex.y.axis = 1.2, cex.all = 1.5) 
           
count_2_32(family = NBI, mu = c(0.5, 1, 5), sigma = c(0.1, 2), 
           miny = 0, maxy = 10, cex.y.axis = 1.5, cex.all = 1.5)

count_2_32R(family = NBI, mu = c(1, 2), sigma = c(0.1, 1, 2), 
           miny = 0, maxy = 10, cex.y.axis = 1.5, cex.all = 1.5) 
           
count_2_33(family = NBI, mu = c(0.1, 1, 2), sigma = c(0.5, 1, 2), 
          miny = 0, maxy = 10, cex.y.axis = 1.5, cex.all = 1.5) 
          
count_3_32(family = SICHEL, mu = c(1, 5, 10), sigma = c(0.5, 1), 
           nu = c(-0.5, 0.5), miny = 0, maxy = 10, cex.axis = 1.5, 
           cex.all = 1.5, cols = c("darkgray", "black"), spacing = 0.2,
           legend.cex=1, legend.x="topright",
                  legend.where=c("left","right"))          
           
count_3_33(family = SICHEL, mu = c(1, 5, 10), sigma = c(0.5, 1, 2), 
           nu = c(-0.5, 0.5, 1), miny = 0, maxy = 10, cex.y.axis = 1.5,
           cex.all = 1.5, cols = c("darkgray", "black"), spacing = 0.3,
           legend.cex=1, legend.x="topright",
           legend.where=c("left","right", "center"))

}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{family}{a gamlss family distribution}
  \item{mu}{the \code{mu} parameter values}
  \item{sigma}{The \code{sigma} parameter values}
  \item{nu}{the \code{nu} parameter values}
  \item{tau}{the \code{tau} parameter values}
  \item{bd}{the binomial denominator}
  \item{miny}{minimal value for the y axis}
  \item{maxy}{maximal value for the y axis}
  \item{cex.axis}{the size of the letters in the two axes}
  \item{cex.y.axis}{the size of the letters in the y axe}
  \item{cex.all}{the overall size of all plotting characters}
  \item{cols}{colours}
  \item{spacing}{spacing between plots}
  \item{ltype}{The type of lines used}
  \item{no.points}{the number of points in the curve}
  \item{y.axis.lim}{the maximum value for the y axis}
  \item{maxYlim}{the maximum permissible value for Y}
  \item{legend.cex}{the size of the legend}
  \item{legend.x}{where in the figure to put the legend}
  \item{legend.where}{where in the whole plot to put the legend}
  }
\details{
Th function plot different types of continuous and discrete distributions:
i) \code{contR}: continuous distribution defined on minus infinity to plus infinity,
ii) \code{contRplus}: continuous distribution defined from  zero to plus infinity,
iii) \code{contR01}: continuous distribution defined from  zero to 1,
iv) \code{bimom} binomial type discrete distributions,
v) \code{count} count type discrete distributions.


The first number after the first underline in the name of the function indicates the number of parameters in the distribution. The two numbers after the second underline indicate how may rows and columns are in the plot. 
}
\value{
The result is a plot
}
\references{
Rigby, R. A. and  Stasinopoulos D. M. (2005). Generalized additive models for location, scale and shape,(with discussion), 
\emph{Appl. Statist.}, \bold{54}, part 3, pp 507-554.

Rigby, R. A., Stasinopoulos, D. M.,  Heller, G. Z.,  and De Bastiani, F. (2019)
	\emph{Distributions for modeling location, scale, and shape: Using GAMLSS in R}, Chapman and Hall/CRC, \doi{10.1201/9780429298547}. An older version can be found in \url{https://www.gamlss.com/}.

Stasinopoulos D. M. Rigby R.A. (2007) Generalized additive models for location scale and shape (GAMLSS) in R.
\emph{Journal of Statistical Software}, Vol. \bold{23}, Issue 7, Dec 2007, \doi{10.18637/jss.v023.i07}.

Stasinopoulos D. M., Rigby R.A., Heller G., Voudouris V., and De Bastiani F., (2017)
\emph{Flexible Regression and Smoothing: Using GAMLSS in R},  Chapman and Hall/CRC.
\doi{10.1201/b21973}

(see also  \url{https://www.gamlss.com/}).
}
\author{
Mikis Stasinopoulos, Robert Rigby, Gillian Heller, Fernada  De Bastiani }
\note{more notes}

%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
 \code{\link{gamlss.family}}
}
\examples{
count_1_31()
}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{distribution}
\keyword{regression}
