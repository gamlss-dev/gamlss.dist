# ONE PARAMETER PARETO Y>1
# the PARETO one parameter distribution
# y>1 mu>0
# MIKIS STASINOPOULOS
#------------------------------------------------------------
# the fitting function
#-------------------------------------------------------------
PARETO <- function (mu.link = "log") 
{
  mstats <- checklink("mu.link", "PARETO", substitute(mu.link),c("inverse", "log", "sqrt", "identity")) 
  structure(
    list(    family = c("PARETO", "Pareto 1 Y>1"),
         parameters = list(mu = TRUE),
              nopar = 1, 
               type = "Continuous", 
            mu.link = as.character(substitute(mu.link)), 
         mu.linkfun = mstats$linkfun, 
         mu.linkinv = mstats$linkinv, 
              mu.dr = mstats$mu.eta, 
               dldm = function(y,mu)
                 {
                dldm <- 1/mu-log(y) 
               dldm
                },
              d2ldm2 = function(y,mu)
                {
             d2ldm2 <-  -1/mu^2   
             d2ldm2 
                },
         G.dev.incr  = function(y,mu,...) -2*dPARETO(x = y, mu = mu, log = TRUE),
         rqres = expression(rqres(pfun="pPARETO", type="Continuous", ymin=1, y=y, mu=mu)), 
         mu.initial =expression({mu <- rep(2,length(y)) } ),
         mu.valid = function(mu) all(mu > 0), 
         y.valid = function(y)  all(y > 1)
    ),
    class = c("gamlss.family","family"))
}
#-----------------------------------------------------------------------------------------
dPARETO<- function(x, mu = 1, log = FALSE)
{
  if (any(mu <= 0 ) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  #if (any(x <= 1) )  stop(paste("x must be >1", "\n", ""))
     ly <- max(length(x),length(mu)) 
      x <- rep(x, length = ly)      
     mu <- rep(mu, length = ly)   
   logL <- log(mu) -(mu+1) * log(x) #
    lik <- if (log) logL else exp(logL)
    lik <- ifelse(x <= 1, 0, lik)
  as.numeric(lik)
}
#----------------------------------------------------------------------------------------
pPARETO <- function(q, mu = 1, lower.tail = TRUE, log.p = FALSE)
{
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
 # if (any(q <= 1) )  stop(paste("q must be >1", "\n", ""))
  cdf <- 1-q^(-mu)
  if (lower.tail == TRUE) cdf <- cdf  
  else cdf <- 1 - cdf
  if (log.p == FALSE) cdf <- cdf
  else cdf < - log(cdf)
  cdf <-  ifelse(q <= 1, 0, cdf)
  cdf
  }
#----------------------------------------------------------------------------------------
qPARETO  <- function(p, mu = 1, lower.tail = TRUE, log.p = FALSE)
{      
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(p < 0) | any(p > 1.0001))  stop(paste("p must be between 0 and 1", "\n", "")) 
  if (log.p==TRUE) p <- exp(p) else p <- p
  q <- 1/(1-p)^(1/mu)
  q
}
#----------------------------------------------------------------------------------------
rPARETO <- function(n, mu = 1)
{ 
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", "")) 
  n <- ceiling(n)
  p <- runif(n)
  r <- qPARETO(p, mu = mu)
  r
}
#----------------------------------------------------------------------------------------
#----------------------------------------------------------------------------------------