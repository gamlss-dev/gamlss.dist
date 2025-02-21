# MS+PA Wednesday, April 3, 2002 at 09:00
EXP <- function (mu.link ="log") 
{
    mstats <- checklink("mu.link", "Exponential", substitute(mu.link), c("inverse", "log", "sqrt","identity")) 
    structure(
          list(family = c("EXP","Exponential"),
           parameters = list(mu=TRUE), 
                nopar = 1,
                type = "Continuous", 
              mu.link = as.character(substitute(mu.link)),  
           mu.linkfun = mstats$linkfun, 
           mu.linkinv = mstats$linkinv,
                mu.dr = mstats$mu.eta, 
                 dldm = function(y,mu) ((y-mu)/mu^2),
               d2ldm2 = function(mu) (-1/mu^2), 
          G.dev.incr  = function(y,mu,...)  -2*dEXP(x = y, mu = mu, log = TRUE) , 
                rqres = expression(rqres(pfun="pEXP", type="Continuous", y=y, mu=mu)), 
           mu.initial = expression(mu <- (y+mean(y))/2),
             mu.valid = function(mu) all(mu > 0) ,  
              y.valid = function(y) all(y > 0),
                 mean = function(mu) mu,
             variance = function(mu) mu^2
          ),
            class = c("gamlss.family","family"))
}
#----------------------------------------------------------------------------------------
dEXP<-function(x, mu = 1, log = FALSE)
 { 
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
        ly <- max(length(x),length(mu)) 
         x <- rep(x, length = ly) 
        mu <- rep(mu, length = ly) 
        fy <- dexp(x = x, rate =1/mu, log = log)
        fy[x <= 0] <- 0 
        fy[x >= Inf] <- 0
        fy
  }
#----------------------------------------------------------------------------------------
pEXP <- function(q, mu = 1, lower.tail = TRUE, log.p = FALSE)
  {     
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
         ly <- max(length(q),length(mu)) 
          q <- rep(q, length = ly) 
         mu <- rep(mu, length = ly)      
        cdf <- pexp(q, rate =1/mu, lower.tail = lower.tail, log.p = log.p)
     cdf[q<=0] <- 0
   cdf[q>=Inf] <- 1
          cdf
   }
#----------------------------------------------------------------------------------------
qEXP <- function(p, mu = 1, lower.tail = TRUE, log.p = FALSE)

  {      
if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (log.p==TRUE) p <- exp(p) 
          q <- qexp(p, rate = 1/mu, lower.tail = lower.tail)
          q[p == 0] <- 0
          q[p == 1] <- Inf
          q[p <  0] <- NaN
          q[p >  1] <- NaN
          return(q)    
   }
#----------------------------------------------------------------------------------------
rEXP <- function(n, mu = 1)
  { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
          if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          r <- rexp(n, rate =1/mu)
          r
  }
#----------------------------------------------------------------------------------------
