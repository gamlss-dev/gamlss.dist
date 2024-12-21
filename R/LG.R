################################################################################
################################################################################
################################################################################
################################################################################
# I think this is working correctly 01/03/10
LG <- function (mu.link = "logit") 
{
    mstats <- checklink("mu.link", "LG", substitute(mu.link),c("logit", "probit", "cloglog", "cauchit", "log", "own"))
    structure(
          list(family = c("LG", "Logarithmic"),
           parameters = list(mu = TRUE), # the mean
                nopar = 1, 
                 type = "Discrete", 
              mu.link = as.character(substitute(mu.link)), 
           mu.linkfun = mstats$linkfun, 
           mu.linkinv = mstats$linkinv, 
                mu.dr = mstats$mu.eta, 
                 dldm = function(y,mu) (y/mu)+1/((1-mu)*log(1-mu)),
               d2ldm2 = function(y,mu) 
                        {
                         dldm <- (y/mu)+1/((1-mu)*log(1-mu))
                       d2ldm2 <- -dldm^2
                       d2ldm2
                        },
          G.dev.incr  = function(y,mu,...) -2*dLG(x = y, mu = mu, log = TRUE),
                rqres = expression(rqres(pfun="pLG", type="Discrete", ymin=1, y=y, mu=mu)), 
            mu.initial =expression({mu <- 0.9 } ),
              mu.valid = function(mu) all(mu > 0  & mu < 1), 
               y.valid = function(y)  all(y > 0),
                  mean = function(mu) -(log(1-mu))^-1 * mu * (1-mu)^-1,
              variance = function(mu) -(log(1-mu))^-1 * mu * (1+(log(1-mu))^-1 * mu) * (1-mu)^-2
          
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dLG<-function(x, mu = 0.5, log = FALSE)
 { 
          if (any(mu <= 0) | any(mu >= 1) )  stop(paste("mu must be greater than 0 and less than 1", "\n", ""))
       #   if (any(x <= 0) )  stop(paste("x must be >0", "\n", ""))
          ly <- max(length(x),length(mu)) 
           xx <- rep(x, length = ly) 
         xx[x <= 0] <- 1 
           mu <- rep(mu, length = ly)   
        logfy <- xx*log(mu)-log(xx)-log(-log(1-mu))
  if(log == FALSE) fy <- exp(logfy) else fy <- logfy
  fy[x <= 0] <- 0
  fy[x==Inf] <- 0
       fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pLG <- function(q, mu = 0.5, lower.tail = TRUE, log.p = FALSE)
  {     
          if (any(mu <= 0) | any(mu >= 1) )  stop(paste("mu must be greater than 0 and less than 1", "\n", ""))
 #  if (any(q <= 0) )  stop(paste("q must be >0", "\n", "")) 
        ly <- max(length(q),length(mu)) 
        qq <- rep(q, length = ly)      
        mu <- rep(mu, length = ly)   
        qq[q==Inf] <- 1 
       fn <- function(q, mu) sum(dLG(1:qq, mu=mu))
     Vcdf <- Vectorize(fn)
      cdf <- Vcdf(q=q, mu=mu)   
      cdf <- if(lower.tail==TRUE) cdf else 1-cdf
      cdf <- if(log.p==FALSE) cdf else log(cdf)    
      cdf[q<1] <- 0 
      cdf[q==Inf] <- 1
      cdf
  }
################################################################################
################################################################################
################################################################################
################################################################################
qLG <- function(p, mu=0.5,  lower.tail = TRUE, log.p = FALSE,  
                 max.value = 10000)
  {      
          if (any(mu <= 0) | any(mu >= 1) )  stop(paste("mu must be greater than 0 and less than 1", "\n", "")) 
# if (any(p < 0) | any(p > 1.0001))  stop(paste("p must be between 0 and 1", "\n", "")) 
          if (log.p==TRUE) p <- exp(p) else p <- p
          if (lower.tail==TRUE) p <- p else p <- 1-p    
           ly <- length(p)                                                       
          QQQ <- rep(0,ly)                         
          nmu <- rep(mu, length = ly)                   
       for (i in seq(along=p))                                                          
      {
       cumpro <- 0                                                                         
     if (p[i]+0.000000001 >= 1) QQQ[i] <- Inf
     else  
        {  
            for (j in seq(from = 1, to = max.value))
            {
            cumpro <-  pLG(j, mu = nmu[i], log.p = FALSE) 
           QQQ[i] <- j 
       if  (p[i] <= cumpro ) break 
            } 
        }
      }          
          QQQ[p == 0] <- 0
          QQQ[p == 1] <- Inf
          QQQ[p <  0] <- NaN
          QQQ[p >  1] <- NaN
          return(QQQ)
   } 
################################################################################
################################################################################
################################################################################
################################################################################
rLG <- function(n, mu = 0.5)
  { 
          if (any(mu <= 0) | any(mu >= 1) )  stop(paste("mu must be greater than 0 and less than 1", "\n", "")) 
          if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qLG(p, mu=mu)
          as.integer(r)
  }
################################################################################
################################################################################
################################################################################
################################################################################
