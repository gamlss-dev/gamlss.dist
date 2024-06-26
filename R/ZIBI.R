################################################################################
################################################################################
################################################################################
################################################################################
## 05_05_10
# This appears to be working
#ZIBI
ZIBI <- function (mu.link = "logit", sigma.link = "logit")
{
    mstats <- checklink("mu.link", "ZIBI", substitute(mu.link),    
                           c("logit", "probit", "cloglog", "cauchit", "log", "own"))   
    dstats <- checklink("sigma.link", "ZIBI", substitute(sigma.link), 
                           c("logit", "probit", "cloglog", "cauchit", "log", "own"))   
    structure(
          list(family = c("ZIBI", "Binomial Zero Inflated"),
           parameters = list(mu=TRUE, sigma=TRUE), 
                nopar = 2, 
                 type = "Discrete",
              mu.link = as.character(substitute(mu.link)),  
           sigma.link = as.character(substitute(sigma.link)), 
           mu.linkfun = mstats$linkfun, 
        sigma.linkfun = dstats$linkfun, 
           mu.linkinv = mstats$linkinv, 
        sigma.linkinv = dstats$linkinv,
                mu.dr = mstats$mu.eta, 
             sigma.dr = dstats$mu.eta, 
     dldm = function(y,mu,sigma,bd) {dldm0 <- (1-sigma)*((sigma+(1-sigma)*dBI(0,bd,mu))^(-1))*dBI(0,bd,mu)*BI()$dldm(0,mu,bd)
                         dldm <- ifelse(y==0, dldm0, BI()$dldm(y,mu,bd))
                         dldm}, 
    d2ldm2 = function(y,mu,sigma,bd) {dldm0 <- (1-sigma)*((sigma+(1-sigma)*dBI(0,bd,mu))^(-1))*dBI(0,bd,mu)*BI()$dldm(0,mu,bd)
                          dldm <- ifelse(y==0, dldm0, BI()$dldm(y,mu,bd))
                        d2ldm2 <- -dldm*dldm
                         d2ldm2},
     dldd = function(y,mu,sigma,bd) {dldd0 <- ((sigma+(1-sigma)*dBI(0,bd,mu))^(-1))*(1-dBI(0,bd,mu))
                         dldd <- ifelse(y==0, dldd0, -1/(1-sigma))
                         dldd}, 
    d2ldd2 = function(y,mu,sigma,bd) {dldd0 <- ((sigma+(1-sigma)*dBI(0,bd,mu))^(-1))*(1-dBI(0,bd,mu))
                          dldd <- ifelse(y==0, dldd0, -1/(1-sigma))
                        d2ldd2 <- -dldd*dldd
                         d2ldd2},
  d2ldmdd = function(y,mu,sigma,bd) {dldm0 <- (1-sigma)*((sigma+(1-sigma)*dBI(0,bd,mu))^(-1))*dBI(0,bd,mu)*BI()$dldm(0,mu,bd)
                         dldm <- ifelse(y==0, dldm0, BI()$dldm(y,mu,bd))
                        dldd0 <- ((sigma+(1-sigma)*dBI(0,bd,mu))^(-1))*(1-dBI(0,bd,mu))
                         dldd <- ifelse(y==0, dldd0, -1/(1-sigma)) 
                      d2ldmdd <- -dldm*dldd
                      d2ldmdd  
                       },
 G.dev.incr  = function(y,mu,sigma,bd,...) -2*dZIBI(y,bd,mu,sigma,log=TRUE),                      
         rqres = expression(rqres(pfun="pZIBI", type="Discrete", ymin=0, bd=bd, y=y, mu=mu, sigma=sigma)) ,
mu.initial = expression(mu <- rep(0.5,length(y))),         #(((y + 0.5)/(bd +1))+0.5)/2}), 
   sigma.initial = expression(sigma <-rep(0.3, length(y))), 
      mu.valid = function(mu) all(mu > 0 & mu < 1),
   sigma.valid = function(sigma)  all(sigma > 0 & sigma < 1), 
       y.valid = function(y)  all(y >= 0)
           ,mean = function(bd, mu, sigma) (1 - sigma) * bd * mu,
       variance = function(bd, mu, sigma) (1 - sigma) * bd * mu * (1 - mu + bd * mu * sigma)
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZIBI<-function(x, bd = 1, mu = 0.5, sigma = 0.1, log = FALSE)
 { 
          if (any(mu <= 0) |  any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))           
          if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
      #    if (any(x < 0) )  stop(paste("x must be 0 or greater than 0", "\n", "")) 
             ly <- max(length(x),length(mu),length(sigma),length(bd)) 
              x <- rep(x, length = ly)      
          sigma <- rep(sigma, length = ly)
             mu <- rep(mu, length = ly)   
             bd <- rep(bd, length = ly) 
          logfy <- rep(0, length(x))
          logfy <- ifelse((x==0), log(sigma+(1-sigma)*dBI(0,bd,mu)), (log(1-sigma) + dBI(x,bd,mu,log=T) ))          
          if(log == FALSE) fy <- exp(logfy) else fy <- logfy
          fy <- ifelse(x < 0, 0, fy) 
          fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pZIBI <- function(q, bd = 1, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {     
         if (any(mu <= 0) |  any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))          
         if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
 #        if (any(q < 0) )  stop(paste("y must be 0 or greater than 0", "\n", "")) 
          ly <- max(length(q),length(mu),length(sigma)) 
           q <- rep(q, length = ly)      
       sigma <- rep(sigma, length = ly)
          mu <- rep(mu, length = ly)   
          bd <- rep(bd, length = ly) 
         cdf <- rep(0,length(q))
         cdf <- pbinom(q, size = bd, prob = mu, lower.tail = TRUE, log.p = FALSE)
         cdf <- sigma + (1-sigma)*cdf
         if(lower.tail == TRUE) cdf <- cdf else cdf <-1-cdf
         if(log.p==FALSE) cdf <- cdf else cdf <- log(cdf) 
         cdf <- ifelse(q < 0, 0, cdf) 
         cdf
   }
################################################################################
################################################################################
################################################################################
################################################################################
qZIBI <- function(p, bd = 1, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {      
         if (any(mu <= 0) |  any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))         
         if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0", "\n", "")) 
#         if (any(p < 0) | any(p > 1))  stop(paste("p must be between 0 and 1", "\n", "")) 
         if (log.p == TRUE) p <- exp(p)   else p <- p
         if (lower.tail == TRUE)  p <- p  else p <- 1 - p
         ly <- max(length(p),length(mu),length(sigma)) 
             p <- rep(p, length = ly)      
         sigma <- rep(sigma, length = ly)
            mu <- rep(mu, length = ly)   
            bd <- rep(bd, length = ly)  
          pnew <- (p-sigma)/(1-sigma)-1e-10
          suppressWarnings(q <- ifelse((pnew > 0 ), qbinom(pnew, size = bd, prob = mu, ), 0))
          q[p == 0] <- 0
          q[p == 1] <- bd
          q[p <  0] <- NaN
          q[p >  1] <- NaN
          return(q)
   }
################################################################################
################################################################################
################################################################################
################################################################################
rZIBI <- function(n, bd = 1, mu = 0.5, sigma = 0.1)
  { 
    if (any(mu <= 0) |  any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))           
    if (any(sigma <= 0) )  stop(paste("sigma must greated than 0", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qZIBI(p, bd = bd, mu = mu, sigma = sigma)
          as.integer(r)
  }
#-----------------------------------------------------------------------------------------
