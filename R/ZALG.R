################################################################################
################################################################################
################################################################################
################################################################################
ZALG <- function (mu.link = "logit", sigma.link = "logit")
{
    mstats <- checklink("mu.link", "ZALG", substitute(sigma.link), 
                           c("logit", "probit", "cloglog", "cauchit", "log", "own"))  
    dstats <- checklink("sigma.link", "ZALG", substitute(sigma.link), 
                           c("logit", "probit", "cloglog", "cauchit", "log", "own"))   
    structure(
          list(family = c("ZALG", "Zero Adjusted Logarithmic"),
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
     dldm = function(y,mu,sigma) {dldm <- ifelse(y==0, 0, LG()$dldm(y,mu))
                         dldm}, 
    d2ldm2 = function(y,mu,sigma) {dldm <- ifelse(y==0, 0, LG()$dldm(y,mu))
                         d2ldm2<- -dldm^2
                         d2ldm2},
     dldd = function(y,mu,sigma) {dldd <- ifelse(y==0, 1/sigma, -1/(1-sigma))
                         dldd}, 
    d2ldd2 = function(y,mu,sigma) {dldd <- ifelse(y==0, 1/sigma, -1/(1-sigma))
                         d2ldd2<- -dldd^2
                         d2ldd2},
  d2ldmdd = function(y,mu,sigma) {d2ldmdd <- 0
                      d2ldmdd},
 G.dev.incr  = function(y,mu,sigma,...) -2*dZALG(y,mu,sigma,log=TRUE),                      
         rqres = expression(rqres(pfun="pZALG", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma)) ,
    mu.initial =expression({mu <- 0.5 }),
   sigma.initial = expression(sigma <-rep(0.3, length(y))), 
      mu.valid = function(mu) all(mu > 0  & mu < 1),
   sigma.valid = function(sigma)  all(sigma > 0 & sigma < 1), 
       y.valid = function(y)  all(y >= 0),
          mean = function(mu, sigma) {
                          alpha <- - 1 / log(1 - mu)
                          return((1 - sigma) * alpha * mu / (1 - mu))
                  }, 
      variance = function(mu, sigma, nu) {
                          alpha <- - 1 / log(1 - mu)
                          return((1 - sigma) * alpha * mu * (1 - (1 - sigma) * alpha * mu) / (1 - mu)^2)
                  }
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZALG<-function(x, mu = 0.5, sigma = 0.1, log = FALSE)
 { 
          if (any(mu <= 0) | any(mu >= 1))  stop(paste("mu must be between 0 and 1", "\n", ""))          
          if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
     #     if (any(x < 0) )  stop(paste("x must be 0 or greater than 0", "\n", "")) 
           ly <- max(length(x),length(mu),length(sigma)) 
            x <- rep(x, length = ly)      
        sigma <- rep(sigma, length = ly)
           mu <- rep(mu, length = ly)   
          logfy <- rep(0, ly)
          logfy <- ifelse((x==0), log(sigma), log(1-sigma)+dLG(ifelse(x==0,1,x),mu,log = TRUE))      
          if(log == FALSE) fy <- exp(logfy) else fy <- logfy
          fy[x < 1] <- 0
          fy[x==Inf] <- 0
          fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pZALG <- function(q, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {     
         if (any(mu <= 0) | any(mu >= 1))  stop(paste("mu must be between 0 and 1", "\n", ""))           
         if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
     #    if (any(q < 0) )  stop(paste("y must be 0 or greater than 0", "\n", "")) 
           ly <- max(length(q),length(mu)) 
           qq <- rep(q, length = ly)      
           mu <- rep(mu, length = ly)   
   qq[q==Inf] <- 10000 
  fn <- function(q, mu) sum(dLG(1:qq, mu=mu))
          cdf <- rep(0,ly)
         cdf1 <- ifelse((qq==0), 0, pLG(ifelse(qq==0,1,q), mu, log.p = FALSE))
         cdf2 <- sigma + (1-sigma)*cdf1
          cdf <- ifelse((qq==0), sigma,  cdf2)
         if(lower.tail == TRUE) cdf <- cdf else cdf <-1-cdf
         if(log.p==FALSE) cdf <- cdf else cdf <- log(cdf) 
          cdf[q<1] <- 0 
          cdf[q==Inf] <- 1
         cdf
   }
################################################################################
################################################################################
################################################################################
################################################################################
qZALG <- function(p, mu = 0.5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {      
if (any(mu <= 0) | any(mu >= 1))  stop(paste("mu must be between 0 and 1", "\n", ""))    
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0", "\n", "")) 
if (log.p == TRUE) p <- exp(p)   else p <- p
if (lower.tail == TRUE)  p <- p  else p <- 1 - p
             ly <- max(length(p),length(mu),length(sigma)) 
              p <- rep(p, length = ly)      
          sigma <- rep(sigma, length = ly)
             mu <- rep(mu, length = ly) 
          pnew  <- (p-sigma)/(1-sigma)-1e-10
           pnew <- ifelse((pnew >0) ,pnew, 0)
              q <- ifelse((pnew > 0 ), qLG(pnew, mu, log.p=FALSE), 0)   
              q[p == 0] <- 0
              q[p == 1] <- Inf
              q[p <  0] <- NaN
              q[p >  1] <- NaN
              return(q)  
   }
################################################################################
################################################################################
################################################################################
################################################################################
rZALG <- function(n, mu = 0.5, sigma=0.1)
  {
    if (any(mu <= 0) | any(mu >= 1))  stop(paste("mu must be between 0 and 1", "\n", ""))          
    if (any(sigma <= 0) )  stop(paste("sigma must greated than 0", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qZALG(p, mu = mu, sigma = sigma)
          as.integer(r)
  }
################################################################################
################################################################################
################################################################################
################################################################################