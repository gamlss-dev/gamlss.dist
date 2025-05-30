# 3/12/2004 version of ZIP amended on 15/10/06 to ZIP2
################################################################################
################################################################################
################################################################################
################################################################################
# This is ZIP2, the Zero Inflated Poisson distribution with extra probability for 0, type 2
# In ZIP2 the parameter mu is exactly the mean of the distribution
ZIP2 <- function (mu.link = "log", sigma.link = "logit")
{
    mstats <- checklink("mu.link", "ZIP2", substitute(mu.link),    
                           c("1/mu^2", "log", "identity"))
    dstats <- checklink("sigma.link", "ZIP2", substitute(sigma.link), 
                           c("logit", "probit", "cloglog", "log"))   
    structure(
     list(
       family = c("ZIP2", "Zero Inflated Poisson 2"),
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
         dldm = function(y,mu,sigma) {  
                          mus <- mu/(1-sigma)
                        dldm0 <- -(((1-sigma)+sigma*exp(mus))^(-1))
                         dldm <- ifelse(y==0, dldm0, (y-mus)/mu)
                         dldm}, 
       d2ldm2 = function(y,mu,sigma) {
                          mus <- mu/(1-sigma)
                        dldm0 <- -(((1-sigma)+sigma*exp(mus))^(-1))
                         dldm <- ifelse(y==0, dldm0, (y-mus)/mu)
                       d2ldm2 <- -dldm*dldm
                       d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)  
                         d2ldm2},
          dldd = function(y,mu,sigma) { 
                          mus <- mu/(1-sigma)
                        dldd0 <- (1-(1+mus)*exp(-mus))*((sigma+(1-sigma)*exp(-mus))^(-1)) 
                         dldd <- ifelse(y==0, dldd0, ((y-1)/(1-sigma))-((mus^2)/mu))
                         dldd}, 
        d2ldd2 = function(y,mu,sigma) {
                          mus <- mu/(1-sigma)
                        dldd0 <- (1-(1+mus)*exp(-mus))*((sigma+(1-sigma)*exp(-mus))^(-1)) 
                         dldd <- ifelse(y==0, dldd0, ((y-1)/(1-sigma))-((mus^2)/mu)) 
                       d2ldd2 <- -dldd*dldd
                       d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15) 
                         d2ldd2},
        d2ldmdd = function(y,mu,sigma) {
                          mus <- mu/(1-sigma)
                        dldm0 <- -(((1-sigma)+sigma*exp(mus))^(-1))
                         dldm <- ifelse(y==0, dldm0, (y-mus)/mu)
                        dldd0 <- (1-(1+mus)*exp(-mus))*((sigma+(1-sigma)*exp(-mus))^(-1)) 
                         dldd <- ifelse(y==0, dldd0, ((y-1)/(1-sigma))-((mus^2)/mu))
                      d2ldmdd <- -dldm*dldd
                      d2ldmdd                        
                       },
     G.dev.incr = function(y,mu,sigma,...) -2*dZIP2(y,mu,sigma,log=TRUE),                       
          rqres = expression(rqres(pfun="pZIP2", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma)),
     mu.initial = expression(mu <- (y+mean(y))/2),     #rep(mean(y),length(y)) ), 
  sigma.initial = expression(sigma <- rep(0.3, length(y))), 
       mu.valid = function(mu) all(mu > 0) , 
    sigma.valid = function(sigma)  all(sigma > 0 & sigma < 1), 
        y.valid = function(y)  all(y >= 0),
           mean = function(mu, sigma) mu,
       variance = function(mu, sigma) mu * (1 +(mu * sigma) / (1-sigma))
          ),
          class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZIP2<-function(x, mu = 5, sigma = 0.1, log = FALSE)
 { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0", "\n", ""))           
          if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
   #       if (any(x < 0) )  stop(paste("x must be 0 or greater than 0", "\n", ""))   
           ly <- max(length(x),length(mu),length(sigma)) 
            x <- rep(x, length = ly)      
        sigma <- rep(sigma, length = ly)
           mu <- rep(mu, length = ly)   
          mus <- mu/(1-sigma)
        logfy <- rep(0, length(x))
  logfy[x==0] <-  log(sigma+(1-sigma)*exp(-mus))
  logfy[x!=0] <- (1-x)*log(1-sigma) - mus +x*log(mu) -lgamma(x+1)
 #       logfy <- ifelse((x==0), log(sigma+(1-sigma)*exp(-mus)), 
#                                  ((1-x)*log(1-sigma) - mus +x*log(mu) -lgamma(x+1)))          
if(log == FALSE) fy <- exp(logfy) else fy <- logfy
      fy[x < 0] <- 0
   fy[x >= Inf] <- 0
          fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pZIP2 <- function(q, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {     
         if (any(mu <= 0) )  stop(paste("mu must be greater than 0", "\n", ""))           
         if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
     #    if (any(q < 0) )  stop(paste("y must be 0 or greater than 0", "\n", "")) 
          ly <- max(length(q),length(mu),length(sigma)) 
           q <- rep(q, length = ly)      
       sigma <- rep(sigma, length = ly)
          mu <- rep(mu, length = ly) 
         mus <- mu/(1-sigma)
         cdf <- rep(0,length(q))
         cdf <- ppois(q, lambda = mus, lower.tail = TRUE, log.p = FALSE)
         cdf <- sigma + (1-sigma)*cdf
         if(lower.tail == TRUE) cdf <- cdf else cdf <-1-cdf
         if(log.p==FALSE) cdf <- cdf else cdf <- log(cdf)  
         cdf[q < 0] <- 0 
         cdf[q > Inf] <- 1 
         cdf
   }
################################################################################
################################################################################
################################################################################
################################################################################
qZIP2 <- function(p, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {      
         if (any(mu <= 0) )  stop(paste("mu must be greater than 0", "\n", ""))           
         if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0", "\n", "")) 
         if (log.p == TRUE) p <- exp(p)   else p <- p
         if (lower.tail == TRUE)  p <- p  else p <- 1 - p
            ly <- max(length(p),length(mu),length(sigma)) 
             p <- rep(p, length = ly)      
         sigma <- rep(sigma, length = ly)
            mu <- rep(mu, length = ly)
           mus <- mu/(1-sigma)
          pnew <- (p-sigma)/(1-sigma)-1e-10
          suppressWarnings(q <- ifelse((pnew > 0 ), qpois(pnew, lambda = mus, ), 0))
          q[p == 0] <- 0
          q[p == 1] <- Inf
          q[p <  0] <- NaN
          q[p >  1] <- NaN
          return(q)  
   }
#-----------------------------------------------------------------------------------------
rZIP2 <- function(n, mu=5, sigma=0.1)
  { 
    if (any(mu <= 0) )  stop(paste("mu must greated than 0", "\n", ""))           
    if (any(sigma <= 0) )  stop(paste("sigma must greated than 0", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qZIP2(p, mu = mu, sigma = sigma)
          as.integer(r)
  }
#-----------------------------------------------------------------------------------------
