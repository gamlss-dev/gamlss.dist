# 3/3/10  correct
################################################################################
################################################################################
################################################################################
################################################################################
# this is new ZAP, the Poisson Zero Adjusted distribution with extra probability for 0, in generic form 
ZAP <- function (mu.link = "log", sigma.link = "logit")
{
    mstats <- checklink("mu.link", "ZAP", substitute(mu.link),    
                           c("1/mu^2", "log", "identity"))
    dstats <- checklink("sigma.link", "ZAP", substitute(sigma.link), 
                           c("logit", "probit", "cloglog", "cauchit", "log", "own"))   
    structure(
          list(family = c("ZAP", "Zero Adjusted Poisson"),
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
     dldm = function(y,mu,sigma) {dldm0 <- PO()$dldm(y,mu) + dPO(0,mu)*PO()$dldm(0,mu)/(1-dPO(0,mu))
                         dldm <- ifelse(y==0, 0 , dldm0)
                         dldm}, 
    d2ldm2 = function(y,mu,sigma) {dldm0 <- PO()$dldm(y,mu) + dPO(0,mu)*PO()$dldm(0,mu)/(1-dPO(0,mu))
                          dldm <- ifelse(y==0, 0 , dldm0)
                        d2ldm2 <- -dldm*dldm
                         d2ldm2},
     dldd = function(y,mu,sigma) {dldd <- ifelse(y==0, 1/sigma, -1/(1-sigma))
                         dldd}, 
    d2ldd2 = function(y,mu,sigma) {d2ldd2 <- -1/(sigma*(1-sigma))
                         d2ldd2},
  d2ldmdd = function(y,mu,sigma) {d2ldmdd <- 0
                      d2ldmdd                        
                       },
 G.dev.incr  = function(y,mu,sigma,...) -2*dZAP(y,mu,sigma,log=TRUE),                      
         rqres = expression(rqres(pfun="pZAP", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma)) ,
    mu.initial = expression(mu <- (y+mean(y))/2),     #rep(mean(y),length(y)) ), 
   sigma.initial = expression(sigma <-rep(0.3, length(y))), 
      mu.valid = function(mu) all(mu > 0) , 
   sigma.valid = function(sigma)  all(sigma > 0 & sigma < 1), 
       y.valid = function(y)  all(y >= 0),
          mean = function(mu, sigma) {
                         c <- (1 - sigma) / (1- exp(-mu))
                         return( c * mu )
                  },
      variance = function(mu, sigma, nu) {
                         c <- (1 - sigma) / (1- exp(-mu))
                         return(c * mu + c * mu^2 - c^2 * mu^2 )
                  } 
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dZAP<-function(x, mu = 5, sigma = 0.1, log = FALSE)
 { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0", "\n", ""))           
          if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
    #      if (any(x < 0) )  stop(paste("x must be 0 or greater than 0", "\n", ""))
             ly <- max(length(x),length(mu),length(sigma)) 
              x <- rep(x, length = ly)      
          sigma <- rep(sigma, length = ly)
             mu <- rep(mu, length = ly)
          logfy <- rep(0, ly)
          logfy <-  log(1-sigma) + dPO(x,mu,log=T) - log(1-dPO(0,mu))
    logfy[x==0] <- log(sigma)  
 #         logfy <- ifelse((x==0), log(sigma), log(1-sigma) + dPO(x,mu,log=T) - log(1-dPO(0,mu)) )          
          if(log == FALSE) fy <- exp(logfy) else fy <- logfy
          fy[x < 0] <- 0
          fy[x == Inf] <- 0
          fy
  }
################################################################################
################################################################################
################################################################################
################################################################################
pZAP <- function(q, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {     
         if (any(mu <= 0) )  stop(paste("mu must be greater than 0", "\n", ""))           
         if (any(sigma <= 0) | any(sigma >= 1) )  stop(paste("sigma must be between 0 and 1", "\n", "")) 
     #    if (any(q < 0) )  stop(paste("y must be 0 or greater than 0", "\n", ""))  
           ly <- max(length(q),length(mu),length(sigma)) 
            q <- rep(q, length = ly)      
        sigma <- rep(sigma, length = ly)
           mu <- rep(mu, length = ly) 
          cdf <- rep(0,ly)
         cdf1 <- ppois(q, lambda = mu, lower.tail = TRUE, log.p = FALSE)
         cdf2 <- ppois(0, lambda = mu, lower.tail = TRUE, log.p = FALSE)
         cdf <- sigma+((1-sigma)*(cdf1-cdf2)/(1-cdf2))
  cdf[q==0]  <- sigma
  #        cdf <- ifelse((q==0),sigma,  cdf3)
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
qZAP <- function(p, mu = 5, sigma = 0.1, lower.tail = TRUE, log.p = FALSE)
  {      
if (any(mu <= 0) )  stop(paste("mu must be greater than 0", "\n", ""))           
if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0", "\n", "")) 

if (log.p == TRUE) p <- exp(p)   else p <- p
if (lower.tail == TRUE)  p <- p  else p <- 1 - p
      ly <- max(length(p),length(mu),length(sigma)) 
       p <- rep(p, length = ly)      
   sigma <- rep(sigma, length = ly)
      mu <- rep(mu, length = ly)
   pnew  <- (p-sigma)/(1-sigma)
   pnew2 <- ppois(0, lambda = mu, lower.tail = TRUE, log.p = FALSE)*(1-pnew) + pnew 
#       q <- 0
#       q[pnew <= 0] <- qpois(pnew2[q[pnew < 0]], lambda = mu)
  suppressWarnings(q <- ifelse((pnew > 0 ), qpois(pnew2, lambda = mu, ), 0))
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
rZAP <- function(n, mu=5, sigma=0.1)
  { 
    if (any(mu <= 0) )  stop(paste("mu must greated than 0", "\n", ""))           
    if (any(sigma <= 0) )  stop(paste("sigma must greated than 0", "\n", "")) 
    if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qZAP(p, mu = mu, sigma = sigma)
          as.integer(r)
  }
################################################################################
################################################################################
################################################################################
################################################################################
