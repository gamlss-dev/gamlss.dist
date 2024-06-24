################################################################################
################################################################################
################################################################################
################################################################################
# MS 
# last modification Feb 2018
# the 
################################################################################
################################################################################
################################################################################
################################################################################
GPO <- function (mu.link = "log", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "Negative Binomial type I", substitute(mu.link), 
                        c("inverse", "log", "identity", "sqrt"))
    dstats <- checklink("sigma.link", "Negative Binomial type I", substitute(sigma.link), 
                        c("inverse", "log", "identity", "sqrt"))
    structure(
          list(family = c("GPO", "Generalised Poisson"),
           parameters = list(mu = TRUE,sigma = TRUE), 
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
                 dldm = function(y,mu,sigma)
                   { # that seems OK/
                   dldm <- -(sigma*y+1)/(mu*sigma+1)+(sigma*mu*(sigma*y+1))/(mu*sigma+1)^2+
                     ((mu*sigma+1)*(1/(mu*sigma+1)-(mu*sigma)/(mu*sigma+1)^2)*y)/mu
                   dldm
                   }, 
               d2ldm2 = function(y, mu,sigma) {
                 dldm <- -(sigma*y+1)/(mu*sigma+1)+(sigma*mu*(sigma*y+1))/(mu*sigma+1)^2+
                   ((mu*sigma+1)*(1/(mu*sigma+1)-(mu*sigma)/(mu*sigma+1)^2)*y)/mu
                 d2ldm2 <- -dldm^2
                 d2ldm2
                 },
                 dldd = function(y,mu,sigma)
                    {
                  dldd <- -(mu*y)/(mu*sigma+1)+((mu^2)*(sigma*y+1))/(mu*sigma+1)^2+
                     ((y-1)*y)/(sigma*y+1)-(mu*y)/(mu*sigma+1)
                  dldd
                    },
               d2ldd2 = function(y,mu,sigma) {# eval.parent(quote(-dldp*dldp))
                 dldd = -((mu*y)/(mu*sigma+1))+(mu*mu*(sigma*y+1))/(mu*sigma+1)^2+
                     ((y-1)*y)/(sigma*y+1)-(mu*y)/(mu*sigma+1)
                  d2ldd2 <- -dldd^2
                  d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)    
                  d2ldd2               
                                   }, #change this
              d2ldmdd = function(y, mu, sigma) {
                dldm <- -(sigma*y+1)/(mu*sigma+1)+(sigma*mu*(sigma*y+1))/(mu*sigma+1)^2+
                  ((mu*sigma+1)*(1/(mu*sigma+1)-(mu*sigma)/(mu*sigma+1)^2)*y)/mu
                dldd = -mu*y/(mu*sigma+1)+(mu*mu*(sigma*y+1))/(mu*sigma+1)^2+
                  ((y-1)*y)/(sigma*y+1)-(mu*y)/(mu*sigma+1)
                d2ldmdd <- -(dldm) * (dldd)
                d2ldmdd <- ifelse(d2ldmdd < -1e-15, d2ldmdd,-1e-15) 
                d2ldmdd  
                },     
          G.dev.incr  = function(y,mu,sigma,...) -2*dGPO(y, mu = mu, sigma = sigma, log = TRUE), 
               rqres = expression(
                          rqres(pfun="pGPO", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma)
                                 ), 
            mu.initial = expression(mu <-  (y+mean(y))/2),
         sigma.initial = expression(
                      sigma <- rep( 0.1,length(y))),
              mu.valid = function(mu) all(mu > 0) , 
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y)  all(y >= 0),
                  mean = function(mu, sigma) mu,
              variance = function(mu, sigma) mu * (1 + sigma * mu)^2
          ),
            class = c("gamlss.family","family"))
}
################################################################################
################################################################################
################################################################################
################################################################################
dGPO<-function(x, mu = 1, sigma = 1, log = FALSE)
 { 
        if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
        if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
    #    if (any(x < 0) )  stop(paste("x must be >=0", "\n", ""))
        ly <- max(length(x),length(mu),length(sigma)) 
         x <- rep(x, length = ly)      
     sigma <- rep(sigma, length = ly)
        mu <- rep(mu, length = ly)   
        logL <- x*log(mu/(1+sigma*mu))+(x-1)*log(1+sigma*x)+(-mu*(1+sigma*x))/(1+sigma*mu)-lgamma(x+1)
        Lik <- if (log) logL else exp(logL)
        if (length(sigma)>1) fy <- ifelse(sigma>0.0000001, 
                                          Lik, 
                                          dPO(x, mu = mu, log = log))
        else fy <- if (sigma<0.0001) dPO(x, mu = mu, log = log) 
                   else Lik
        fy <-ifelse(x < 0, 0, fy) 
        fy
}
################################################################################
################################################################################
################################################################################
################################################################################
pGPO <- function(q, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
  {     
        if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
        if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
#      if (any(q < 0) )  stop(paste("y must be >=0", "\n", ""))
        if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
      ly <- length(q)                                                       
     FFF <- rep(0,ly)                         
  nsigma <- rep(sigma, length = ly)
     nmu <- rep(mu, length = ly) 
    j <- seq(along=q) 
    for (i in j)                                                          
    {                                                                 
         y.y <- q[i]                                                   
          mm <- nmu[i]
        nsig <- nsigma[i]                                                     
      allval <- seq(0,y.y)
      pdfall <- dGPO(allval, mu = mm, sigma = nsig,  log = FALSE)
      FFF[i] <- sum(pdfall)                                             
    }  
    cdf <- FFF
    cdf <- if(lower.tail==TRUE) cdf else 1-cdf
    cdf <- if(log.p==FALSE) cdf else log(cdf)                                                                    
  #  cdf
  if (length(sigma)>1) cdf <- ifelse(sigma>0.0001, 
                                     cdf, 
                                    pPO(q, mu = mu, log.p = log.p, lower.tail=lower.tail))
    else cdf <- if (sigma<0.0001)   pPO(q, mu = mu, log.p = log.p, lower.tail=lower.tail) 
    else cdf 
        cdf <-ifelse(q < 0, 0, cdf) 
        cdf
   }
################################################################################
################################################################################
################################################################################
################################################################################
qGPO <- function(p, mu = 1, sigma = 1,  lower.tail = TRUE, log.p = FALSE, max.value = 10000)
  {      
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
#  if (any(p < 0) | any(p > 1.0001))  stop(paste("p must be between 0 and 1", "\n", "")) 
  if (log.p==TRUE) p <- exp(p) else p <- p
  if (lower.tail==TRUE) p <- p else p <- 1-p    
  ly <- length(p)                                                       
  QQQ <- rep(0,ly)                         
  nsigma <- rep(sigma, length = ly)
  nmu <- rep(mu, length = ly)                
  for (i in seq(along=p))                                                          
  {
    cumpro <- 0                                                                         
    if (p[i]+0.000000001 >= 1) QQQ[i] <- Inf
    else  
    {  
      for (j in seq(from = 0, to = max.value))
      {
        cumpro <-  pGPO(j, mu = nmu[i], sigma = nsigma[i], log.p = FALSE) 
        # else  cumpro+dSICHEL(j, mu = nmu[i], sigma = nsigma[i], nu = nnu[i], log = FALSE)# the above is faster 
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
rGPO <- function(n, mu = 1, sigma = 1, max.value = 10000)
  { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
          if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
          if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qGPO(p, mu=mu, sigma=sigma, , max.value = max.value)
          r
  }
################################################################################
################################################################################
################################################################################
################################################################################
