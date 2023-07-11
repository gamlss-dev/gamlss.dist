## the Poison Inverse Gaussian 2 distribution
## RAR and MS   02_09_2019
##-----------------------------------------------------------------------------------------
PIG2 <- function (mu.link = "log", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "PIG2", substitute(mu.link),
                         c("inverse", "log", "identity", "sqrt"))   
    dstats <- checklink("sigma.link", "PIG2", substitute(sigma.link), 
                        c("inverse", "log", "identity", "sqrt"))   
    structure(
          list(family = c("PIG2","Poisson.Inverse.Gaussian.2"),
           parameters = list(mu = TRUE, sigma = TRUE),
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
                          dldm <- (y/mu)-1+(mu-y)/sqrt(mu*mu+sigma*sigma)
                          dldm
                              },
               # dldm = function(y, mu, sigma) {   
               #   nd <- numeric.deriv(dPIG2(y, mu, sigma, log=TRUE), "mu", delta=0.001)
               # dldm <- as.vector(attr(nd, "gradient"))
               # dldm
               #  },
               #  d2ldv2 = function(y,mu,sigma){
               #     nd <- numeric.deriv(dPIG2(y, mu, sigma, log=TRUE), "mu", delta=0.001)
               #   dldm <- as.vector(attr(nd, "gradient"))
               # d2ldm2 <- -dldm*dldm
               # d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)  
               #  d2ldv2},
               d2ldm2 = function(y,mu,sigma) {
                          #  dldm <- (y/mu)-1+ (mu-y)/sqrt(mu*mu+sigma*sigma)
                          d2ldm2 <- -(1/mu) +  (mu*mu + sigma*sigma)^(-0.5)
                          d2ldm2 <- ifelse(d2ldm2 < -1e-15, d2ldm2,-1e-15)
                          d2ldm2
                                    },
               #   dldd = function(y,mu,sigma) {
               #            alpha <- 1/(((mu*mu+sigma*sigma)^0.5)-mu)
               #            dlda <- PIG()$dldd(y,mu,alpha)
               #            dadd <- -sigma*((mu*mu+sigma*sigma)^(-0.5))/(alpha*alpha)
               #            dldd <- dlda*dadd
               #            dldd          
               #                      },
               # d2ldd2 = function(y,mu,sigma){
               #            alpha <- 1/(((mu*mu+sigma*sigma)^0.5)-mu)
               #            dlda <- PIG()$dldd(y,mu,alpha)
               #            dadd <- -sigma*((mu*mu+sigma*sigma)^(-0.5))/(alpha*alpha)
               #            dldd <- dlda*dadd
               #            d2ldd2 <- -dldd*dldd
               #            d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)  
               #            d2ldd2
                dldd = function(y,mu,sigma) {                           
                  nd <- numeric.deriv(dPIG2(y, mu, sigma, log=TRUE), "sigma", delta=0.001)
                dldd <- as.vector(attr(nd, "gradient"))
                dldd
                 },
               d2ldd2 = function(y,mu,sigma){
                   nd <- numeric.deriv(dPIG2(y, mu, sigma, log=TRUE), "sigma", delta=0.001)
                 dldd <- as.vector(attr(nd, "gradient"))
               d2ldd2 <- -dldd*dldd
               d2ldd2 <- ifelse(d2ldd2 < -1e-15, d2ldd2,-1e-15)  
              d2ldd2},                     
              d2ldmdd = function(y,mu,sigma) {
                         d2ldmdd <- 0
                         d2ldmdd
                                    },
           G.dev.incr  = function(y,mu,sigma,pw=1,...) -2*dPIG2(y, mu, sigma, log=TRUE),
                rqres = expression(
                  rqres(pfun="pPIG2", type="Discrete", ymin=0, y=y, mu=mu, sigma=sigma)
                                   ), 
           mu.initial = expression(mu <- (y+mean(y))/2),
        sigma.initial = expression( 
          sigma <- sqrt(rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y))^(-2) + 
              2*mu*(rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y)) ^(-1))))
        ,
# above sigma <- 1 needs changing to the following 2 lines
# alpha <- rep( max( ((var(y)-mean(y))/(mean(y)^2)),0.1),length(y)) )
# sigma <- sqrt(alpha^(-2) + 2*mu*(alpha^(-1)))
             mu.valid = function(mu) all(mu > 0) , 
          sigma.valid = function(sigma)  all(sigma > 0), 
              y.valid = function(y)  all(y >= 0)
          ),
                class = c("gamlss.family","family"))
}
##-----------------------------------------------------------------------------------------
dPIG2<-function(x, mu = 0.5, sigma = 0.02 , log = FALSE)
 { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
          if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
   #       if (any(x < 0) )  stop(paste("x must be >=0", "\n", ""))  
          ly <- length(x)                                                                    
      nsigma <- rep(sigma, length = ly)
         nmu <- rep(mu, length = ly)
       alpha <- 1/(((nmu*nmu+nsigma*nsigma)^0.5)-nmu)
          fy <- dPIG(x,nmu,alpha,log=log)
          fy <-ifelse(x < 0, 0, fy) 
          fy
  }
##-----------------------------------------------------------------------------------------  
pPIG2 <- function(q, mu=0.5, sigma=0.02, lower.tail = TRUE, log.p = FALSE)
  {     
  if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
  if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
 # if (any(q < 0) )  stop(paste("y must be >=0", "\n", ""))  
      lq <- length(q)                                                                    
  nsigma <- rep(sigma, length = lq)
     nmu <- rep(mu, length = lq)   
   alpha <- 1/(((nmu*nmu+nsigma*nsigma)^0.5)-nmu)
     cdf <- pPIG(q,nmu,alpha,lower.tail=lower.tail,log.p=log.p)
     cdf <- ifelse(q < 0, 0, cdf) 
     cdf
   }
##-----------------------------------------------------------------------------------------
qPIG2 <- function(p, mu = 0.5, sigma = 0.02,  lower.tail = TRUE, log.p = FALSE, 
                 max.value = 10000)
  {      
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
          if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
          if (any(p < 0) | any(p > 1.0001))  stop(paste("p must be between 0 and 1", "\n", "")) 
          if (log.p==TRUE) p <- exp(p) else p <- p
          if (lower.tail==TRUE) p <- p else p <- 1-p 
              ly <- length(p)                                                                              
          nsigma <- rep(sigma, length = ly)
             nmu <- rep(mu, length = ly)    
           alpha <- 1/(((nmu*nmu+nsigma*nsigma)^0.5)-nmu)
             QQQ <- qPIG(p,nmu,alpha,lower.tail=lower.tail,log.p=log.p)
             QQQ   
   }
##-----------------------------------------------------------------------------------------
rPIG2 <- function(n, mu = 0.5, sigma = 0.02)
  { 
          if (any(mu <= 0) )  stop(paste("mu must be greater than 0 ", "\n", "")) 
          if (any(sigma <= 0) )  stop(paste("sigma must be greater than 0 ", "\n", "")) 
          if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))  
          n <- ceiling(n)
          p <- runif(n)
          r <- qPIG2(p, mu=mu, sigma=sigma)
          as.integer(r)
  }
##-----------------------------------------------------------------------------------------
