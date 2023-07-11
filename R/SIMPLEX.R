SIMPLEX <- function (mu.link = "logit", sigma.link = "log") 
{
     mstats <- checklink("mu.link", "SIMPLEX", substitute(mu.link), 
                        c("logit", "probit", "cloglog", "cauchit", "log", "own"))
    dstats <- checklink("sigma.link", "SIMPLEX", substitute(sigma.link), 
                        c("inverse", "log", "identity", "sqrt", "own"))
    structure(
          list(family = c("SIMPLEX", "Simplex"),
           parameters = list(mu = TRUE,sigma = TRUE), 
                nopar = 2, 
                 type = "Continuous", 
              mu.link = as.character(substitute(mu.link)),
           sigma.link = as.character(substitute(sigma.link)), 
           mu.linkfun = mstats$linkfun, 
        sigma.linkfun = dstats$linkfun, 
           mu.linkinv = mstats$linkinv, 
        sigma.linkinv = dstats$linkinv,
                mu.dr = mstats$mu.eta, 
             sigma.dr = dstats$mu.eta, 
                 dldm = function(y,mu,sigma){
                    dldm <- -((mu-y)*(mu^2+y-2*y*mu))/(sigma^2*y*(y-1)*mu^3*(mu-1)^3)
                              dldm }, 
               d2ldm2 = function(y,mu,sigma) { 
                    dldm <- -((mu-y)*(mu^2+y-2*y*mu))/(sigma^2*y*(y-1)*mu^3*(mu-1)^3)
                  d2ldm2 <- -dldm *dldm
                  d2ldm2         },
                 dldd = function(y,mu,sigma)
                                 {
                    dldd <- ((y-mu)^2)/((mu^2)*(1-mu)^2 *y*(1-y)*sigma^3)-(1/sigma)
                            dldd },
               d2ldd2 = function(y,mu,sigma) {
                    dldd <- ((y-mu)^2)/((mu^2)*(1-mu)^2 *y*(1-y)*sigma^3)-(1/sigma)
                  d2ldd2 <- -dldd *dldd
                  d2ldd2         }, 
              d2ldmdd = function(y, mu, sigma) 
                                 { 
                dldm <- -((mu-y)*(mu^2+y-2*y*mu))/(sigma^2*y*(y-1)*mu^3*(mu-1)^3)
                dldd <- ((y-mu)^2)/((mu^2)*(1-mu)^2 *y*(1-y)*sigma^3)-(1/sigma)
                d2ldmdd <- -dldm*dldd
                d2ldmdd           },     
           G.dev.incr  = function(y,mu,sigma,...) -2*dSIMPLEX(y, mu = mu, sigma = sigma, log = TRUE), 
                 rqres = expression(rqres(pfun = "pSIMPLEX",  type = "Continuous", 
                                   y = y, mu = mu, sigma = sigma)), 
            mu.initial = expression(mu <- (y + mean(y))/2),
         sigma.initial = expression(sigma <- rep( 1,length(y))),
              mu.valid = function(mu) all(mu > 0 & mu < 1),
           sigma.valid = function(sigma)  all(sigma > 0), 
               y.valid = function(y) all(y > 0 & y < 1)
          ),
            class = c("gamlss.family","family"))
}
#----------------------------------------------------------------------------------------
dSIMPLEX <- function (x, mu=0.5, sigma=1, log = FALSE) 
{
   if (any(mu <= 0) || any(mu >= 1) )  stop(paste("mu must be between 0 and 1", "\n", ""))
   if (any(sigma <= 0) )  stop(paste("sigma must be positive", "\n", "")) 
#   if (any(x <= 0) || any( x >= 1))  stop(paste(" must be between 0 and 1", "\n", ""))
       xx <- ifelse( x<= 0 | x>=1, 0.5 ,  x) 
   logpdf <- -((xx - mu)/(mu * (1 - mu)))^2/(2 * xx * (1 - xx) * sigma^2) - 
              (log(2 * pi * sigma^2) + 3 * (log(xx) + log(1 - xx)))/2
   if (!log) logpdf <- exp(logpdf)
     logpdf <- ifelse( x<= 0 | x>=1, 0, logpdf)
     logpdf
}
#----------------------------------------------------------------------------------------
# pSIMPLEX <- function (q, mu=0.5, sigma=1, lower.tail = TRUE, log.p = FALSE) 
# {
#     if (any(q <= 0) || any(q >= 1)) stop(paste("q must be between 0 and 1", "\n", ""))
#     if (any(mu <= 0) || any(mu >= 1))   stop(paste("mu must be between 0 and 1", "\n", ""))
#     if (any(sigma <= 0) )    stop(paste("sigma must be between positive", "\n", ""))    
#          lp <- pmax.int(length(q), length(mu), length(sigma))                                                                  
#           q <- rep(q, length = lp)
#       sigma <- rep(sigma, length = lp)
#          mu <- rep(mu, length = lp)
#         cdf <- rep(0, length = lp)
#        for (i in 1:lp)
#           {
#          cdf[i] <- integrate(function(x) 
#                  dSIMPLEX(x, mu = mu[i], sigma = sigma[i]), 0, q[i] )$value
#           }    
#          if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf 
#          if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
#          cdf    
# }
pSIMPLEX <- function (q, mu=0.5, sigma=1, lower.tail = TRUE, log.p = FALSE) 
{
 # if (any(q <= 0) || any(q >= 1)) stop(paste("q must be between 0 and 1", "\n", ""))
  if (any(mu <= 0) || any(mu >= 1))   stop(paste("mu must be between 0 and 1", "\n", ""))
  if (any(sigma <= 0) )    stop(paste("sigma must be between positive", "\n", ""))    
     lp <- pmax.int(length(q), length(mu), length(sigma))                                                                  
      q <- rep(q, length = lp)
  sigma <- rep(sigma, length = lp)
     mu <- rep(mu, length = lp)
   zero <- rep(0, length = lp)
    qq <- ifelse( q<= 0 | q>=1, 0.5 ,  q)
    pdf <- function(x, mu,sigma) 1/sqrt(2 * pi * sigma^2 * (x * (1 - x))^3) * exp(-1/2/sigma^2 * 
                      (x - mu)^2/(x * (1 - x) * mu^2 * (1 - mu)^2))
    cdfun <- function(upper, mu, sigma) 
     {int <- integrate(pdf, lower=0, upper=upper, mu, sigma)
      int$value 
      }
    Vcdf <- Vectorize(cdfun)
     cdf <- Vcdf(upper=qq, mu=mu, sigma=sigma)
  if(lower.tail==TRUE) cdf  <- cdf else  cdf <- 1-cdf 
  if(log.p==FALSE) cdf  <- cdf else  cdf <- log(cdf) 
  cdf <- ifelse( q<= 0 | q>=1, 0, cdf)   
  cdf    
}


#----------------------------------------------------------------------------------------
# qSIMPLEX <- function (p,  mu=0.5, sigma=1, lower.tail = TRUE, log.p = FALSE) 
# {
# { 
#     #---functions--------------------------------------------   
#        h1 <- function(q)
#        { 
#      pSIMPLEX(q , mu = mu[i], sigma = sigma[i]) - p[i]  
#        }
#      #-----------------------------------------------------------------
#     if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
#     if (any(mu <= 0) || any(mu >= 1))   stop(paste("mu must be between 0 and 1", "\n", ""))     
#     if (log.p==TRUE) p <- exp(p) else p <- p
#     if (lower.tail==TRUE) p <- p else p <- 1-p
#     if (any(p < 0)|any(p > 1))  stop(paste("p must be between 0 and 1", "\n", ""))     
#          lp <-  max(length(p),length(mu),length(sigma))
#           p <- rep(p, length = lp)                                                                     
#       sigma <- rep(sigma, length = lp)
#          mu <- rep(mu, length = lp)
#           q <- rep(0,lp)  
#          for (i in  seq(along=p)) 
#          {
#             q[i] <- uniroot(h1, c(0.001,.999))$root
#          }
#     q
#    }
# }

qSIMPLEX <- function (p,  mu=0.5, sigma=1, lower.tail = TRUE, log.p = FALSE) 
{
  { 
    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
    if (any(mu <= 0) || any(mu >= 1))   stop(paste("mu must be between 0 and 1", "\n", ""))     
    if (log.p==TRUE) p <- exp(p) else p <- p
    if (lower.tail==TRUE) p <- p else p <- 1-p
    if (any(p < 0)|any(p > 1))  stop(paste("p must be between 0 and 1", "\n", ""))     
     lp <-  max(length(p),length(mu),length(sigma))
      p <- rep(p, length = lp)                                                                     
  sigma <- rep(sigma, length = lp)
     mu <- rep(mu, length = lp)
      q <- rep(0,lp) 
  # local functions    
  h1 <- function(x,mu,sigma,p) pSIMPLEX(x , mu, sigma ) - p  
 uni <- function(mu, sigma, p)
     {
      val <- uniroot(h1, c(0.001,.999), mu=mu, sigma=sigma, p=p)
      val$root
     }
UNI <- Vectorize(uni)
  q <- UNI( mu=mu, sigma=sigma, p=p)  
  q
  }
}
#---------------------------------------------------------------------------
 rSIMPLEX <- function (n = 1,  mu=0.5, sigma=1) 
  {if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", "")) 
  if (any(n <= 0))  stop(paste("n must be a positive integer", "\n", ""))    
    n <- ceiling(n)
    p <- runif(n)
    r <- qSIMPLEX(p, mu=mu, sigma=sigma)
    r
 }
#-------------------------------------------------------------------------- 
 
 
 
