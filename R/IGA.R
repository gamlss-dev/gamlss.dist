######################################
##### Inverse Gamma Distribution #####
######################################
##IGAMMA (alpha, mu/(alpha+1))?NO
# where alpha = 1/sigma^2
# x>0, alpha > 0, mu > 0
################################################################################
################################################################################
################################################################################
################################################################################
#Probability Density function
dIGAMMA <- function(x, mu = 1, sigma = 0.5, log = FALSE)
{
if (any(mu < 0)) stop(paste("mu must be greater than 0", "\n", ""))
if (any(sigma <= 0)) stop(paste("sigma must be greater than 0", "\n", ""))
      ln <- max(length(x), length(mu), length(sigma))
       x <- rep_len(x, ln)
      mu <- rep_len(mu, ln)
   sigma <- rep_len(sigma, ln)
   alpha <- 1/(sigma^2)
     lfy <- alpha*log(mu) + alpha*log(alpha+1) - lgamma(alpha) -
	          (alpha + 1)*log(x) - ((mu*(alpha + 1))/x)
     fy <- if (log == FALSE) exp(lfy) else lfy
  fy[x <= 0] <- 0
fy[x >= Inf] <- 0
   fy 
}
################################################################################
################################################################################
################################################################################
################################################################################
#Cumulative density function
pIGAMMA <- function(q, mu = 1, sigma = 0.5, lower.tail = TRUE, log.p = FALSE)
{
if (any(mu <= 0)) stop(paste("mu must be greater than 0", "\n", ""))
if (any(sigma <= 0)) stop(paste("sigma must be greater than 0", "\n", ""))
       n <- max(length(q), length(mu), length(sigma))
       q <- rep_len(q, n)
      mu <- rep_len(mu, n)
   sigma <- rep_len(sigma, n)
   alpha <- 1/(sigma^2)
    lcdf <- pgamma(((mu*(alpha + 1))/q), shape=alpha, lower.tail=FALSE, log.p = TRUE)  
if (log.p == FALSE) cdf <- exp(lcdf) else cdf <- lcdf 
if (lower.tail == TRUE) cdf <- cdf else cdf <- 1 - cdf
    cdf[q<=0] <- 0
    cdf[q>=Inf] <- 1
    cdf
} 
################################################################################
################################################################################
################################################################################
################################################################################
#Quantile 
#qIGAMMA <- function(p, mu = 1, sigma = 1, lower.tail = TRUE, log.p = FALSE)
#{
#  #---functions--------------------------------------------   
#       h1 <- function(q)
#       { 
#     pIGAMMA(q , mu = mu[i], sigma = sigma[i])-p[i]   
#       }
#       h <- function(q)
#       { 
#     pIGAMMA(q , mu = mu[i], sigma = sigma[i])   
#       }
#     #-------------------------------------------------------
#    if (any(mu <= 0))  stop(paste("mu must be positive", "\n", "")) 
#    if (any(sigma <= 0))  stop(paste("sigma must be positive", "\n", ""))      
#    if (lower.tail==TRUE) p <- p else p <- 1-p
#    if (log.p==TRUE) p <- exp(p) else p <- p
#    if (any(p < 0)|any(p > 1))  stop(paste("p must be between 0 and 1", #"\n", ""))     
#         lp <-  max(length(p),length(mu),length(sigma))
#          p <- rep(p, length = lp)                                                                     
#      sigma <- rep(sigma, length = lp)
#         mu <- rep(mu, length = lp)
#          q <- rep(0,lp)      
#         for (i in  seq(along=p)) 
#         {
#         if (h(mu[i])<p[i]) 
#          { 
#           interval <- c(mu[i], mu[i]+sigma[i])
#           j <-2
#           while (h(interval[2]) < p[i]) 
#              {interval[2]<- mu[i]+j*sigma[i]
#              j<-j+1 
#              }
#           } 
#          else  
#           {
#           interval <-  interval <- c(.Machine$double.xmin, mu[i])
#           }
#        q[i] <- uniroot(h1, interval)$root
#         }
#    q
#   }
################################################################################
################################################################################
################################################################################
################################################################################
qIGAMMA <- function(p, mu=1, sigma=0.5,  lower.tail = TRUE, log.p = FALSE )
 { 
if (any(mu < 0))  stop(paste("mu must be positive", "\n", "")) 
if (any(sigma < 0))  stop(paste("sigma must be positive", "\n", ""))  
if (log.p==TRUE) p <- exp(p) else p <- p
if (lower.tail==TRUE) p <- p else p <- 1-p
               n <- max(length(p), length(mu), length(sigma))
               p <- rep_len(p, n)
              mu <- rep_len(mu, n)
           sigma <- rep_len(sigma, n)  
              nu <- -1
               p <- if(nu>0)  p else 1-p
               z <- qGA(p, mu=1, sigma=sigma*abs(nu))
             mu2 <- mu *(1+sigma^2) 
              y <- mu2*z^(1/nu)
              y[p == 0] <- 0
              y[p == 1] <- Inf
              y[p <  0] <- NaN
              y[p >  1] <- NaN
              return(y)
    
 }
################################################################################
################################################################################
################################################################################
################################################################################
#Random Generating
rIGAMMA <- function (n, mu = 1, sigma = 0.5)
{
   if (any(mu <= 0))
	stop(paste("mu must be greater than 0", "\n", ""))
   if (any(sigma <= 0))
	stop(paste("sigma must be greater than 0", "\n", ""))
   if (any(n <= 0)) 
        stop(paste("n must be a positive integer", "\n", ""))  
   n <- ceiling(n)
   p <- runif(n)
   r <- qIGAMMA(p, mu = mu, sigma = sigma)
   r 
}
#--------------------------------------------------------------------------------
#Gamlss Family Function
IGAMMA <- function (mu.link = "log", sigma.link = "log") 
{
    mstats <- checklink("mu.link", "Inverse Gamma", substitute(mu.link), 
        c("inverse", "log", "identity", "own"))
    dstats <- checklink("sigma.link", "Inverse Gamma", substitute(sigma.link), 
        c("inverse", "log", "identity", "own"))
    structure(list(family = c("IGAMMA", "Inverse Gamma"), 
    parameters = list(mu = TRUE, sigma = TRUE), 
    nopar = 2, type = "Continuous", 
    mu.link = as.character(substitute(mu.link)), 
    sigma.link = as.character(substitute(sigma.link)), 
    mu.linkfun = mstats$linkfun, 
    sigma.linkfun = dstats$linkfun, 
    mu.linkinv = mstats$linkinv, 
    sigma.linkinv = dstats$linkinv, 
    mu.dr = mstats$mu.eta, 
    sigma.dr = dstats$mu.eta, 
    dldm = function(y, mu, sigma){ 
       alpha <- 1/(sigma^2)
       dldm <- (alpha/mu) - ((alpha + 1)/y)
       dldm
    }, 
    d2ldm2 = function(y, mu, sigma){
       d2ldm2 <- -(1/(sigma^2*mu^2)) 
       d2ldm2
    }, 
    dldd = function(y, mu, sigma){
        alpha <- 1/(sigma^2)
        dldd <- (-2/(sigma^3))*(log(mu) + (alpha/(alpha+1)) + log(alpha+1) - 
                 digamma(alpha) - log(y) - (mu/y))
        dldd
    }, 
    d2ldd2 = function(y, mu, sigma){
        d2ldd2 <- -((4*(-((sigma^2*(1+2*sigma^2))/((1+sigma^2)^2))+
                 psigamma(1/sigma^2, 1)))/(sigma^6))
        d2ldd2
    }, 
    d2ldmdd = function(y, mu, sigma){ 
       d2ldmdd <- -(2/(mu*sigma+mu*sigma^3))
       d2ldmdd 
    }, 
    G.dev.incr = function(y, mu, sigma, ...) -2 * dIGAMMA(y, mu, sigma, log = TRUE), 
         rqres = expression(rqres(pfun = "pIGAMMA", type = "Continuous", y = y, 
                             mu = mu, sigma = sigma)), 
    mu.initial = expression({ mu <- rep(mean(y), length(y))}), 
 sigma.initial = expression({ sigma <- rep(((mean(y)^2)/var(y))+2, length(y)) }), 
      mu.valid = function(mu) all(mu > 0), 
   sigma.valid = function(sigma) all(sigma > 0), 
       y.valid = function(y) TRUE,
          mean = function(mu, sigma) ifelse(sigma^2 < 1,
                                   ((1+sigma^2)*mu)/(1-sigma^2),Inf),
      variance = function(mu, sigma) ifelse(sigma^2 < 0.5,
                      ((1+sigma^2)^2*mu^2*sigma^2)/((1-sigma^2)^2*(1-2*sigma^2)),
                                       Inf)
    ),
    class = c("gamlss.family", "family"))
}
################################################################################
################################################################################
################################################################################
################################################################################


