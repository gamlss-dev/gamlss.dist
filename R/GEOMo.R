#--------------------------------#
#---- GEOMETRIC DISTRIBUTION original----#
#--------------------------------#
################################################################################
################################################################################
################################################################################
################################################################################
dGEOMo<-function (x, mu = .5, log = FALSE)
{
    if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", "")) 
   # if (any(x < 0)) stop(paste("x must be >=0", "\n", ""))
        lx <- max(length(x), length(mu))
        mu <- rep(mu, length = lx)
        fy <- dgeom(x = x, prob = mu, log = log)
        fy[x < 0] <- 0
        fy[x == Inf] <- 0
        fy #logfx
}
################################################################################
################################################################################
################################################################################
################################################################################
#Cumulative density function
pGEOMo<-function (q, mu = .5, lower.tail = TRUE, log.p = FALSE)
{
    if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", "")) 
    #if (any(q < 0)) stop(paste("q must be >=0", "\n", ""))
     ly <- max(length(q), length(mu))
      q <- rep(q, length = ly)
     mu <- rep(mu, length = ly)
    cdf <-  pgeom(q, prob=mu, lower.tail = lower.tail, log.p =log.p)
    cdf[q < 0] <- 0
    cdf[q ==Inf] <- 1
    cdf
}
################################################################################
################################################################################
################################################################################
################################################################################
#Quantile function
qGEOMo<-function (p, mu = .5, lower.tail = TRUE, log.p = FALSE)
{
if (any(mu < 0) | any(mu > 1))  stop(paste("mu must be between 0 and 1", "\n", "")) 
if (log.p==TRUE) p <- exp(p) 
if (lower.tail == FALSE) p <- 1-p 
   QQQ <- qgeom(p, prob=mu, lower.tail = TRUE, log.p = FALSE)
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
#Random Generating Function
rGEOMo<- function(n, mu=.5)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0)", "\n", ""))
    if (any(n <= 0))
        stop(paste("n must be a positive integer", "\n", ""))
     n <- ceiling(n)
     p <- runif(n)
     r <- qGEOMo(p, mu=mu)
     as.integer(r)
}
################################################################################
################################################################################
################################################################################
################################################################################
#Distribution function
GEOMo<-function (mu.link = "logit")
{
mstats <- checklink("mu.link", "Geometric", substitute(mu.link), 
        c("logit", "probit", "cloglog", "cauchit", "log", "own"))

structure(list(family = c("GEOMo", "Geometric original"),      
           parameters = list(mu = TRUE),     
                nopar = 1,                       
                 type = "Discrete",               
              mu.link = as.character(substitute(mu.link)),  
           mu.linkfun = mstats$linkfun,
           mu.linkinv = mstats$linkinv,
                mu.dr = mstats$mu.eta,
                 dldm = function(y, mu){
                      
          dldm <- 1/mu - y/(1-mu)
          dldm
        },                                             
        d2ldm2 = function(y, mu){
         
            dldm <-  1/mu - y/(1-mu)
          d2ldm2 <- -dldm*dldm
          d2ldm2
        }, 
        G.dev.incr = function(y, mu, ...) -2*dGEOMo(y, mu, log = TRUE),                 
        rqres = expression(rqres(pfun = "pGEOMo", type = "Discrete", 
                            ymin = 0, y = y, mu = mu)),   
        mu.initial = expression(mu <- rep(.5, length(y))),            
        mu.valid = function(mu) all(mu > 0) && all(mu < 1),   
        y.valid = function(y) all(y >=0),
        mean = function(mu) (1-mu) * mu^-1,
        variance = function(mu) (1-mu) * mu^-2
        ),      
        class = c("gamlss.family", "family"))
}
################################################################################
################################################################################
################################################################################
################################################################################