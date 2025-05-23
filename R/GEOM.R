#--------------------------------#
#---- GEOMETRIC DISTRIBUTION ----#
#--------------------------------#
################################################################################
################################################################################
################################################################################
################################################################################
dGEOM<-function (x, mu = 2, log = FALSE)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0)", "\n", ""))
  #  if (any(x < 0)) stop(paste("x must be >=0", "\n", ""))
     lx <- max(length(x), length(mu))
     mu <- rep(mu, length = lx) 
   prob <- 1/(mu+1)
     fy <- dgeom(x = x, prob = prob, log = log)
     fy[x < 0] <- 0
     fy[x == Inf] <- 0
     fy 
}
################################################################################
################################################################################
################################################################################
################################################################################
#Cumulative density function
pGEOM<-function (q, mu = 2, lower.tail = TRUE, log.p = FALSE)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0", "\n", ""))
 #   if (any(q < 0)) stop(paste("q must be >=0", "\n", ""))
     ly <- max(length(q), length(mu))
      q <- rep(q, length = ly)
     mu <- rep(mu, length = ly)
   prob <- 1/(mu+1)
    cdf <-  pgeom(q, prob=prob, lower.tail = lower.tail, log.p =log.p)
    cdf[q < 0] <- 0
    cdf[q ==Inf] <- 1
    cdf    
}
################################################################################
################################################################################
################################################################################
################################################################################
#Quantile function
qGEOM<-function (p, mu = 2, lower.tail = TRUE, log.p = FALSE)
{
if (any(mu < 0))
        stop(paste("mu must be > 0)", "\n", ""))  
if (log.p==TRUE) p <- exp(p) 
if (lower.tail == FALSE) p <- 1-p 
     ly <- max(length(p), length(mu))
      p <- rep(p, length = ly)
     mu <- rep(mu, length = ly)
   prob <- 1/(mu+1)
    QQQ <- qgeom(p, prob=prob, lower.tail = TRUE, log.p = FALSE)
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
rGEOM<- function(n, mu=2)
{
    if (any(mu < 0))
        stop(paste("mu must be > 0)", "\n", ""))
    if (any(n <= 0))
        stop(paste("n must be a positive integer", "\n", ""))
     n <- ceiling(n)
     p <- runif(n)
     r <- qGEOM(p, mu=mu)
     as.integer(r)
}
################################################################################
################################################################################
################################################################################
################################################################################
#Distribution function
GEOM<-function (mu.link = "log")
{
mstats <- checklink("mu.link", "Geometric", substitute(mu.link), 
        c("log", "probit", "cloglog", "cauchit", "log", "own"))
structure(list(family = c("GEOM", "Geometric"),      
        parameters = list(mu = TRUE),     
        nopar = 1,                       
        type = "Discrete",               
        mu.link = as.character(substitute(mu.link)),  
        mu.linkfun = mstats$linkfun, 
        mu.linkinv = mstats$linkinv, 
        mu.dr = mstats$mu.eta, 
        dldm = function(y, mu) (y - mu)/(mu + (mu^2)),                                             
        d2ldm2 = function(mu){
          d2ldm2 <- -1/(mu+(mu^2))
          d2ldm2
        }, 
        G.dev.incr = function(y, mu, ...) -2*dGEOM(x=y, mu=mu, log = TRUE),                 
        rqres = expression(rqres(pfun = "pGEOM", type = "Discrete", 
                            ymin = 0, y = y, mu = mu)),   
        mu.initial = expression(mu <- rep(mean(y), length(y))),            
        mu.valid = function(mu) all(mu > 0) , 
        y.valid = function(y) all(y >=0),
        mean = function(mu) mu,
        variance  = function(mu) mu + mu^2  
        ),      
        class = c("gamlss.family", "family"))
}

