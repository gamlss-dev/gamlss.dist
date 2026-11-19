################################################################################
################################################################################
################################################################################
################################################################################
dZIP3 <- function (x, mu = 5.0, sigma = 1, log = FALSE){
  if (any(mu <= 0)) 
    stop(paste("mu must be greater than 0", "\n", ""))
  if (any(sigma <= 0))
    stop(paste("sigma must be greater than 0", "\n", ""))
  ly <- max(length(x), length(mu), length(sigma))
  x <- rep(x, length = ly)
  sigma <- rep(sigma, length = ly)
  mu <- rep(mu, length = ly)
  logfy <- rep(0, length(x))
  logfy <- ifelse((x == 0), log(sigma + mu * exp(-(mu + sigma) )) - log(mu + sigma), 
                  (log(mu) + (x - 1) * log(mu + sigma) - mu - sigma - lgamma(x + 1) ))
  if (log == FALSE) 
    fy <- exp(logfy)
  else fy <- logfy
  fy <- ifelse(x < 0, 0, fy)
  fy
} #ok!
################################################################################
################################################################################
################################################################################
################################################################################
qZIP3 <- function (p, mu = 5, sigma = 1, lower.tail = TRUE, log.p = FALSE) 
{
  if (any(mu <= 0)) 
    stop(paste("mu must be greater than 0", "\n", ""))
  if (any(sigma <= 0)) 
    stop(paste("sigma must be greater than 0", "\n", ""))
  if (any(p < 0) | any(p > 1)) 
    stop(paste("p must be between 0 and 1", "\n", ""))
  
  if(log.p == TRUE) p <- exp(p) else p <- p
  if (lower.tail == TRUE) p <- p else p <- 1 - p
  
  ly <- max(length(p), length(mu), length(sigma))
  p <- rep(p, length = ly)
  sigma <- rep(sigma, length = ly)
  mu <- rep(mu, length = ly)
  lambda <- mu + sigma
  delta <- sigma/(mu + sigma)
  pnew <- ((p - delta)/(1 - delta)) - (1e-07)
  pnew <- ifelse(pnew > 0, pnew, 0)
  q <- qpois(pnew, lambda = lambda)
  q
} #ok!
################################################################################
################################################################################
################################################################################
################################################################################
pZIP3 <- function (q, mu = 5, sigma = 1,
                   lower.tail = TRUE, log.p = FALSE) 
{
  if (any(mu <= 0)) 
    stop(paste("mu must be greater than 0", "\n", ""))
  if (any(sigma <= 0)) 
    stop(paste("sigma must be greater than 0", "\n", ""))
  ly <- max(length(q), length(mu), length(sigma))
  q <- rep(q, length = ly)
  sigma <- rep(sigma, length = ly)
  mu <- rep(mu, length = ly)
  lambda <- mu + sigma
  delta <- sigma/(mu + sigma)
  cdf <- rep(0, length(q))
  cdf <- pZIP(q, mu = lambda, sigma = delta, lower.tail = lower.tail, log.p = log.p)
  
  cdf
} #ok!
################################################################################
################################################################################
################################################################################
################################################################################
rZIP3 <- function (n, mu = 5, sigma = 1) 
{
  if (any(mu <= 0)) 
    stop(paste("mu must greated than 0", "\n", ""))
  if (any(sigma <= 0)) 
    stop(paste("sigma must greated than 0", "\n", ""))
  if (any(n <= 0)) 
    stop(paste("n must be a positive integer", "\n", ""))
  n <- ceiling(n)
  p <- runif(n)
  r <- qZIP3(p, mu = mu, sigma = sigma)
  as.integer(r)
} #ok!
################################################################################
################################################################################
################################################################################
################################################################################
ZIP3 <- function (mu.link = "log", sigma.link = "log"){
  mstats <- checklink("mu.link", "ZIP3", substitute(mu.link), 
                      c("log", "identity"))
  dstats <- checklink("sigma.link", "ZIP3", substitute(sigma.link), 
                      c("log", "identity"))
  structure(list(family = c("ZIP3", "Zero Inflated Poisson 3"), 
                 parameters = list(mu = TRUE, sigma = TRUE), nopar = 2, 
                 type = "Discrete", mu.link = as.character(substitute(mu.link)), 
                 sigma.link = as.character(substitute(sigma.link)), mu.linkfun = mstats$linkfun, 
                 sigma.linkfun = dstats$linkfun, mu.linkinv = mstats$linkinv, 
                 sigma.linkinv = dstats$linkinv, mu.dr = mstats$mu.eta, 
                 sigma.dr = dstats$mu.eta, 
                 dldm = function(y, mu, sigma) {
                   dldm0 <- (1-mu)/(mu + sigma*exp(mu + sigma)) - 1/(mu + sigma)
                   dldm1 <- 1/mu + (y-1)/(mu + sigma) - 1
                   dldm <- ifelse(y == 0, dldm0, dldm1)
                   dldm
                 }, #ok! 
                 d2ldm2 = function(y, mu, sigma) {
                   d2ldm0 <- ((mu-2)*sigma*exp(mu+sigma) - 1)/((mu + sigma*exp(mu + sigma))^2) + 1/((mu+sigma)^2)
                   d2ldm1 <- -1/(mu^2) - (y-1)/((mu + sigma)^2)
                   d2ldm2 <- ifelse(y == 0, d2ldm0, d2ldm1)
                   d2ldm2
                 }, #ok
                 dldd = function(y, mu, sigma) {
                   dldd0 <- (exp(mu+sigma) - mu)/(mu + sigma*exp(mu + sigma)) - 1/(mu + sigma)
                   dldd1 <- (y-1)/(mu + sigma) - 1
                   dldd <- ifelse(y == 0, dldd0, dldd1)
                   dldd
                 }, #ok!
                 d2ldd2 = function(y, mu, sigma) {
                   d2ldd0 <- (exp(mu+sigma)*(mu*(sigma + 2) - exp(mu + sigma)) )/((mu + sigma*exp(mu + sigma))^2) + 1/((mu + sigma)^2)
                   d2ldd1 <- -(y - 1)/((mu + sigma)^2)
                   d2ldd2 <- ifelse(y == 0, d2ldd0, d2ldd1)
                   d2ldd2
                 }, #ok!
                 d2ldmdd = function(y, mu, sigma) {
                   d2ldmdd0 <- ((mu - 1)*(sigma + 1)*exp(mu + sigma))/((mu + sigma*exp(mu + sigma))^2) + 1/((mu + sigma)^2)
                   d2ldmdd1 <- -(y-1)/((mu + sigma)^2)        
                   d2ldmdd <- ifelse(y == 0, d2ldmdd0, d2ldmdd1)
                   d2ldmdd
                 }, #ok!
                 G.dev.incr = function(y, mu, sigma, ...) -2 * dZIP3(y, mu, sigma, log = TRUE),
                 rqres = expression(rqres(pfun = "pZIP3", type = "Discrete", 
                                          ymin = 0, y = y, mu = mu, sigma = sigma)), 
                 mu.initial = expression(mu <- rep(mean(y), length(y))), 
                 sigma.initial = expression(sigma <- rep(var(y)/mean(y) - 1, length(y))), 
                 mu.valid = function(mu) all(mu > 0), 
                 sigma.valid = function(sigma) all(sigma > 0),
                 y.valid = function(y) all(y >= 0), 
                 mean = function(mu, sigma) mu, 
                 variance = function(mu, sigma) mu * (1 + sigma)
  ), 
  class = c("gamlss.family", "family"))
} 
################################################################################
################################################################################
################################################################################
################################################################################