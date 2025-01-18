################################################################################
# this functions test for continuous gamlss.family distributions
# the tests are as follows
# i) use the function `checkUsage()` to check if there are  undefined variables 
# ii) use the local test_range() function to check if the range of the `d`,`p`, and `q` functions are  defined properly 
# iii) use the local `test_options_in_qfun()` to check if option in `q` function are working 
# iv)  start a loop at different values of the distribution parameters (depending on the number of parameters) i.e 1,2,3 and 4 
#     and 
#    a) check whether the `d` function is summing up to 1
#    b) whether the `d` and `q` functions produce compatible results
#    c) whether `d` and `p` functions match 
#    d) checking  the tails of the distribution using  in `p` function  
#    e) creates data using the `r` function and fit models using `gamlss()` and `gamlss2()` 
#    to compare the deviances 
#    g) if deferences occurred saves the different data for rechecking
################################################################################
################################################################################
################################################################################
################################################################################
# Mikis : this function is a new version of the old `testContDist`
# 21_11_2007   RS with 500 and mixed(20,500) for  3 and 4 parameter distributions
test_continuous_gamlss_dist<- function(family = "NO", 
                   y.range = c(-Inf,Inf), # the range of the resposnse  
                  mu.range = c(-Inf,Inf), # 
               sigma.range = c(0,Inf),
                  nu.range = c(0,Inf),
                 tau.range = c(0,Inf), 
                    mu.val = c(0,1,10,30), # which values to test
                 sigma.val = c(1,5,10,30),
                    nu.val = c(1,5,10,30),
                   tau.val = c(1,5,10,30),
                         N = 100,         
                      save = TRUE, 
                     trace = TRUE,
                      crit = 0.001 # what difference we are looking
                 )
{
################################################################################
################################################################################
################################################################################ 
################################################################################  
# local function
  test_range <- function(family, lower, upper )
{
     fam  <- as.gamlss.family(family)
    fname <- fam$family[[1]] 
     dfun <- paste0("d", fname)
     pfun <- paste0("p", fname)
     qfun <- paste0("q", fname)
  
cat("-------------------------------------------", "\n")
cat("-----------------START---------------------", "\n")
cat("-------------------------------------------", "\n")
cat("pdf", "\n")
      p <- rep(1,4)
cat(eval(call(dfun, x=lower)),   "\n")
cat(eval(call(dfun, x=lower-1)), "\n")
cat(eval(call(dfun, x=upper)), "  \n")
cat(eval(call(dfun, x=upper+1)), "\n")
cat("------------------------------------------", "\n") 
cat("cdf", "\n")
cat(eval(call(pfun,q=lower)), "\n")
cat(eval(call(pfun,q=lower-1)), "\n")
cat(eval(call(pfun,q=upper)), "\n")
cat(eval(call(pfun,q=upper+1)), "\n")
cat("------------------------------------------", "\n")
cat("q-function", "\n")
cat(eval(call(qfun,p=0)), "\n")
cat(eval(call(qfun,p=1)), "\n")
cat(eval(call(qfun,p=0-1)), "\n")
cat(eval(call(qfun,p=1+1)), "\n")  
cat("-------------------------------------------", "\n")
cat("--------------------END--------------------", "\n")
cat("-------------------------------------------", "\n")
  }  
################################################################################  
test_options_in_qfun <- function(family)
  {
    fam  <- as.gamlss.family(family)
    fname <- fam$family[[1]]
    qfun <- paste0("q", fname) 
cat(eval(call(qfun,-1, log.p=TRUE)), "\n")
cat(eval(call(qfun, exp(-1))), "\n")
cat(eval(call(qfun, exp(-1), lower.tail=FALSE)), "\n")
  }
################################################################################
################################################################################
################################################################################  
################################################################################
## main function  starts here    
require(codetools)
require(gamlss)
require(gamlss2)
################################################################################  
       fam  <- as.gamlss.family(family) # family
    distype <- fam$type 
if (distype!="Continuous") stop("The type of the distribution should be Continuous")
      fname <- fam$family[[1]] # family names 
no.active.par <- sum(as.vector(unlist(fam$parameters[1:fam$nopar])))
     family <- c("None", "None") 
     dorfun <- paste("d",fname,sep="") # say dNO
     porfun <- paste("p",fname,sep="") # say pNO
     qorfun <- paste("q",fname,sep="") # say qNO
     rorfun <- paste("r",fname,sep="") # say rN0
    thedata <- NULL #seq(1,length=N)
theDiffdata <- NULL
     nfails <- 0
    nDfails <- 0
cat("************** the test for", paste(fname), "distribution starts here ****************", "\n") 
cat("----- first using the checkUsage() function ----", "\n")
# checking using codetools #####################################################
checkUsage(eval(parse(text=fname)))
checkUsage(eval(parse(text=dorfun)))
checkUsage(eval(parse(text=porfun)))
checkUsage(eval(parse(text=qorfun)))
checkUsage(eval(parse(text=rorfun)))
cat("----- checkUsage() finished ----", "\n")
################################################################################
cat("----- start checking the range of the functions ----", "\n")
test_range(fname, lower=y.range[1], upper=y.range[2])
cat("----- finish checking the range of the functions ----", "\n")
################################################################################
################################################################################
cat("----- start checking the option of the q-function ----", "\n")
test_options_in_qfun(fname)
cat("----- finish checking the option of the q-function ----", "\n")
################################################################################
################################################################################
# the loop
################################################################################
################################################################################
# one parameter
################################################################################
################################################################################ 
if(no.active.par==1) # one parameter distributions
 {
  for (i in 1:length(mu.val))
   {
if (trace==TRUE)
 {
cat("------------------------------------------------------------------------ \n")
cat("testing for  mu at value", mu.val[i], "\n")
 }
################################################################################
##  test whether it is a proper distribution
##  does it summing up to 1?
   intY <- try(integrate(function(x) eval(call(dorfun, x=x, mu=mu.val[i])), 
                         y.range[1], y.range[2], subdivisions=1000)$value)
  if (any(class(intY)[1]%in%"try-error")) 
    warning(cat( "testing whether pdf sum up to 1: the numerical integral have failed", "at mu", mu.val[i], "\n"))
  else  if (abs(intY-1)> crit) 
   warning( cat( "the numerical integral do NOT add to 1", "at mu", mu.val[i], "\n"))
  else cat("The", fname,  "distribution at mu", mu.val[i], "sums up to one", "\n")   
################################################################################
## checking  whether the cdf and the inverse cdf  (q-function) are compatible 
val <- eval(call(rorfun, n=100,   mu=mu.val[i])) # randomly generate a value
 pr <- eval(call(porfun, q=val, mu=mu.val[i])) #  get the cdf
qq <-  eval(call(qorfun, p=pr,  mu=mu.val[i])) 
if(any(abs(qq-val)>crit)) 
   warning( cat( "the p and q functions of",fname, "are NOT all matching", "at mu", mu.val[i], "\n"))
 else  cat("The p and q function of distribution", fname, "at mu=", mu.val[i], "are compatible", "\n")   
################################################################################
## checking whether pdf and cdf match 
p1 <-try(integrate(function(x) eval(call(dorfun, x=x,mu=mu.val[i])), y.range[1], val[1])$value)
 if (any(class(p1)[1]%in%"try-error")) warning(cat( "the numerical integral have failed", "at mu", mu.val[i], "\n"))
 else 
  { 
  aa <- p1- eval(call(porfun, q=val[1], mu=mu.val[i])) #pNO(val)
  if(abs(aa)>crit)
  warning( cat( "the values of d and p functions do NOT match", "at mu", mu.val[i], "\n"))  
  else
    cat("The d and p values of distribution", fname, "at mu=" , mu.val[i], "are OK", "\n")   
  } 
################################################################################
## checking  the tails in p function 
tt <- eval(call(porfun, q=val, mu=mu.val[i], lower.tail=TRUE))+
      eval(call(porfun, q=val, mu=mu.val[i], lower.tail=FALSE))
if(any(abs(tt-1)>0.0001))
   warning( cat( "for the value", val, "the tails of p functions do not add to 1", "at mu", mu.val[i], "\n")) else 
     cat("The tails of", fname, "at mu=" , mu.val[i], "are OK", "\n")     
     
################################################################################
################################################################################
################################################################################
################################################################################
# fittings distribution to data 
# create data set in the  global envinroment 
dat <<- eval(call(rorfun, N ,mu=mu.val[i])) #
if (trace==TRUE) cat("--fitting gamlss and gamlss2 models-- \n")
m1 <- try(gamlss(dat~1, family=fam, trace=FALSE, n.cyc=100))  
      if (any(class(m1)[1]%in%"try-error"||any(is.na(deviance(m1)))))
         { 
         warning(paste("gamlss failed", "at mu", mu.val[i], "\n"))
      }
cat("gamlss: ", deviance(m1), "\n")
m2 <- try(gamlss2(dat~1, family=fam, trace=FALSE))
cat("gamlss2:", deviance(m2), "\n")
if (any(class(m2)[1]%in%"try-error"||any(is.na(deviance(m2)))))
         { 
         warning(paste("gamlss2 failed", "at mu", mu.val[i], "\n"))
         }
      if (any(class(m2)[1]%in%"try-error"||any(is.na(deviance(m2))))||(any(class(m1)[1]%in%"try-error"||any(is.na(deviance(m1))))))
         { 
         nfails=nfails+1 
         cat("Currrent No. of model fitting fails: ", nfails, "\n")
if (trace==FALSE) cat("---------------------------------------------------------- \n") 
         }
      if (!any(class(m1)%in%"try-error") & !any(class(m2)%in%"try-error"))
         {
      dd <- deviance(m1)-deviance(m2)
if(abs(dd)>crit) # what level we want ?? 
      warning( cat("The deviances for gamlss and gamlss2 do not agree at mu", mu.val[i], "with a difference", dd, "\n"))  
          theDiffdata<-cbind(theDiffdata,dat)
           nDfails <- nDfails+1 
         }
    }
 }
################################################################################
################################################################################
## two parameters
################################################################################
################################################################################
if(no.active.par==2) # two parameter distributions
 {
 for (i in 1:length(mu.val))
   {
   for (j in 1:length(sigma.val))
     {
if (trace==TRUE)
 {
cat("------------------------------------------------------------------------ \n")
cat("testing at mu", mu.val[i], "sigma", sigma.val[j], "\n")
 }
################################################################################
## test whether a proper distribution summing up to 1
##  does it summing up to 1?     
  intY <- try(integrate(function(x) eval(call(dorfun, x=x, mu=mu.val[i], sigma=sigma.val[j])), y.range[1], y.range[2], subdivisions=1000)$value)
if (any(class(intY)[1]%in%"try-error")) 
    warning(cat("testing whether pdf sum up to 1: the numerical integral have failed", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))
  else  if (abs(intY-1)> crit) 
    warning( cat(" the numerical integral do NOT add to 1", "at mu", mu.val[i], "and sigma", sigma.val[j],"\n")) else 
      cat("The", fname,"distribution at mu", mu.val[i], "and sigma",  sigma.val[i],"sums up to one", "\n") 
   
################################################################################
  ## checking  whether the cdf and the inverse cdf  (q-function) are compatible
val <- eval(call(rorfun, n=1,mu=mu.val[i],sigma=sigma.val[j])) # randomly generate a value
 pr <- eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j])) #  get the cdf
 qq <-  eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j])) 
if(abs(qq-val)>0.0001) 
   warning( cat( "the p and q functions are not matching", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n")) else  
    cat("The p and q function of distribution", fname, "at mu=", mu.val[i],"and sigma=",  sigma.val[i], "are compatible", "\n")  
################################################################################
## checking whether pdf and cdf match 
p1 <- try(integrate(function(x) eval(call(dorfun, x=x,mu=mu.val[i], sigma=sigma.val[j])), y.range[1], val)$value)
 if (any(class(p1)[1]%in%"try-error")) warning(cat( "the numerical integral have failed", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))
   else
    { 
 aa <- p1-eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j])) #pNO(val)
 if (abs(aa)>crit)
 warning( cat( "for the value", val, "the d and p functions do not match", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))  
 else
   cat("The d and p values of distribution", fname, "at mu=", mu.val[i],"and sigma=", sigma.val[i], "are OK", "\n") 
    } 
 
################################################################################
## checking whether tails in p function 
tt <- eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], lower.tail=TRUE))+
      eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], lower.tail=FALSE))
if(abs(tt-1)>0.0001)
   warning( cat( "for the value", val, "the tails of p functions do not add to 1", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))  else
     cat("The tails of", fname, "at mu=", mu.val[i],"and sigma=",sigma.val[i], "are OK", "\n")
################################################################################
## fitting data  
## create data set in the  global environment 
dat <<- dat<-eval(call(rorfun, N ,mu=mu.val[i], sigma=sigma.val[j])) #create data
if (trace==TRUE) cat("--fitting models-- \n")
m1 <- try(gamlss(dat~1, family=fam, trace=FALSE, n.cyc=100))  
      if (any(class(m1)[1]%in%"try-error"||any(is.na(deviance(m1)))))
         { 
         warning(paste("gamlss method RS()  failed", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))
      }
cat("gamlss: ", deviance(m1), "\n")
m2 <- try(gamlss2(dat~1, family=fam, trace=FALSE))
cat("gamlss2:", deviance(m2), "\n")
if (any(class(m2)[1]%in%"try-error"||any(is.na(deviance(m2)))))
         { 
         warning(paste("gamlss method mixed()  failed", "at mu", mu.val[i], "and sigma", sigma.val[j], "\n"))
         }
      if (any(class(m2)[1]%in%"try-error"||any(is.na(deviance(m2))))||(any(class(m1)[1]%in%"try-error"||any(is.na(deviance(m1))))))
         { 
         thedata<-cbind(thedata,dat) 
         nfails=nfails+1 
            cat("Currrent No. of model fitting fails: ", nfails, "\n")
          if (trace==FALSE) cat("---------------------------------------------------------- \n") 
         }
      if (!any(class(m1)[1]%in%"try-error") & !any(class(m2)[1]%in%"try-error"))
         {
      dd <- deviance(m1)-deviance(m2)
   if(abs(dd)>crit) # what level we want ?? 
      warning( cat("The deviances for gamlss and gamlss2 do not agree at mu", mu.val[i], "and sigma", sigma.val[j], "with a difference", dd, "\n"))    
   if(abs(dd)>0.5)
          {
   theDiffdata <- cbind(theDiffdata,dat) 
       nDfails <- nDfails+1 
          }
         }
      }
    }
  }  
################################################################################
################################################################################
## three parameters
################################################################################
################################################################################ 
if(no.active.par==3) # three parameter distributions
 {
 for (i in 1:length(mu.val))
   {
   for (j in 1:length(sigma.val))
     {
     for (k in 1:length(nu.val))
       {
################################################################################
if (trace==TRUE)
 {
cat("--------------------------------------------------------------------------- \n")
cat("testing at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k], "\n")
}
################################################################################       
## test whether a proper distribution summing up to 1
intY <- try(integrate(function(x) eval(call(dorfun, x=x, mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k])), y.range[1], y.range[2], subdivisions=1000)$value)
if (any(class(intY)[1]%in%"try-error")) warning(cat( "testing whether pdf sum up to 1: the numerical integral have failed", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],"\n"))
else  if (abs(intY-1)> crit) 
   warning( cat( "testing whether pdf sum up to 1: the numerical integral do not add to 1", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],"\n"))
     else 
cat("The",fname,"distribution at mu", mu.val[i], "sigma", sigma.val[i], "and nu", nu.val[i], "sums up to one", "\n") 
################################################################################
## checking the whether the cdf and yje inverse cdf are inversable 
##randomly generate a value
val <- eval(call(rorfun, n=1,mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k])) 
 pr <- eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k])) #  get the cdf
qq <-  eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k])) 
if(abs(qq-val)>0.0001) 
   warning( cat( "The p and q functions are not matching", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],  "\n")) else 
     cat("The p and q functions of", fname, "at mu", mu.val[i], "sigma", sigma.val[i], "and nu", nu.val[i], "sums up to one", "\n")   
################################################################################
## checking whether pdf and cdf match 
p1 <-try(integrate(function(x) eval(call(dorfun, x=x,mu=mu.val[i], sigma=sigma.val[j],nu=nu.val[k])), y.range[1], val)$value)
if (any(class(p1)[1]%in%"try-error")) warning(cat( "the numerical integral have failed", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],"\n"))
else {
aa <- p1- eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k])) #pNO(val)
if(abs(aa)>0.0001)
   warning( cat( "for the value", val, "the d and p functions do NOT match", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k], "\n"))  else 
     cat( "The d and p functions of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "and nu",
     nu.val[k], "are OK", "\n")
   }
################################################################################ 
## checking whether tails in p function 
tt <- eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], lower.tail=TRUE))+
      eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], lower.tail=FALSE))
if(abs(tt-1)>0.0001)
   warning( cat( "for the value", val, "the tails of p functions do  add to 1", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],  "\n"))  else 
     cat( "The tails of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "and nu",
          nu.val[k], "are OK", "\n") 
     
################################################################################
# fittings distribution to data 
# create data set in the  global environment 
dat <<- dat<-eval(call(rorfun, N ,mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k])) #create data

if (trace==TRUE) cat("--fitting models-- \n")
m1 <- try(gamlss(dat~1, family=fam, trace=FALSE, n.cyc=500))  
      if (any(class(m1)[1]%in%"try-error"||any(is.na(deviance(m1)))))
         { 
          warning(paste("gamlss method RS()  failed", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],  "\n"))
      }
cat("gamlss : ", deviance(m1), "\n")
m2 <- try(gamlss(dat~1, family=fam, trace=FALSE, method=mixed(20,500)))
cat("gamlss2: ", deviance(m2), "\n")
if (any(class(m2)[1]%in%"try-error"||any(is.na(deviance(m2)))))
         { 
         warning(paste("gamlss method mixed()  failed", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k], "\n"))
         }
      if (any(class(m2)[1]%in%"try-error"||any(is.na(deviance(m2))))||(any(class(m1)[1]%in%"try-error"||any(is.na(deviance(m1))))))
         { 
         thedata<-cbind(thedata,dat) 
         nfails=nfails+1
           cat("Currrent No. of model fitting fails: ", nfails, "\n")
         if (trace==FALSE) cat("---------------------------------------------------------- \n") 
         }
      if (!any(class(m1)[1]%in%"try-error") & !any(class(m2)[1]%in%"try-error"))
         {
         dd <- deviance(m1)-deviance(m2)
         if(abs(dd)>crit) # what level we want ?? 
         warning( cat("The deviances for methods RS() and mixed() do not agree at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],  "with a difference", dd, "\n"))  
         if(abs(dd)>0.5)
          {
           theDiffdata<-cbind(theDiffdata,dat) 
          nDfails=nDfails+1 
          }
         }
       }
      }
    }
  }  
################################################################################
################################################################################
## four parameters
################################################################################
################################################################################
if(no.active.par==4) # four parameter distributions
 {
 for (i in 1:length(mu.val))
   {
   for (j in 1:length(sigma.val))
     {
     for (k in 1:length(nu.val))
       {
       for (l in 1:length(tau.val))
         {
################################################################################
if (trace==TRUE)
 {
cat("--------------------------------------------------------------------------- \n")
cat("testing at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l],"\n")
 }
################################################################################
## test whether a proper distribution summing up to 1
  intY <- try(integrate(function(x) eval(call(dorfun, x=x, mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])), y.range[1], y.range[2], subdivisions=1000)$value)
if (any(class(intY)[1]%in%"try-error")) warning(cat( "testing whether pdf sum up to 1: the numerical integral have failed", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "\n"))
  else  if (abs(intY-1)> crit) 
  warning( cat( "The numerical integral do not add to 1", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],"and tau", tau.val[l], "\n"))
  else 
    cat("The", fname, "distribution at mu", mu.val[i], "sigma", sigma.val[i], "nu", nu.val[i],
        "and tau", tau.val[i], "sums up to one","\n")     
################################################################################
## checking the whether the cdf an inverse cdf are comparable 
val <- eval(call(rorfun, n=1,mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) # randomly generate a value
 pr <- eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) #  get the cdf
qq <-  eval(call(qorfun, p=pr, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) 
if(abs(qq-val)>0.0001) 
   warning( cat( "the p and q functions are NOT matching", "at mu", mu.val[i], "sigma", sigma.val[j], "and nu", nu.val[k],  "\n")) else 
    cat( "The d and p functions of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "nu",
          nu.val[k], "and tau", tau.val[k], "are OK", "\n")
################################################################################
## checking whether pdf and cdf match 
p1 <-try(integrate(function(x) eval(call(dorfun, x=x,mu=mu.val[i], sigma=sigma.val[j],nu=nu.val[k], tau=tau.val[l])), y.range[1], val)$value)
if (any(class(p1)[1]%in%"try-error")) warning(cat( "the numerical integral have failed", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k], "and tau", tau.val[l],"\n"))
else    {
aa <- p1- eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l])) 
if(abs(aa)>0.0001)
   warning( cat( "for the value", val, "the d and p functions do not match", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k], "and tau", tau.val[l], "\n"))  else 
     cat( "The d and p functions of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "nu",
          nu.val[k], "and tau", tau.val[l], "are OK", "\n")   
}
################################################################################
## checking  tails in p function 
tt <- eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], lower.tail=TRUE))+
      eval(call(porfun, q=val, mu=mu.val[i],sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l], lower.tail=FALSE))
if(abs(tt-1)>0.0001)
   warning( cat( "for the value", val, "the tails of p functions do NOT add to 1", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "\n"))  else 
    cat( "The tails of", fname, "at mu", mu.val[i], "sigma", sigma.val[j], "nu",
            nu.val[k], "and tau", nu.val[k],"are OK", "\n")   
################################################################################
## fitting data  
## create data 
dat<<-dat<-eval(call(rorfun, N ,mu=mu.val[i], sigma=sigma.val[j], nu=nu.val[k], tau=tau.val[l]))
if (trace==TRUE) cat("--fitting models-- \n")
 m1 <- try(gamlss(dat~1, family=fam, trace=FALSE, n.cyc=500))  
if (any(class(m1)[1]%in%"try-error"||any(is.na(deviance(m1)))))
          { 
          warning(paste("gamlss method RS()  failed", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "\n"))
} else cat("gamlss : ", deviance(m1), "\n") 
m2 <- try(gamlss(dat~1, family=fam, trace=FALSE, method=mixed(20,500)))
if (any(class(m2)[1]%in%"try-error"||any(is.na(deviance(m2)))))
          { 
          warning(paste("gamlss method mixed()  failed", "at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "\n"))
          } else cat("gamlss2: ", deviance(m2), "\n") 
if (any(class(m2)[1]%in%"try-error"||any(is.na(deviance(m2))))||(any(class(m1)[1]%in%"try-error"||any(is.na(deviance(m1))))))
          { 
         thedata<-cbind(thedata,dat) 
         nfails=nfails+1 
             cat("Currrent No. of model fitting fails: ", nfails, "\n")
          if (trace==FALSE) cat("---------------------------------------------------------- \n") 
          }
      if (!any(class(m1)[1]%in%"try-error") & !any(class(m2)[1]%in%"try-error"))
         {
      dd <- deviance(m1)-deviance(m2)
   if(abs(dd)>crit) # what level we want ?? 
      warning( cat("The deviances for methods RS() and mixed() do not agree at mu", mu.val[i], "sigma", sigma.val[j], "nu", nu.val[k],"and tau", tau.val[l], "with a difference", dd, "\n"))  
      if(abs(dd)>0.5)
          {
           theDiffdata <-cbind(theDiffdata,dat) 
            nDfails <- nDfails+1 
          }   
         }
        }
       }
      }
    }
  }  
################################################################################
################################################################################
# PLOTS
no.active.par
if (no.active.par==2&&y.range[1]==-Inf&&y.range[2]==Inf)
  contR_2_12(fam)
if (no.active.par==3&&y.range[1]==-Inf&&y.range[2]==Inf)
  contR_3_11(fam)
if (no.active.par==4&&y.range[1]==-Inf&&y.range[2]==Inf)
  contR_4_13(fam)
if (no.active.par==2&&y.range[1]==0&&y.range[2]==Inf)
  contRplus_2_11(fam)
if (no.active.par==3&&y.range[1]==0&&y.range[2]==Inf)
  contRplus_3_13(fam)
if (no.active.par==4&&y.range[1]==0&&y.range[2]==Inf)
  contRplus_4_33(fam)
if (no.active.par==2&&y.range[1]==0&&y.range[2]==1)
  contR01_2_13(fam)
if (no.active.par==4&&y.range[1]==0&&y.range[2]==1)
  contR01_4_33(fam)
################################################################################
################################################################################
# exit 
on.exit(rm(dat, envir=.GlobalEnv))
if (save==TRUE&&nfails>0)  thedata<<-data.frame(thedata)
if (save==TRUE&&nDfails>0) theDdata<<-data.frame(theDiffdata)
cat("----------------------------------------------------", "\n")
cat("number of failed model fits", nfails, "\n")
if (nfails>0) cat("The data for failed models are saved in data.frame thedata \n")
cat("----------------------------------------------------", "\n")
cat("The number of models in which gamlss and gamls2 methods differ is", nDfails, "\n")
if (nDfails>0) cat("The data for failed models are saved in data.frame theDdata \n")
cat("************** the test for", paste(fname), "distribution finish here ****************", "\n") 
}
################################################################################
################################################################################
################################################################################
################################################################################
################################################################################
