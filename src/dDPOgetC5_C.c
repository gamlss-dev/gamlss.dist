#include <math.h>
#include <Rmath.h>
/* remember that
          as.double(mu): vector of length lmu
       as.double(sigma): vector of length lmu
          as.integer(y): vector of length ly
        as.integer(lmu): scalar integer, it is equal to length(mu)
         as.integer(ly): scalar integer, it is equal to maxV+1
        ans=double(lmu): vector of length lmu
         
  
getC <- function(mu, sigma, maxV)
{
     ly <- length(mu)
   theC <- rep(0, ly) 
      y <- 0:maxV
for (i in 1:(ly))
  {
   logofy <- ifelse(y==0,1,log(y))	
  theC[i] <- sum(exp(-0.5*log(sigma[i])-mu[i]/sigma[i]-lgamma(y+1)+
              y*logofy-y+ (y*log(mu[i]))/sigma[i]+y/sigma[i]-
              (y*logofy)/sigma[i]))#+TheCat0	
  }
theC<-1/theC
theC 	
}         
*/
void dDPOgetC5_C(double *mu, double *sigma, int *lmu, int *ly, double *ans) {
  double  ylogofy[*ly], sumC, mus, lmus, lsig2, invs, lga[*ly], ym[*ly], ls;
  int i,j;
  for (j=0 ; j <*ly ; j++) {
    ylogofy[j] = j * ((j==0)? 1 : log(j));
    lga[j] = lgamma(j + 1);
    ym[j] = (j - ylogofy[j]);
  }  
  for (i=0; i<*lmu; i++){
    sumC = 0;
    mus = mu[i] / sigma[i];
    lsig2 = -0.5 * log(sigma[i]);
    lmus = log(mu[i]) / sigma[i] - 1;
    invs = 1 / sigma[i];
    ls = lsig2 - mus;
    for (j=0 ; j <*ly ; j++){
      sumC += exp(ls - lga[j] + ylogofy[j] + j * lmus + invs * ym[j]);   
    }       
    ans[i] = pow(sumC,-1); 
  }	
}
