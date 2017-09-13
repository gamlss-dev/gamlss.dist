//
//  getBI_C2.c
//  
//
//  Created by Marco Enea on 23/07/17.
//
//

#include "getBI_C2.h"
#include <R.h>
#include <math.h>
#include <Rmath.h>
/* The R function
 
 
 getBI_C2 <- function( mu, sigma,  bd)
 {
 ly <- max(length(bd),length(mu),length(sigma))
 bd <- rep(bd, length = ly)
 sigma <- rep(sigma, length = ly)
 mu <- rep(mu, length = ly)
 theC <- rep(0, ly)
 for (i in 1:ly)
 {
 x <- 0:bd[i]
 logofx <- ifelse(x==0,1,log(x))
 logofbd_x <- ifelse(bd[i]==x,1,log(bd[i]-x))
 ss <- lchoose(bd[i],x)+x*logofx+(bd[i]-x)*logofbd_x-bd[i]*log(bd[i])+
 bd[i]*sigma[i]*log(bd[i]) + sigma[i]*x*log(mu[i])+sigma[i]*(bd[i]-x)*log(1-mu[i])-
 sigma[i]*x*logofx - sigma[i]*(bd[i]-x)*logofbd_x
 expss <- exp(ss)
 theC[i] <- log(1/sum(expss))
 }
 theC
 }
*/

/* The .c function */
void getBI_C2(double *mu, double *sigma, double *bd, int *ly, double *theC) {
    int i,j,n;
    for (i = 0; i < *ly; i++){
        double sumC=0;
        double sm1 = sigma[i] - 1, s_1=1 - sigma[i];
        n = bd[i];
        double nlogn = n * ((n==0)? 1 : log(n));
        double logchoose[n], ylogofy[n], nylogofny[n], ylogmu[n],
               nylog1mu[n], ss[n], expss[n];
        int bdj;
      for (j = 0; j <= n; j++) {
        bdj = n - j;
        logchoose[j] = lgamma(n + 1) - lgamma(j + 1) - lgamma(bdj + 1);
        ylogofy[j] = j * ((j == 0)? 1 : log(j));
        nylogofny[j] = bdj * ((bdj == 0)? 1 : log(bdj));
        ylogmu[j] = j * log(mu[i]);
        nylog1mu[j] = bdj * log(1 - mu[i]);
        ss[j] = logchoose[j]  + (ylogofy[j] + nylogofny[j]) * s_1 +
                  nlogn * sm1 + (ylogmu[j] + nylog1mu[j]) * sigma[i];
        expss[j] = exp(ss[j]);
        sumC += expss[j];
      }
    theC[i] = -log(sumC);
    }
}


