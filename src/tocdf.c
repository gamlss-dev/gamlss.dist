/*
 *  tocdfSIbis.c.c 
 *   R package gamlss.dist by ............ Copyright (C) ......... .
 *   This file is part of the R package gamlss.dist.
 *
 *   The R package gamlss.dist is free software: you can redistribute it and/or modify
 *   it under the terms of the GNU General Public License as published by
 *   the Free Software Foundation, either version 3 of the License, or
 *   (at your option) any later version.
 *
 *   The R package gamlss.dist is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *  Created by Marco Enea on 07/05/14.
 *
 */
#include "tofyHead.h"
#include <math.h>

void tocdf(double *y, double *mu, double *sigma, double *ans, int *ny) {
 	int z1, zny, i, j, lyp1;
  double sumT;
	for (zny = 0; zny < *ny; zny++)
	{
    z1 = *ny - zny - 1;
		lyp1 = y[z1] + 1;
    sumT = 0; 
    double tynew[lyp1];
    double lpnew[lyp1]; 
		tynew[0] = mu[z1]*pow(1+2*sigma[z1]*mu[z1],-0.5);    
  	lpnew[0] = (1 - pow(1+2*sigma[z1]*mu[z1],0.5))/sigma[z1];  
		for (i = 1; i < lyp1; i++)
		{
        tynew[i] = ((sigma[z1]*(2*i-1)/mu[z1])+(1/tynew[i-1]))*(pow(tynew[0],2));
        lpnew[i] = lpnew[i-1] + log(tynew[i-1]) - log(i);
		}
    for (j=0 ;j<lyp1;j++) sumT += exp(lpnew[j]);
		ans[z1] = sumT;
	}
}


