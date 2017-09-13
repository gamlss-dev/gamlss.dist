/*
 *  tofydel1.c
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
 *
 */

#include "tofyHead.h"
#include <math.h>

void tofydel1(double *y, double *mu,double *sigma, double *nu, double *ans, int *ny, int *maxy) {
	int maxyp1 = *maxy + 1;
	double tofY[maxyp1];
	int iy, i, j;
	double dum;
	for (i = 0; i < *ny; i++)
	{
		iy = y[i]+1;
		tofY[0] = mu[i] * nu[i] + mu[i] * (1-nu[i])/(1+mu[i] * sigma[i] * (1-nu[i]));     
		for (j = 1; j < iy; j++)
		{
			dum = 1 + 1/(mu[i]*sigma[i]*(1-nu[i]));
			tofY[j] = (j+mu[i]*nu[i]+1/(sigma[i]*(1-nu[i]))-(mu[i]*nu[i]*j)/tofY[j-1])/dum;
		}
		ans[i] = tofY[iy-1];
	}
}

