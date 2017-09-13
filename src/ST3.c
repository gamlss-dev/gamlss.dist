/*################################################################################
##
##   R package gamlss.dist by ............ Copyright (C) ......... .
##   This file is part of the R package gamlss.dist.
##
##   The R package gamlss.dist is free software: you can redistribute it and/or modify
##   it under the terms of the GNU General Public License as published by
##   the Free Software Foundation, either version 3 of the License, or
##   (at your option) any later version.
##
##   The R package gamlss.dist is distributed in the hope that it will be useful,
##   but WITHOUT ANY WARRANTY; without even the implied warranty of
##   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
##   GNU General Public License for more details.
##
##   C Code by Alexios Ghalanos 2013
#################################################################################*/

# include "ST3.h"
# include <R.h>
# include <limits.h>
# include <math.h>
# include <Rmath.h>

double st3_dldm(const double y, const double mu, const double sigma, const double nu, const double tau)
{
	double e, s1, s2, dsq1, dsq2, w1, w2, ans;
	s1 = sigma/nu;
	s2 = sigma*nu;
	e  = y-mu;
	dsq1 = (e/s1)*(e/s1);
	dsq2 = (e/s2)*(e/s2);
	w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1) : 1.0;
    w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2) : 1.0;
	ans = (e < 0)? (w1*e)/(s1*s1) : (w2*e)/(s2*s2);
	return ans;
}
void c_st3_dldm(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_dldm(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}

double st3_d2ldm2(const double y, const double mu, const double sigma, const double nu, const double tau)
{
	double ans = st3_dldm(y,mu,sigma,nu,tau);
	ans =  -1.0*ans*ans;
	ans = (ans < -1E-15)? ans: -1E-15;
	return ans;
}
void c_st3_d2ldm2(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldm2(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}

double st3_dldd(const double y, const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2): 1.0;
	double ans = (y < mu)? (w1*dsq1-1.0)/(sigma) : (w2*dsq2-1.0)/(sigma);
	return ans;
}
void c_st3_dldd(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_dldd(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_d2ldd2(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double ans = st3_dldd(y,mu,sigma,nu,tau);
	ans =  -1.0*ans*ans;
	ans = (ans < -1E-15)? ans: -1E-15;
	return ans;
}
void c_st3_d2ldd2(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldd2(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_dldv(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2): 1.0;
	double dldv = (y < mu)? -1.0*(w1*dsq1-1.0)/(nu) : (w2*dsq2+1.0)/(nu);
	dldv = dldv - 2.0*nu/(1.0+nu*nu);
	return dldv;
}
void c_st3_dldv(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_dldv(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_d2ldv2(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double ans = st3_dldv(y,mu,sigma,nu,tau);
	ans =  -1.0*ans*ans;
	ans = (ans < -1E-15)? ans: -1E-15;
	return ans;
}
void c_st3_d2ldv2(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldv2(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_dldt(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2): 1.0;
	double dldta = -0.5*log(1.0+dsq1/tau)+(w1*dsq1-1.0)/(2.0*tau);
	double dldtb = -0.5*log(1.0+dsq2/tau)+(w2*dsq2-1.0)/(2.0*tau);
	double dldt = (y < mu)? dldta : dldtb;
	dldt = dldt+0.5*digamma((tau+1.0)/2.0)-0.5*digamma(tau/2.0);
	return dldt;
}
void c_st3_dldt(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_dldt(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_d2ldt2(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double ans = st3_dldt(y,mu,sigma,nu,tau);
	ans =  -1.0*ans*ans;
	ans = (ans < -1E-15)? ans: -1E-15;
	return ans;
}
void c_st3_d2ldt2(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldt2(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}

double st3_d2ldmdd(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2): 1.0;
    double dldm = (y < mu)? (w1*(y-mu))/(s1*s1): (w2*(y-mu))/(s2*s2);
    double dldd = (y < mu)? (w1*dsq1-1.0)/(sigma): (w2*dsq2-1.0)/(sigma);
    double ans = -1.0*dldm*dldd;
	return ans;
}
void c_st3_d2ldmdd(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldmdd(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_d2ldmdv(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2): 1.0;
    double dldm = (y < mu)? (w1*(y-mu))/(s1*s1): (w2*(y-mu))/(s2*s2);
    double dldv = (y < mu)? (w1*dsq1-1.0)/(nu): (w2*dsq2+1.0)/(nu);
    dldv = dldv - 2.0*nu/(1.0+nu*nu);
    double ans = -1.0*dldm*dldv;
	return ans;
}
void c_st3_d2ldmdv(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldmdv(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_d2ldmdt(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2): 1.0;
	double dldm = (y<mu)?(w1*(y-mu))/(s1*s1): (w2*(y-mu))/(s2*s2);
	double dldta = -0.5*log(1.0+dsq1/tau)+(w1*dsq1-1.0)/(2.0*tau);
	double dldtb = -0.5*log(1.0+dsq2/tau)+(w2*dsq2-1.0)/(2.0*tau);
	double dldt = (y<mu)? dldta: dldtb;
	dldt = dldt + 0.5*digamma((tau+1.0)/2.0)-0.5*digamma(tau/2.0);
    double ans = -1.0*dldm*dldt;
	return ans;
}
void c_st3_d2ldmdt(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldmdt(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_d2ldddv(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2): 1.0;
	double dldd = (y < mu)? (w1*dsq1-1.0)/(sigma) : (w2*dsq2-1.0)/sigma;
	double dldv = (y < mu)? -1.0*(w1*dsq1-1.0)/(nu) : (w2*dsq2+1.0)/nu;
	dldv = dldv - 2.0*nu/(1.0+nu*nu);
	double ans = -1.0*(dldd*dldv);
	return ans;
}
void c_st3_d2ldddv(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldddv(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}

double st3_d2ldddt(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1)/(tau+dsq2): 1.0;
	double dldd = (y < mu)? (w1*dsq1-1.0)/(sigma) : (w2*dsq2-1.0)/sigma;
	double dldta = -0.5*log(1.0+dsq1/tau)+(w1*dsq1-1.0)/(2.0*tau);
	double dldtb = -0.5*log(1.0+dsq2/tau)+(w2*dsq2-1.0)/(2.0*tau);
	double dldt = (y < mu)? dldta : dldtb;
	dldt  = dldt+0.5*digamma((tau+1.0)/2.0)-0.5*digamma(tau/2.0);
	double ans = -1.0*(dldd*dldt);
	return ans;
}
void c_st3_d2ldddt(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldddt(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_d2ldvdt(const double y,const double mu, const double sigma, const double nu, const double tau)
{
	double s1 = sigma/nu;
	double s2 = sigma*nu;
	double res2 = (y-mu)*(y-mu);
	double dsq1 = res2/(s1*s1);
	double dsq2 = res2/(s2*s2);
	double w1 = (tau < 1000000)? (tau+1.0)/(tau+dsq1): 1.0;
	double w2 = (tau < 1000000)? (tau+1.0)/(tau+dsq2): 1.0;
	double dldv = (y < mu)? -(w1*dsq1-1.0)/(nu) : (w2*dsq2+1.0)/nu;
	dldv = dldv - 2.0*nu/(1.0+nu*nu);
	double dldta = -0.5*log(1.0+dsq1/tau)+(w1*dsq1-1.0)/(2.0*tau);
	double dldtb = -0.5*log(1.0+dsq2/tau)+(w2*dsq2-1.0)/(2.0*tau);
	double dldt = (y < mu)? dldta : dldtb;
	dldt  = dldt+0.5*digamma((tau+1.0)/2.0)-0.5*digamma(tau/2.0);
	double ans = -1.0*(dldv*dldt);
	return ans;
}
void c_st3_d2ldvdt(double *y, double *mu,double *sigma, double *nu, double *tau, double *ans, int *n)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_d2ldvdt(y[i], mu[i], sigma[i], nu[i], tau[i]);
	}
}
double st3_dst3(const double x,const double mu, const double sigma, const double nu, const double tau, const int logr)
{
	double nu2 = nu*nu;
	double e =  x-mu;
	double z = e/sigma;
	double loglik1a = dt((nu*z), tau, 1);
	double loglik2a = dt(e/(sigma*nu), tau, 1);
	double loglika = (e < 0)? loglik1a : loglik2a;
	loglika = loglika+log(2.0*nu/(1.0+nu2)) - log(sigma);
	double loglik1b = dnorm((nu*z), 0, 1, 1);
	double loglik2b = dnorm(e/(sigma*nu), 0, 1, 1);
	double loglikb = (e < 0)? loglik1b : loglik2b;
	loglikb = loglikb+log(2.0*nu/(1.0+nu2)) - log(sigma);
	double loglik = (tau<1000000)? loglika : loglikb;
	if(logr==0) loglik = exp(loglik);
	return loglik;
}

void c_st3_dst3(double *x,double *mu, double *sigma, double *nu, double *tau, double *ans, int *n, int* logr)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_dst3( x[i], mu[i], sigma[i], nu[i], tau[i], logr[0]);
	}
}

double st3_pst3(const double q,const double mu, const double sigma, const double nu, const double tau, const int lower_tail, const int logr)
{
	double nu2 = nu*nu;
	double e =  q-mu;
	double cdf1 = 2.0*pt(nu*e/sigma, tau, 1, 0);
	double cdf2 = 1 + 2.0*nu2*(pt(e/(sigma*nu), tau, 1, 0)-0.5);
	double cdf = (e<0)? cdf1 : cdf2;
	cdf = cdf/(1.0+nu2);
	cdf = (lower_tail==1)? cdf : 1.0-cdf;
	cdf = (logr==1)?  log(cdf): cdf;
	return cdf;
}

void c_st3_pst3(double *q,double *mu, double *sigma, double *nu, double *tau, double *ans, int *n, int *lower_tail, int* logr)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_pst3( q[i], mu[i], sigma[i], nu[i], tau[i], lower_tail[0], logr[0]);
	}
}


double st3_qst3(const double p,const double mu, const double sigma, const double nu, const double tau, const int lower_tail, const int logr)
{
	double nu2 = nu*nu;
	double xp = (logr==1)? exp(p): p;
	xp = (lower_tail==1)? xp: 1-xp;
	double q1 = mu+(sigma/nu)*qt(xp*(1.0+nu2)/2, tau, 1, 0);
	double q2 = mu+(sigma*nu)*qt((xp*(1.0+nu2)-1.0)/(2.0*nu2) + 0.5, tau, 1, 0);
	double q = (xp<(1.0/(1.0+nu2)))? q1 : q2;
	return q;
}

void c_st3_qst3(double *p,double *mu, double *sigma, double *nu, double *tau, double *ans, int *n, int *lower_tail, int* logr)
{
	int i;
	for(i=0;i<*n;i++)
	{
		ans[i] = st3_qst3( p[i], mu[i], sigma[i], nu[i], tau[i], lower_tail[0], logr[0]);
	}
}

