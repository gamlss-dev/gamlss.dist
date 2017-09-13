#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
Check these declarations against the C/Fortran source code.
*/

/* .C calls */
extern void c_st3_d2ldd2(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldddt(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldddv(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldm2(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldmdd(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldmdt(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldmdv(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldt2(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldv2(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_d2ldvdt(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_dldd(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_dldm(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_dldt(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_dldv(void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_dst3(void *, void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_pst3(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void c_st3_qst3(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void cdfSICHEL(void *, void *, void *, void *, void *, void *);
extern void dDPOgetC5_C(void *, void *, void *, void *, void *);
extern void getBI_C2(void *, void *, void *, void *, void *);
extern void tocdf(void *, void *, void *, void *, void *);
extern void tofydel1(void *, void *, void *, void *, void *, void *, void *);
extern void tofydel2(void *, void *, void *, void *, void *, void *, void *);
extern void tofyPIG1(void *, void *, void *, void *, void *, void *);
extern void tofyPIG2(void *, void *, void *, void *, void *, void *);
extern void tofySI1(void *, void *, void *, void *, void *, void *, void *, void *);
extern void tofySI2(void *, void *, void *, void *, void *, void *, void *, void *);
extern void tofySICHEL1(void *, void *, void *, void *, void *, void *, void *, void *, void *);
extern void tofySICHEL2(void *, void *, void *, void *, void *, void *, void *, void *, void *);

static const R_CMethodDef CEntries[] = {
  {"c_st3_d2ldd2",  (DL_FUNC) &c_st3_d2ldd2,  7},
  {"c_st3_d2ldddt", (DL_FUNC) &c_st3_d2ldddt, 7},
  {"c_st3_d2ldddv", (DL_FUNC) &c_st3_d2ldddv, 7},
  {"c_st3_d2ldm2",  (DL_FUNC) &c_st3_d2ldm2,  7},
  {"c_st3_d2ldmdd", (DL_FUNC) &c_st3_d2ldmdd, 7},
  {"c_st3_d2ldmdt", (DL_FUNC) &c_st3_d2ldmdt, 7},
  {"c_st3_d2ldmdv", (DL_FUNC) &c_st3_d2ldmdv, 7},
  {"c_st3_d2ldt2",  (DL_FUNC) &c_st3_d2ldt2,  7},
  {"c_st3_d2ldv2",  (DL_FUNC) &c_st3_d2ldv2,  7},
  {"c_st3_d2ldvdt", (DL_FUNC) &c_st3_d2ldvdt, 7},
  {"c_st3_dldd",    (DL_FUNC) &c_st3_dldd,    7},
  {"c_st3_dldm",    (DL_FUNC) &c_st3_dldm,    7},
  {"c_st3_dldt",    (DL_FUNC) &c_st3_dldt,    7},
  {"c_st3_dldv",    (DL_FUNC) &c_st3_dldv,    7},
  {"c_st3_dst3",    (DL_FUNC) &c_st3_dst3,    8},
  {"c_st3_pst3",    (DL_FUNC) &c_st3_pst3,    9},
  {"c_st3_qst3",    (DL_FUNC) &c_st3_qst3,    9},
  {"cdfSICHEL",     (DL_FUNC) &cdfSICHEL,     6},
  {"dDPOgetC5_C",   (DL_FUNC) &dDPOgetC5_C,   5},
  {"getBI_C2",      (DL_FUNC) &getBI_C2,      5},
  {"tocdf",         (DL_FUNC) &tocdf,         5},
  {"tofydel1",      (DL_FUNC) &tofydel1,      7},
  {"tofydel2",      (DL_FUNC) &tofydel2,      7},
  {"tofyPIG1",      (DL_FUNC) &tofyPIG1,      6},
  {"tofyPIG2",      (DL_FUNC) &tofyPIG2,      6},
  {"tofySI1",       (DL_FUNC) &tofySI1,       8},
  {"tofySI2",       (DL_FUNC) &tofySI2,       8},
  {"tofySICHEL1",   (DL_FUNC) &tofySICHEL1,   9},
  {"tofySICHEL2",   (DL_FUNC) &tofySICHEL2,   9},
  {NULL, NULL, 0}
};

void R_init_gamlss_dist(DllInfo *dll)
{
  R_registerRoutines(dll, CEntries, NULL, NULL, NULL);
  R_useDynamicSymbols(dll, FALSE);
}