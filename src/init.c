#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP as_HLS(SEXP, SEXP, SEXP);
extern SEXP as_HSV(SEXP, SEXP, SEXP);
extern SEXP as_LAB(SEXP, SEXP, SEXP);
extern SEXP as_LUV(SEXP, SEXP, SEXP);
extern SEXP as_RGB(SEXP, SEXP, SEXP);
extern SEXP as_XYZ(SEXP, SEXP, SEXP);
extern SEXP as_polarLAB(SEXP, SEXP, SEXP);
extern SEXP as_polarLUV(SEXP, SEXP, SEXP);
extern SEXP as_sRGB(SEXP, SEXP, SEXP);
extern SEXP hex_to_RGB(SEXP, SEXP);
extern SEXP sRGB_to_RColor(SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"as_HLS",         (DL_FUNC) &as_HLS,         3},
    {"as_HSV",         (DL_FUNC) &as_HSV,         3},
    {"as_LAB",         (DL_FUNC) &as_LAB,         3},
    {"as_LUV",         (DL_FUNC) &as_LUV,         3},
    {"as_RGB",         (DL_FUNC) &as_RGB,         3},
    {"as_XYZ",         (DL_FUNC) &as_XYZ,         3},
    {"as_polarLAB",    (DL_FUNC) &as_polarLAB,    3},
    {"as_polarLUV",    (DL_FUNC) &as_polarLUV,    3},
    {"as_sRGB",        (DL_FUNC) &as_sRGB,        3},
    {"hex_to_RGB",     (DL_FUNC) &hex_to_RGB,     2},
    {"sRGB_to_RColor", (DL_FUNC) &sRGB_to_RColor, 2},
    {NULL, NULL, 0}
};

void R_init_colorspace(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}
