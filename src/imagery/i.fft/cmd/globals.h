
/* file names in cell_misc for double arrays */
#define FFTREAL "fftreal"
#define FFTIMAG "fftimag"
#define NDIM 2

#define TWOPI 6.283185307179590 /* Constant value of 2 pi */

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL int Range;
GLOBAL char Cellmap_orig[50], Cellmap_real[50], Cellmap_imag[50];
