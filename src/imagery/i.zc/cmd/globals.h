
#define NDIM 2

#define TWOPI 6.283185307179590 /* Constant value of 2 pi */

/* fft constants */
#define FORWARD 1
#define INVERSE -1
#define SCALE 1
#define NOSCALE 0

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL double Thresh;
GLOBAL int NumOrients;
