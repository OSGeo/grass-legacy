
#define NDIM 2

#define TWOPI 6.283185307179590 /* Constant value of 2 pi */

/* fft constants */
#define FORWARD 1
#define INVERSE -1
#define SCALE 1
#define NOSCALE 0
#define POINTS 1000

#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif

GLOBAL double Thresh;
GLOBAL int NumOrients;
GLOBAL char *OUTPUT_MAP;

/* class.c */
int class(double *, int);
/* del2g.c */
int del2g(double *[2], int, double);
/* fft.c */
int fft(int, double *[2], int, int, int);
/* fft_curve.c */
void fft_curve(double [], int, int);
/* findzc.c */
int findzc(double [], int, double [], double);
/* getg.c */
int getg(double, double *[2], int);
/* max_pow2.c */
long max_pow2(long);
long min_pow2(long);
/* mult.c */
int mult(double *[2], int, double *[2], int, double *[2], int);
/* polint.c */
void polint(double [], double [], int, double, double *, double *);
/* ratint.c */
void ratint(double [], double [], int, double, double *, double *);
/* splint.c */
void splint(double [], double [], double [], int, double, double *);
void spline(double [], double [], int, double, double, double []);
/* zc_curve.c */
int zc_curve(int, int, double, double [POINTS], double [POINTS], int *, double *);
