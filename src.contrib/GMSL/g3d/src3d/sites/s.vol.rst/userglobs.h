#include "G3d.h"

#ifdef USERMAIN
double /* pargr */ xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax;
double /* norm */ xmin0, xmax0, ymin0, ymax0, zmin0, zmax0, wmin0, wmax0, delt, dnorm;
double /* MAT */ *A;
double /* PRISP */ fi, rsm, fstar2, alphat, betat;

double /* out */ *b, *w;
double /* orig */ x0utm, y0utm, z0utm;
double /* gcmax */ gmin, gmax, c1min, c1max, c2min, c2max, c3min, c3max;
double /* gcmax */ a1min, a1max, a2min, a2max;
float *zero_array1, *zero_array2, *zero_array3, *zero_array4, *zero_array5, *zero_array6, *zero_array7;
int out_cond1,out_cond2;  
double xmn,xmx,ymn,ymx,zmn,zmx;
double z_orig_in,tb_res_in;
int             cursegm;
int             totsegm;
int             iw2;
int             n_rows_in; /* fix by JH 04/24/02 */

char            msg[80];

FILE *dev;
FCELL   *zero_array_cell;
G3D_Region current_region;
#else
extern double /* pargr */ xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax;
extern double /* norm */ xmin0, xmax0, ymin0, ymax0, zmin0, zmax0, wmin0, wmax0, delt, dnorm;
extern double /* MAT */ *A;
extern double /* PRISP */ fi, rsm, fstar2, alphat, betat;

extern double /* out */ *b, *w;
extern double /* orig */ x0utm, y0utm, z0utm;
extern double /* gcmax */ gmin, gmax, c1min, c1max, c2min, c2max, c3min, c3max;
extern double /* gcmax */ a1min, a1max, a2min, a2max;
extern float *zero_array1, *zero_array2, *zero_array3, *zero_array4, *zero_array5, *zero_array6, *zero_array7;
extern int out_cond1,out_cond2;  
extern double xmn,xmx,ymn,ymx,zmn,zmx;
extern double z_orig_in,tb_res_in;

extern int             cursegm;
extern int             totsegm;
extern int             iw2;
extern int             n_rows_in; /* fix by JH 04/24/02 */

extern char            msg[80];

extern FILE *dev;
extern FCELL   *zero_array_cell;
extern G3D_Region current_region;
#endif
