


double /* pargr */ xmin, xmax, ymin, ymax, zmin, zmax, wmin, wmax;
double /* norm */ xmin0, xmax0, ymin0, ymax0, zmin0, zmax0, wmin0, wmax0, delt, dnorm;
double /* MAT */ *A;
double /* PRISP */ fi, rsm, fstar2, alphat, betat;

double /* out */ *b, *w;
double /* orig */ x0utm, y0utm, z0utm;
double /* gcmax */ gmin, gmax, c1min, c1max, c2min, c2max, c3min, c3max;
double /* gcmax */ a1min, a1max, a2min, a2max;

int             cursegm;
int             iw2;
int             n_rows_in; /* fix by JH 04/24/02 */

char            msg[80];
int             totsegm;

float *zero_array1, *zero_array2, *zero_array3, *zero_array4, *zero_array5, *zero_array6, *zero_array7;
FCELL   *zero_array_cell;
int out_cond1,out_cond2;  
double z_orig_in,tb_res_in;
