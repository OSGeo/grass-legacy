

double /* pargr */ xmin, xmax, ymin, ymax, zmin, zmax;
double /* norm */ xmin0, xmax0, ymin0, ymax0, zmin0, zmax0, delt, dnorm;
double /* MAT */ *A;
double /* PRISP */ fi, rsm, fstar2, alphat, betat, tfsta2, tfstad;

double /* out */ *b, *w;
double /* orig */ x0utm, y0utm;
double /* gcmax */ gmin, gmax, c1min, c1max, c2min, c2max;

int             cursegm;
int             iw2;

char            msg[80];
int             totsegm;

/*******************added for spline and log global var************************/

double      xt[1005],yt[1005], sp[1005][11];
double      yd0, ydm;
double alg[10005], blg[10005];
