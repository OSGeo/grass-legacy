#define EPS     1.e-7
#define MAXW    7000000

FILE *fdelevin, *fddxin, *fddyin, *fdrain, *fdinfil, *fdtraps, *fdmanin, *fddepth, *fddisch, *fderr, *fdoutwalk,*fdwalkers;
FILE *fdwdepth,*fddetin,*fdtranin,*fdtauin, *fdtc, *fdet, *fdconc, *fdflux,*fderdep;
FILE *fdsfile,*fw;

char *elevin;
char *dxin;
char *dyin;
char *rain;
char *infil;
char *traps;
char *manin;
char *sfile;
char *depth;
char *disch;
char *err;
char *outwalk;
char *mapset;
char *mscale;
char *tserie;

char *wdepth;
char *detin;
char *tranin;
char *tauin;
char *tc;
char *et;
char *conc;
char *flux;
char *erdep;

  struct
  {
    struct Option *elevin,*dxin,*dyin,*rain,*infil,*traps,*manin,*sfile,*depth,*disch,*err,
*outwalk,*nwalk,*niter,*outiter,*density,*diffc,*hmax,*halpha,*hbeta,*wdepth,
*detin,*tranin,*tauin,*tc,*et,*conc,*flux,*erdep;
  } parm;

  struct
  {
    struct Flag *mscale, *tserie;
  } flag;


struct {
    long int is1, is2;
} seed;

struct Cell_head cellhd;

struct Point
{
    double north, east;
    double z1;
};
struct Point *points;
int npoints;
int npoints_alloc;


int input_data(void);
int seeds(long int,long int);
int seedg(long int,long int);
int grad_check(void);
void erod(double **);
void main_loop(void);
int output_data(int,double);    
int output_et(void);
double ulec(void);
double gasdev(void);
double amax1(double,double);
double amin1(double,double);
int min(int,int);
int max(int,int);

double xmin, ymin, xmax, ymax;
double mayy, miyy, maxx, mixx;
int mx, my;
int mx2, my2;

double bxmi,bymi,bxma,byma,bresx,bresy;
int maxwab;
double step,conv;

double frac;
double bxmi, bymi;

float **zz, **cchez;
double **v1, **v2, **slope;
double **gama, **gammas,**si,**inf,**sigma;
float **dc,**tau,**er, **ct, **trap;
float  **dif;

double vavg[MAXW][2], stack[MAXW][3],w[MAXW][3];
int iflag[MAXW];

double hbeta;
int ldemo;
double hhmax, sisum,vmean;
double infsum,infmean;
int maxw, maxwa, nwalk;
double rwalk, bresx, bresy, xrand, yrand;
double stepx, stepy, xp0, yp0;
double chmean, si0, deltap, deldif, cch, hhc,halpha;
double eps;
int maxwab, nstack;
int iterout, mx2o, my2o;
int miter,nwalka,lwwfin;
double timec;
int timesec;
