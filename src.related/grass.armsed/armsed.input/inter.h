#include "gis.h"

#define COHM     .00012
#define DPOW     2.0
#define Taock    .047
#define AGB      .056
#define BEX      1.5
#define DCOEFF   .001

EXTERN int old_flag;
EXTERN int wat_flag;
EXTERN int stats_flag;
EXTERN char plane[5];

EXTERN double chlength;
EXTERN double chslope;

EXTERN double olslope[2];
EXTERN double flow[2];
EXTERN float porosity[2];
EXTERN float Kw[2];
EXTERN float Yc[2];
EXTERN float Dof[2];
EXTERN float ChDof;
EXTERN float K[2];
EXTERN int plasticity[2];
EXTERN float finer1[2];
EXTERN float finer2[2];
typedef struct
{
    float sed_size;
    float per_finer;
} SED;
EXTERN SED *sed;

EXTERN float cover[6];

EXTERN double Tc[2];
EXTERN double a[2];
EXTERN double b[2];
EXTERN double z[2];

EXTERN double ADW[2];
EXTERN float VC[2];
EXTERN float VG[2];
/* Si is initial soil saturation, Sw is final soil saturation */
EXTERN float Sw[2];
EXTERN float Si[2];
EXTERN float Mann_n;
EXTERN float Temp;

EXTERN int num_sizes;
EXTERN int NCON, DTIM, FTIM;

EXTERN int NRAIN1;
EXTERN int NRAIN2;

typedef struct
{
    float intensity;
    int end_time;
} RAIN;

EXTERN RAIN *rain1;
EXTERN RAIN *rain2;

EXTERN int NWS, NPL;
EXTERN int rain_flag, sed_flag;
EXTERN int sed_default;
EXTERN int depres_flag;
EXTERN int *unit_type;
EXTERN double depres[2];
EXTERN char sim_title[14];
EXTERN char unit_ident[12];
EXTERN int *print;

EXTERN FILE *tape1_fd;
EXTERN FILE *tape2_fd;
EXTERN FILE *tape3_fd;
EXTERN FILE *tape4_fd;

EXTERN FILE *temp1_fd;
EXTERN FILE *temp2_fd;
EXTERN FILE *temp3_fd;
EXTERN FILE *temp4_fd;

EXTERN char *temp1_name;
EXTERN char *temp2_name;
EXTERN char *temp3_name;
EXTERN char *temp4_name;

EXTERN char tape1_name[150];
EXTERN char tape2_name[150];
EXTERN char tape3_name[150];
EXTERN char tape4_name[150];
EXTERN int option;
EXTERN char dir[100];
EXTERN char path[150];
EXTERN char extthin_name[50], *extthin_mapset;

EXTERN float Chplast;
EXTERN float chKw, chYc, chporos;
EXTERN float chSi, chSw;
EXTERN int NCH;
EXTERN int divnum;
EXTERN int *number;
EXTERN int *chan_num;
typedef struct
{
    int ws_drain[3];
    int pl_drain[2];
    int str_drain[3];
} DRAIN;

EXTERN DRAIN *drain;

typedef struct
{
    int fromnum;
    int tonum;
} DRAIN_PAT;

EXTERN DRAIN_PAT *drain_pat;
