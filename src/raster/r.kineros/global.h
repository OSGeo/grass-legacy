#include "gis.h"

struct data {
    int order;
    int index;
    int plane[3];
    int trib[2];
    int chan_shape;
    int print;
    int pond;
    int print_rain;
    float length;
    float width;
    float slope;
    float bank[2];
    float stream_width;
    float diameter;
    float mannings_n;
    float laminar_k;
    float fmin;
    float G;
    float porosity;
    float Sint;
    float Smax;
    float Rock;
    float recess;
    float intercept;
    int res_law;
    float Cf;
    float Cg;
    float Ch;
    float Co;
    float d50;
    float rho_s;
    float pave;
    float sigma_s;
};

struct {
    struct Option *accum;
    struct Option *dem;
    struct Option *drain;
    struct Option *half_basin;
    struct Option *rock;
    struct Option *slopes;
    struct Option *soil;
    struct Option *stream;
    struct Option *width;
} parm;

struct Cell_head region;
struct data *element;

char *mapset;

int *col_head;
int *row_head;

int convert_GIS_units;
int erosion;
int max_soil;
int min_ele;
int min_soil;
int ncols;
int npart;
int nres;
int nrows;
int num_ele;
int units_space;
int units_time;

float char_length;
float dt;
float res;
float res_diag;
float temp;
float tfin;
float theta;

float fmin[MAX_SOIL];
float G[MAX_SOIL];
float porosity[MAX_SOIL];
float Sint[MAX_SOIL];
float Smax[MAX_SOIL];

int index();

FILE *open_file();

int **imatrix();
int  *ivector();
float **fmatrix();
float  *fvector();
void free_imatrix();
void free_ivector();
void free_fmatrix();
void free_fvector();
