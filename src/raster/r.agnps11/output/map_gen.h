#include <stdio.h>
#include <math.h>
#include "gis.h"


#define max_datasets	2

#define YES     1
#define NO      0

#define cur     0	/* current simulatuions data */
#define prev    1	/* previous run data */

#define sig_fac      10

typedef struct MAPS {  /* GRASS maps used in analysis */
char *p;               /* pointer to map name */
char *mapset;	       /* mapset of the map */
short val;             /* Map number */
int fd;                /* Map file descriptor */
CELL *rbuf;            /* pointer to map row buffer */
CELL *cptr;            /* pointer to current col in buffer */
struct MAPS *next;     /* Next map */
} MAPS ;

struct   Cell_head	orig_window;

#define cell_nos	10000


   struct	sed_anlys{
	float	upland[6];      /* area weighted upland erosion in t/a */
	float	channel[6];     /* area weighted channel erosion in t/a */
	int	del_ratio[6];      /* delivery ratio in % */
	int	enrich_ratio[6];  /* enrichment ratio in % */
	float	mean_conc[6];      /* mean concentration in ppm */
	float	weighted_yield[6]; /* area weighted yield in t/a */
	float	yield[6];         /*yield in tons */
	} wshd_sed[max_datasets];


   struct	cell_sed{
	float	cell_erosion[6];	/* cell erosion in t/a */
	float	gen_above[6];		/* generated above in t */
	float	within[6];		/* within in t */
	float	yield[6];		/* yield in t */
	int	deposition[6];		/* deposition in % */
	} cell_sediment[max_datasets][cell_nos];


   struct	cell_ro{
	int	dr_area;	/* drainage area in acres */
	float	overland_ro;	/* overland runoff in inches */
	float	us_ro;		/* upstream runoff in inches */
	int	peak_us;	/* peak flow upstream in cfs */
	float	ds_ro;		/* downstream runoff in inches */
	int	peak_ds;	/* peak flow downstream in cfs */
	} cell_runoff[max_datasets][cell_nos];

   struct 	nutrient{
	float	N_sed_within;	/* N Sediment within cell in lbs/a */
	float	N_sed_outlet;	/* N sediment at the outlet cell in lbs/a */
	float	N_sol_within;	/* N water soluble within cell in lbs/a */
	float	N_sol_outlet;	/* N water soluable at outlet cell in lbs/a */
	int	N_conc;		/* N water soulble concentration in ppm */
	
	float	P_sed_within;	/* P Sediment within cell in lbs/a */
	float	P_sed_outlet;	/* P sediment at the outlet cell in lbs/a */
	float	P_sol_within;	/* P water soluble within cell in lbs/a */
	float	P_sol_outlet;	/* P water soluable at outlet cell in lbs/a */
	int	P_conc;		/* P water soulble concentration in ppm */
	
	float	COD_sol_within;	/* COD water soluble within cell in lbs/a */
	float	COD_sol_outlet;	/* COD water soluble outlet cell in lbs/a */
	int	COD_conc;	/* COD water soluble concentration in ppm */
	} nut_anlys[max_datasets][cell_nos];

/* variables to set max and min of set of maps on each screen option */

float	gen_above_min[max_datasets], gen_above_max[max_datasets];
float	within_min[max_datasets], within_max[max_datasets];
float	yield_min[max_datasets], yield_max[max_datasets];

float	ro_us_min[max_datasets], ro_us_max[max_datasets];
float	ro_gen_min[max_datasets], ro_gen_max[max_datasets];
float	ro_ds_min[max_datasets], ro_ds_max[max_datasets];

/*
int     gen_above_factor[max_datasets], within_factor[max_datasets], yield_factor[max_datasets];
float   gen_above_mean, gen_above_range;
float   within_mean, within_range;
float   yield_mean, yield_range;
*/

float	N_sed_in_min[max_datasets], N_sed_in_max[max_datasets];
float	N_sed_out_min[max_datasets], N_sed_out_max[max_datasets];
float	N_ro_in_min[max_datasets], N_ro_in_max[max_datasets];
float	N_ro_out_min[max_datasets], N_ro_out_max[max_datasets];

float	P_sed_in_min[max_datasets], P_sed_in_max[max_datasets];
float	P_sed_out_min[max_datasets], P_sed_out_max[max_datasets];
float	P_ro_in_min[max_datasets], P_ro_in_max[max_datasets];
float	P_ro_out_min[max_datasets], P_ro_out_max[max_datasets];

float	COD_ro_in_min[max_datasets], COD_ro_in_max[max_datasets];
float	COD_ro_out_min[max_datasets], COD_ro_out_max[max_datasets];

/* watershed summary variables */
int   no_cell;  /* to check weather the AGNPS input file and the watershed map has same # of cells */
int     total_area[max_datasets], outlet_cell[max_datasets], no_cells[max_datasets], grid_res[max_datasets];
float   cell_area[max_datasets], rainfall[max_datasets], ei[max_datasets], ro_vol[max_datasets], peak_ro_rate[max_datasets];
float   tot_N_sed[max_datasets], tot_P_sed[max_datasets];
float   tot_N_ro[max_datasets], tot_P_ro[max_datasets], tot_COD_ro[max_datasets];
float   sol_N_conc_ro[max_datasets], sol_P_conc_ro[max_datasets], sol_COD_conc_ro[max_datasets];
char	wshd_des[max_datasets][60];

char	file_name[60];

/* flags to set appropriate variables */
int	wshd_view; /* flag to see wshd summury view */
int	usr_modified;/* flag to set yes when ever user modified his viewing maps*/

#define	sed_in		"Sed_in"
#define	sed_gen		"Sed_gen"
#define	sed_out		"Sed_out"

#define	sed_above	"usr_sed_above"
#define	sed_gented	"usr_sed_gen"
#define	sed_yield	"usr_sed_yield"

#define	ro_us		"ro_us"
#define	ro_gen		"ro_gen"
#define	ro_ds		"ro_ds"

#define	N_sed_in	"N_sed_in"
#define	N_sed_out	"N_sed_out"
#define	N_ro_in		"N_ro_in"
#define	N_ro_out	"N_ro_out"

#define	P_sed_in	"P_sed_in"
#define	P_sed_out	"P_sed_out"
#define	P_ro_in		"P_ro_in"
#define	P_ro_out	"P_ro_out"

#define	COD_ro_in	"COD_ro_in"
#define	COD_ro_out	"COD_ro_out"

#define	usr_ro_us	"usr_ro_us"
#define	usr_ro_gen	"usr_ro_gen"
#define	usr_ro_ds	"usr_ro_ds"

#define	usr_N_sed_in	"usr_N_sed_in"
#define	usr_N_sed_out	"usr_N_sed_out"
#define	usr_N_ro_in	"usr_N_ro_in"
#define	usr_N_ro_out	"usr_N_ro_out"

#define	usr_P_sed_in	"usr_P_sed_in"
#define	usr_P_sed_out	"usr_P_sed_out"
#define	usr_P_ro_in	"usr_P_ro_in"
#define	usr_P_ro_out	"usr_P_ro_out"

#define	usr_COD_ro_in	"usr_COD_ro_in"
#define	usr_COD_ro_out	"usr_COD_ro_out"

float	max_sed[max_datasets], min_sed[max_datasets];

float	max_ro[max_datasets], min_ro[max_datasets];

float	max_N_sed[max_datasets], min_N_sed[max_datasets];
float	max_N_ro[max_datasets], min_N_ro[max_datasets];

float	max_P_sed[max_datasets], min_P_sed[max_datasets];
float	max_P_ro[max_datasets], min_P_ro[max_datasets];

float	max_COD_ro[max_datasets], min_COD_ro[max_datasets];

MAPS	*cell_num_map;

/* user input aspect map name */
char    wshd_aspect[64];
char    wshd_name[64];

extern char *get_mapset();
extern char *get_old_name();
extern char *emalloc();

/* AGNPS input file structure */


struct	agnps_input{
	int	cell_num;
	int	rcell_num;
	int	cn;
	float	slope_pct;
	int	slope_shape;
	int	slope_ln;
	float	chnl_slope;
	float	chnl_side_slope;
	float	man_n;
	float	k_val;
	float	c_fac;
	float	p_fac;
	float	surf_cond;
	int	aspect;
	int	texture;
	int	fert_fac;
	int	fert_avl_fac;
	int	feedlot;
	int	gully;
	int	cod_fac;
	int	impd_fac;
	int	chnl_ind;
	} agnps_input;

struct	agnps_input ag_inp[max_datasets][cell_nos];

/* flag variables for different nutrients and its
   attachments */

int	NUTRIENT;
int	NUT_ATTCH;
int	ANALYSIS_DATA;
int	ANALYSIS;
int	SED_ANALYSIS;
int	RO_ANALYSIS;
int	N_SED_ANALYSIS;
int	P_SED_ANALYSIS;
int	N_RO_ANALYSIS;
int	P_RO_ANALYSIS;
int	COD_ANALYSIS;
int	MAPS_SAVED;

/* Newly created maps are stored in current mapset */

char	*this_mapset;
/* user specified max and min range to dispaly */
float	orig_max, orig_min;
/* user specified area to compute the avg stats */
int	row1, row2, col1, col2;

/* max and min of each input */
struct	max_min{
	float	mx;
	float	mn;
	} max_min;

struct 	max_min	cn[max_datasets], slope_pct[max_datasets], slope_ln[max_datasets];
struct 	max_min	c_fac[max_datasets], k_val[max_datasets], fert_fac[max_datasets];

/* define each input, so that easy to identify for any computation */

#define	CN		101
#define	Slope		102
#define	Slope_ln	103
#define	C_fac		104
#define	K_val		105
#define	Fert_fac	106

/* X & Y co-ordinates to draw cdf (Cumulative distribution curve) and fdc (frequency distribution curve).
A max of 10 intervals are allowed */

double  X[10], Y[10];
int	no_X;
