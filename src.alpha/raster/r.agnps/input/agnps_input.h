
	/*---------------------------------------------------------*
	 *               AGNPS/GRASS Interface Project             *
	 *  Developed in the Agriculture Engineering Department    *
	 *                at Purdue University                     *
	 *                        by                               *
	 *         Raghavan Srinivasan and Bernard Engel           *
	 *                                                         *
	 *   (c)Copyright, 1992 Purdue Research Foundation, West   *
	 *   Lafayette, Indiana 47907. All Rights Reserved. Unless *
	 *   permission is granted, this material shall not be     *
	 *   copied, reproduced or coded for reproduction by any   *
	 *   electrical, mechanical or chemical processes,  or     *
	 *   combinations thereof, now known or later developed.   *
	 *---------------------------------------------------------*/

#include <stdio.h>
#include <math.h>
#include "gis.h"

#define MAX_CELLS     100000

#define YES     1
#define NO      0

#define WATER     0
#define SAND      1
#define SILT      2
#define CLAY      3
#define PEAT      4

/* define the landuses so that no need to do strcmp */

#define fallow     		1
#define row_crops  		2
#define small_grain		3
#define legumes 		4
#define rotation_meadow		5
#define close_seeded_legumes	6
#define pasture 		7
#define range	 		8
#define permanent_meadow	9
#define meadow	 		10
#define woods	 		11
#define woodland 		12
#define grass_waterway	 	13
#define hard_surface	 	14
#define farmsteads 		15
#define urban	 		16
#define roads_(dirt) 		17
#define water      		18
#define marsh      		19

typedef struct MAPS {  /* GRASS maps used in analysis */
char *p;               /* pointer to map name */
char *mapset;          /* pointer to mapset location */
short val;             /* Map number */
int fd;                /* Map file descriptor */
CELL *rbuf;            /* pointer to map row buffer */
CELL *cptr;            /* pointer to current col in buffer */
short  flag;           /* to check wheather the map exists*/
struct MAPS *next;     /* Next map */
} MAPS ;

struct   Cell_head	orig_window;

/* agnps input maps variables */

MAPS	*elev, *soils, *landuse, *mgt_practice, *nutrient, *machinery;
MAPS	*channel_slope, *slope_length, *wshd, *C_fac, *hyg, *K_fac;
MAPS	*sand, *clay, *hy_cond;

/* watershed input variables */

float   cell_area, rainfall, ei;
char	wshd_des[60];
char	wshd_name[60];
int	tot_cells;
int	grid_res;

extern char *get_mapset();
extern char *get_old_name();
extern char *get_new_name();
extern char *emalloc();
extern int assign_landuse();

/* all new or temp maps are stored in currect mapset */
char	*this_mapset;

/* cell data variables */

typedef struct cell {
	int     cell_num; /* cell number */
	int     rcell_num; /* receiving cell number */
	int	acc;	   /* number of cell accumulating in that cell*/
	int	CN;	   /* SCS CN */
	float	ovl_slope;	/* Average overland slope in %*/
	int	slp_shpe_fact;	/* Slope shape factor */
	int	fld_slp_len;	/* Average field slope length in feet */
	float	chl_slope;	/* Average channel slope in % */
	float	ch_side_slope;	/* Average channel side slope in % */
	float	man_n;		/* Mannings 'n' for channel */
	float	K_fac;		/* K factor from USLE */
	float	C_fac;		/* C factor from USLE */
	float	P_fac;		/* P factor from USLE */
	float	sur_const;	/* Surface condition constant */
	int	aspect;		/* Aspect flow direction (1-8) */
	int	texture;	/* Soil texture sand, slit, clay, or peat */
	int	fert_level;	/* Fertilization level zero, low, med, or high */
	int	incor_level;	/* % fertilizer in top 1 cm of soil */
	int	pt_src;		/* point source indicator */
	int	gully_src;	/* gully source level indicator */
	int	COD_fac;	/* Chemical Oxygen Demond factor */
	int	impd_fac;	/* impoundment factor */
	int	chl_indicator;	/* indicating existence of a defined channel */
	} cell;

struct cell	cel[MAX_CELLS];


/* new temp maps generated from the input interface */

MAPS	*cell_num_map, *temp_dir_map, *temp_slope_map;
MAPS	*temp_drain_map, *temp_cn_map;

char	in_fl_name[64];
int	amc; /* AMC */
