
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

/* changed MAX_CELLS from 10000 to 50000        M. Neteler 12/96 */
#define MAX_CELLS     50000

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
MAPS    *pesticide; /* variable added by Dave Peterson, February 1996 */

/* watershed input variables */

float   cell_area, rainfall, ei;
char	wshd_des[60];
char	wshd_name[60];
int	tot_cells;
int	grid_res;



/*  Zhian Li, July 1995: Added parameters for AGNPS version 5.0 */

char    stmtp[4];
float   stmtm;
float   stmnppm;
char    pkflcal[10];
char    gmorp[10];
char    hydrosf[10];
float   kfact;
char    yesno[4];
int     fcel_id;

/*  End of modification                    */
/*                                         */
/*                  Zhian Li, July, 1995   */
 

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
	int	fert_level;	/* Fertilization level zero, low, med, or high*/
	int     pest_level;     /* added by Dave Peterson, February 1996 */
	int	incor_level;	/* % fertilizer in top 1 cm of soil */



      /* The following variables are added by Zhian Li                  */
 
        float    chlw;          /* Channel width in feet     */
        float    chlwc;         /* Channel width coefficient */
        float    chlwe;         /* Channel width exponent    */
        float    chld;          /* Channel depth in feet     */
        float    chldc;         /* Channel depth coefficient */
        float    chlde;         /* Channel depth exponent    */
        float    chllnth;       /* Channel length in feet    */
        float    chllnthc;      /* Channel length coefficient*/
        float    chllnthe;      /* Channel length exponent   */
        float    chlslope;      /* Channel Slope in feet     */
        float    chlsslop;      /* Channel Side Slope %      */
        float    chlm;          /* Channel Manning factor    */
 
     /* Nutrient information                                 */
        int     fert4n;         /* Nitrogen application rate lb/acres */
        int     fert4p;         /* Phosphorus application rate lb/acre*/
        int     fert_nfac;      /* Nitrogen availability factor       */
        int     fert_pfac;      /* Phosphorus availability factor     */
	int	pt_src;		/* point source indicator             */
	int	gully_src;	/* gully source level indicator       */
	int	COD_fac;	/* Chemical Oxygen Demond factor      */
	int	impd_fac;	/* impoundment factor                 */
	int	chl_indicator;	/* indicator of channel type          */
	} cell;

struct cell	cel[MAX_CELLS];



/*  Zhian Li, July 1995: Add new parameters for the feedlot and nonfeedlot*/
/*  point sources.                                   */


        int     tptsrc;         /* Total number of point sources          */
        int     mfdlt;          /* Number of feedlots                     */
        int     mnfdlt;         /* Number of nonfeed lots                 */


      /* Zhian Li, July 1995: For nonfeedlot point sources parameters     */

        int      pntsrctp;      /* The type of the point source            */
        float    wdisch;        /* Water discharge rate; Cubic Feet/second */
        float    tn;            /* Total Nitrogen discharged from this non */
                                /* feed lot point source(ppm).             */ 
        float    tp;            /* Total Phosphorus discgared from this non*/
                                /* feed lot point source(ppm).             */ 
        float    tcod;          /* Total COD discarged from this non feed  */
                                /* lot point source(ppm).                  */
        int      ptsloc;        /* Location of the point source in the cell*/
                                /*       1 = top                           */
                                /*       0 = bottom                        */
      /* End of nonfeedlot point source parameter block*/
    

      /* For feedlot point source parameters         */
        float    fdarea;        /* Feed lot area in acres                  */
        int      fcs;           /* Feed lot curve number                   */
        float    rfarea;        /* Feed lot roofed area in acres           */
        int      fln;           /* Feed lot nitrogen concentration in ppm  */
        int      flp;           /* Feed lot Phosphorus concentration in ppm*/
        int      flcod;         /* Feed lot COD  concentration in ppm      */


        /* for the tributary area (area 2 of feedlot)*/
        float    tb1a;          /* Area of subarea 1 of the tributary area */
        int      tb1cnt;        /* Curve number of subarea 1 of the        */
                                /*        tributary area.                  */

        float    tb2a;          /* Area of subarea 2 of the tributary area */
        int      tb2cnt;        /* Curve number of subarea 2 of the        */
                                /*        tributary area.                  */

        float    tb3a;          /* Area of subarea 3 of the tributary area */
        int      tb3cnt;        /* Curve number of subarea 3 of the        */
                                /*        tributary area.                  */

        float    tb4a;          /* Area of subarea 4 of the tributary area */
        int      tb4cnt;        /* Curve number of subarea 4 of the        */
                                /*        tributary area.                  */

        float    tb5a;          /* Area of subarea 5 of the tributary area */
        int      tb5cnt;        /* Curve number of subarea 5 of the        */
                                /*        tributary area.                  */

        float    tb6a;          /* Area of subarea 6 of the tributary area */
        int      tb6cnt;        /* Curve number of subarea 6 of the        */
                                /*        tributary area.                  */


       /* For the areas adjacent to the feedlot    */
        float    ad1a;          /* Area of subarea 1 adjacent to feedlot   */
        int      ad1cnt;        /* Curve number of subarea 1 adjacent to   */
                                /*        the feedlot.                     */

        float    ad2a;          /* Area of subarea 2 adjacent to feedlot   */
        int      ad2cnt;        /* Curve number of subarea 2 adjacent to   */
                                /*        the feedlot.                     */

        float    ad3a;          /* Area of subarea 3 adjacent to feedlot   */
        int      ad3cnt;        /* Curve number of subarea 3 adjacent to   */
                                /*        the feedlot.                     */

        float    ad4a;          /* Area of subarea 4 adjacent to feedlot   */
        int      ad4cnt;        /* Curve number of subarea 4 adjacent to   */
                                /*        the feedlot.                     */

        float    ad5a;          /* Area of subarea 5 adjacent to feedlot   */
        int      ad5cnt;        /* Curve number of subarea 5 adjacent to   */
                                /*        the feedlot.                     */

        float    ad6a;          /* Area of subarea 6 adjacent to feedlot   */
        int      ad6cnt;        /* Curve number of subarea 6 adjacent to   */
                                /*        the feedlot.                     */


        /* for the 3 buffer areas*/
        float    bf1slp;        /* Slope for buffer area 1.                */
        float    bf1scnt;       /* Buffer area surface constant            */
        int      bf1fll;        /* Flow length for buffer area 1           */   
        float    bf2slp;        /* Slope for buffer area 2.                */
        float    bf2scnt;       /* Buffer area surface constant            */
        int      bf2fll;        /* Flow length for buffer area 2           */  
        float    bf3slp;        /* Slope for buffer area 3.                */
        float    bf3scnt;       /* Buffer area surface constant            */
        int      bf3fll;        /* Flow length for buffer area 3           */   
        /* for the animal entries*/
        int      nanimal1;      /* Number of animals of type 1             */
        float    animcod1;      /* animal COD factor for animal type 1     */ 
        float    animp1;        /* animal Phosphorus for animal type 1     */
        float    animn1;        /* animal Nitrogen for animal type 1       */ 
        int      nanimal2;      /* Number of animals of type 2             */
        float    animcod2;      /* animal COD factor for animal type 2     */ 
        float    animp2;        /* animal Phosphorus for animal type 2     */
        float    animn2;        /* animal Nitrogen for animal type 2       */ 
        int      nanimal3;      /* Number of animals of type 3             */
        float    animcod3;      /* animal COD factor for animal type 3     */ 
        float    animp3;        /* animal Phosphorus for animal type 3     */
        float    animn3;        /* animal Nitrogen for animal type 3       */ 
        
      /* End of feedlot point source parameter block*/

      /* Soil information                           */
      /* Note:                                      */
      /*                                            */
      /*    The user must provide this set of data  */
      /*   if the soil indicator is none zero.  But */
      /*   if the soil indicator is zero, this set  */
      /*   of data must not present in the input    */
      /*   deck.  The AGNPS manual did not make this*/
      /*   clear.                                   */


        float    basen;         /* Nitrogen presents in soil lb N/lb Soil  */
        float    basep;         /* Phosphorus presents in soil lb P/lb Soil*/
        float    poren;         /* Nitrogen in soil pores             ppm  */
        float    porep;         /* Phosphorus in soil pores           ppm  */
        float    extractn;      /* Nitrogen extracted from soil into runoff*/
        float    extractp;      /* phosphorus extracted from soil to runoff*/
        float    leachn;        /* Nitrogen leached into soil              */
        float    leachp;        /* Phosphorus leached into soil            */
        int      soilorg;       /* Organic matter remaining in the soil    */


      /*   The following section defines fertilizer */
      /*   application rate.                        */
    

        int      fert1n;        /* Nitrogen application rate for fertilizer*/
                                /* level 1.                     lb/acre.   */ 
        int      fert1p;        /* Phosphorus application rate for fertilizer*/
                                /* level 1.                     lb/acre.   */ 
        int      fert2n;        /* Nitrogen application rate for fertilizer*/
                                /* level 2.                     lb/acre.   */ 
        int      fert2p;        /* Phosphorus application rate for fertilizer*/
                                /* level 2.                     lb/acre.   */ 
        int      fert3n;        /* Nitrogen application rate for fertilizer*/
                                /* level 3.                     lb/acre.   */ 
        int      fert3p;        /* Phosphorus application rate for fertilizer*/
                                /* level 3.                     lb/acre.   */ 
        int      fert4n;        /* user input    Nitrogen application rate */
                                /* in fertilizer                lb/acre.   */ 
        int      fert4p;        /* user input  Phosphorus application rate */
                                /* in fertilizer                lb/acre.   */ 
        int      fert_nfac;     /* User input N availability factor.       */
        int      fert_pfac;     /* User input P availability factor.       */

     /* Model run hydrologic and geomorphic calculation flags */
        int     hydro_mdl; /* Hydrology Calculation model     */
        int     geom_mod;  /* Geomorphic calculation indicator*/


     /* Channel type to be passed to the channel data input functions.*/
       
        int      chl_type;      /* Channel type:             */
                                /* There are 9 channel types */
                                /* as defined in the AGNPS   */
                                /* input manual.             */
                                /*                           */
                                /*   0.  Water cell          */
                                /*   1.  No definitive chanl.*/
                                /*   2.  Drainage ditch.     */
                                /*   3.  Road ditch.         */
                                /*   4.  Grass Waterway.     */
                                /*   5.  Ephemeral stream.   */
                                /*   6.  Intermittent stream.*/
                                /*   7.  Perennial stream.   */
                                /*   8.  Other type of chanl.*/ 

     /* Channel information for cells of waterway or no definitive channel */
        float    chnw;          /* Channel width in feet     */
        float    chnwco;        /* Channel width coefficient */
        float    chwexp;        /* Channel width exponent    */
        float    chdepth;       /* Channel depth in feet     */
        float    chdepco;       /* Channel depth coefficient */
        float    chdepexp;      /* Channel depth exponent    */
        float    chlenth;       /* Channel length in feet    */
        float    chlco;         /* Channel length coefficient*/
        float    chlexp;        /* Channel length exponent   */
        float    chmanf;        /* Channel Manning factor    */


     /* Channel information for cell with definitive channel */
        int      chl_cel_id;    /* Cell Number of the cell for*/  
                                /* which the channel data are*/
                                /* to be entered.            */
        float    chlw;          /* Channel width in feet     */
        float    chlwc;         /* Channel width coefficient */
        float    chlwe;         /* Channel width exponent    */
        float    chld;          /* Channel depth in feet     */
        float    chldc;         /* Channel depth coefficient */
        float    chlde;         /* Channel depth exponent    */
        float    chllnth;       /* Channel length in feet    */
        float    chllnthc;      /* Channel length coefficient*/
        float    chllnthe;      /* Channel length exponent   */
        float    chlslope;      /* Channel Slope in feet     */
        float    chlsslop;      /* Channel Side Slope %      */
        float    chlm;          /* Channel Manning factor    */
  
     /*  Additional Model run flags                          */
        int      agdecay;       /* AGNPS decay indicator     */
        int      ndecay;        /* % N decay from chanl. flow*/
        int      pdecay;        /* % P decay from chanl. flow*/
        int      coddecay;      /* % COD decay from chl. flow*/
        int      clyscour;      /* Clay scouring indicator   */
        int      sltscour;      /* silt scouring indicator   */
        int      saggsur;       /* small Agg. scouring indi. */
        int      laggsur;       /* large Agg. scouring indi. */
        int      sandsur;       /* sand scouring indicator   */



MAPS    *temp_grid_map;         /* Temporary map used to hold*/
                                /* grid map which is used to */
                                /* create aspect map from the*/
                                /* base elevation map.       */ 


/*  New structures and data definitions end here    */
/*  End of modifications                            */
/*                                                  */
/*               Zhian Li, July, 1995               */
 

/* new temp maps generated from the input interface */

MAPS	*cell_num_map, *temp_dir_map, *temp_slope_map;
MAPS	*temp_drain_map, *temp_cn_map;

char	in_fl_name[64];
int	amc; /* AMC */
