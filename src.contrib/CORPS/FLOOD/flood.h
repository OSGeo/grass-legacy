/*******************************************************************************
                    Floodplain Analysis Toolkit
               Mother Earth Systems, Boulder, Colorado


This software was been developed for the U.S. Army Corps of Engineers,
Ft. Worth District under contract #DACW63-91-M-1085 and for the Omaha District
under contract #DACW45-92-P-1301.

This code is in the public domain.  Permission to use, copy, modify, and
distribute this software and its documentation for any purpose and without
fee is granted.

Mother Earth Systems disclaims all warranties with regard to this software,
including all implied warranties of merchantability and fitness. In no event
shall Mother Earth Systems be liable for any special, indirect or consequential
damages or any damages whatsoever resulting from loss of use, data or profits,
whether in an action of contract, negligence or other tortious action,
arising out of or in connection with the use or performance of this software.

*******************************************************************************/

/*---------*/
/* boolean */
/*---------*/
#define TRUE  1
#define FALSE 0

/*-------------------------------*/
/* cross section sorting options */
/*-------------------------------*/
#define IGNORE 0
#define NOSORT 1
#define USER   2
#define VECTOR 3
#define VALUE  4

/*----------------------------------*/
/* surface interpolation algorithms */
/*----------------------------------*/
#define VORONOI 1
#define TPS     2
#define IDW     3
#define IDW2    4
#define CONTOUR 5

/*--------------------------*/
/* character string lengths */
/*--------------------------*/
#define CRVLEN       4 
#define SUFFIXLEN    8
#define WORDLEN     81
#define LINELEN    133
#define FNAMELEN   257
#define BUFFLEN   1024
#define PATHLEN   1024

/*--------------------*/
/* input data lengths */
/*--------------------*/
#define DD_OFFSET   45

#define H2_IDLEN      2
#define H2_FIELDLEN   8
#define H2_RECLEN    81

/*------------*/
/* area units */
/*------------*/
#define NAREAUNITS  4
#define AREA_SQMI   0
#define AREA_ACRE   1
#define AREA_SQMT   2
#define AREA_HECT   3

#define PROJECTION  -1
#define SQ_FEET      1
#define SQ_MILES     2
#define SQ_METERS    3
#define ACRES        4
#define HECTARES     5

/*--------------------*/
/* economic reporting */
/*--------------------*/
#define NUMBER  0
#define STRING  1

#define DOLLAR  0
#define COUNT   1
#define ONEDEC  2

#define TBL_FMT 0
#define ROW_FMT 1

#define NPROPCATS   7
#define CAT_RES     0
#define CAT_MFR     1
#define CAT_MOB     2
#define CAT_COM     3
#define CAT_PUB     4
#define CAT_POV     5
#define CAT_TOT     6

/*--------------*/
/* volume units */
/*--------------*/
#define CU_FEET      1
#define CU_METERS    2
#define ACRE_FEET    3

/*--------------*/
/* value limits */
/*--------------*/
#define FWIDTH_I8       99999999
#define NOVALUE          -999999.0
#define BIG_VALUE        1000000.0
#define BIG_DISTANCE   999999999.0

/*------------*/
/* line types */
/*------------*/
#define HORIZONTAL 1
#define VERTICAL   2 
#define SLOPED     3

/*-------------------------*/
/* array allocation blocks */
/*-------------------------*/
#define CRV_INCREMENT    10
#define XSECT_INCREMENT  50
#define DEPTH_INCREMENT 100
#define TAB_INCREMENT    11
#define EVENT_INCREMENT   9
#define GR_INCREMENT     10

/*------------------------*/
/* GRASS data directories */
/*------------------------*/
#define ATT     "dig_att"
#define PLUS    "dig_plus"

/*--------------------*/
/* user supplied maps */
/*--------------------*/
static char *xsect_id_mconv  = "cross.sect";
static char *build_loc_mconv = "buildings";
static char *elev_mconv      = "elevation";

/*----------------*/
/* generated maps */
/*----------------*/
static char *felement         = "f_ctrl";
static char *xsect_wsel_mconv = "xsect";
static char *reach_mconv      = "reach";
static char *wsurf_mconv      = "wsurf";
static char *depth_mconv      = "depth";
static char *sort_mconv       = "sorted";
static char *inun_mconv       = "inun";
static char *clump_mconv      = "clump";
static char *damage_mconv     = "damage";
static char *temp_mconv       = "temp";

/*---------------------------*/
/* user supplied ascii files */
/*---------------------------*/
static char *finput_ctrl      = "finput.ctrl";
static char *fwsurf_ctrl      = "fwsurf.ctrl";
static char *fecon_ctrl       = "fecon.ctrl";
static char *fdetail_ctrl     = "fdetail.ctrl";
static char *fclean_ctrl      = "fclean.ctrl";

static char *hec2_out_fconv   = "hec2.out";
static char *hec2_split_fconv = "hec2.split";
static char *build_info_fconv = "build.dat";
static char *damcrv_fconv     = "damcrv.dat";

/*-----------------------*/
/* generated ascii files */
/*-----------------------*/
static char *summary_report_fconv  = "summary.report";
static char *damage_report_fconv   = "damage.report";
static char *depth_report_fconv    = "depth.report";
static char *xsect_hec2_fconv      = "xsect.hec2";
static char *log_fconv             = "flood.log";

/*---------*/
/* Globals */
/*---------*/
int   sort;
char *f_mapset;
char  log_fname[PATHLEN];

/*-----------------------*/
/* structure definitions */
/*-----------------------*/
#define ORIG   1
#define NEW    2
#define C_HEAD struct Cell_head
#define M_INFO struct Map_info
#define L_PNTS struct line_pnts
#define X_INFO struct xs_info
 
struct xs_info
{
   int      source;
   int      index;
   int      secno;
   L_PNTS  *points;
   X_INFO  *next;
   X_INFO  *prev;
};

/*-----------------------*/
/* function declarations */
/*-----------------------*/
char   *mapsuffix();
char   *make_reach();
char   *get_xsect_name();
double  distance();
double  distance_sq();
double  dist_factor();
double  cvt_dist();
double  cvt_area();
double  cvt_volume();
double  point_line_snap();
X_INFO *snap_xsect();

/*-----------------*/
/* GRASS functions */
/*-----------------*/
#ifdef __cplusplus
extern "C" {
   void   R_open_driver();
   void   R_flush();
   void   R_standard_color( int );

   int    D_move_abs(), D_cont_abs();
   int    D_translate_color( char *);
   void   D_setup( int );
   double D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east();
   double D_d_to_u_row( double ), D_d_to_u_col( double );

   void   G_setup_plot( double, double, double, double, int(*)(), int(*)() );
   char  *G_find_cell( char*, char* );
   int    G_open_cell_old( char*, char* );
   void   G_get_set_window( C_HEAD* );
   CELL  *G_allocate_cell_buf();
   void   G_get_map_row( int, CELL*, int );
   double G_col_to_easting( double, C_HEAD );
   double G_row_to_northing( double, C_HEAD );
   void   G_plot_line( double, double, double, double );
}

#else
   void   R_open_driver();
   void   R_flush();
   void   R_standard_color();

   int    D_move_abs(), D_cont_abs();
   int    D_translate_color();
   void   D_setup();
   double D_get_d_north(), D_get_d_south(), D_get_d_west(), D_get_d_east();
   double D_d_to_u_row(), D_d_to_u_col();

   void   G_setup_plot();
   char  *G_find_cell();
   int    G_open_cell_old();
   int    G_get_set_window();
   CELL  *G_allocate_cell_buf();
   int    G_get_map_row();
   double G_col_to_easting();
   double G_row_to_northing();
   void   G_plot_line();

#endif
