#include "defs.h"
#include "protodefs.h"


#ifndef GLOBAL
#  define GLOBAL extern
#  define INIT(x)
#else
#  define INIT(x) = x
#endif

GLOBAL int SCREEN_TOP;
GLOBAL int SCREEN_BOTTOM;
GLOBAL int SCREEN_LEFT;
GLOBAL int SCREEN_RIGHT;

/** These are all curses (text) windows **
 ** GLOBAL Window *INFO_WINDOW;
 ** GLOBAL Window *MENU_WINDOW;
 ** GLOBAL Window *PROMPT_WINDOW;
**/

GLOBAL View *VIEW_MAP1;
GLOBAL View *VIEW_TITLE1;
GLOBAL View *VIEW_MAP1_ZOOM;
GLOBAL View *VIEW_TITLE1_ZOOM;

GLOBAL View *VIEW_MAP2;
GLOBAL View *VIEW_TITLE2;
GLOBAL View *VIEW_MAP2_ZOOM;
GLOBAL View *VIEW_TITLE2_ZOOM;

GLOBAL View *VIEW_MENU;

GLOBAL Rectify_Group group;

GLOBAL char interrupt_char;
GLOBAL char *tempfile1;
GLOBAL char *tempfile2;
GLOBAL char *digit_points;            /* digitizer control points */
GLOBAL char *digit_results;           /* digitizer results */
GLOBAL char *tempfile_camera;         /* panel_restore under camera */

/* group file list, target cell,vector files */
GLOBAL char *group_list INIT(NULL);
GLOBAL char *cell_list INIT(NULL);
GLOBAL char *vect_list INIT(NULL);
GLOBAL char *color_list INIT(NULL);

GLOBAL int  from_keyboard  INIT(-1);   /* input method */
GLOBAL int  from_digitizer INIT(-1);
GLOBAL int  from_screen    INIT(-1);
GLOBAL int  from_flag      INIT(0);
GLOBAL int  use_digitizer INIT(0);    /* is there a digitizer out there? */

GLOBAL int  use_zoom_box INIT(0);    
GLOBAL int  use_zoom_pnt INIT(0);    

GLOBAL int  from_keyboard_fid  INIT(-1);   /* input method */
GLOBAL int  from_camera_fid    INIT(-1);
GLOBAL int  from_flag_fid      INIT(0);

GLOBAL int  dotsize INIT(4);

GLOBAL int THE_COLORS[10];
#define I_COLOR_BLACK  THE_COLORS[0]
#define I_COLOR_BLUE   THE_COLORS[1]
#define I_COLOR_BROWN  THE_COLORS[2]
#define I_COLOR_GREEN  THE_COLORS[3]
#define I_COLOR_GREY   THE_COLORS[4]
#define I_COLOR_ORANGE THE_COLORS[5]
#define I_COLOR_PURPLE THE_COLORS[6]
#define I_COLOR_RED    THE_COLORS[7]
#define I_COLOR_WHITE  THE_COLORS[8]
#define I_COLOR_YELLOW THE_COLORS[9]

/* name, mapset, color of vectors */
#define VFILES 12
GLOBAL DisplayList display_list;
GLOBAL int  line_color;

/* transformation order and parameters */
GLOBAL char      order_msg[10];
GLOBAL int xmax, ymax, gmax;
GLOBAL double rms;
GLOBAL double *xres, *yres, *gnd;


/* name, and mapset of raster image */
GLOBAL char raster_file[100];
GLOBAL char raster_mapset[100];

double row_to_northing();
double col_to_easting();
double northing_to_row();
double easting_to_col();


/*=============== New stuff ===============*/
#define Number(x)    ((sizeof(x))/(sizeof(x[0])))
#define True   	     ((int)1)
#define False  	     ((int)0)

GrassEnv new_environment();

#undef INIT

/** Digitizer stuff */
GLOBAL char CLR_LINE;
GLOBAL char CLR_AREA;
GLOBAL char CLR_SITE;
GLOBAL char CLR_LSITE;
GLOBAL char CLR_LLINE;
GLOBAL char CLR_LAREA;
GLOBAL char CLR_AMARK;
GLOBAL char CLR_ALABEL;
GLOBAL char CLR_LLABEL;
GLOBAL char CLR_HIGHLIGHT;
GLOBAL char CLR_ERASE;
GLOBAL char CLR_UNKNOWN;
GLOBAL char CLR_OVERLAY;
GLOBAL char CLR_0_NODE;
GLOBAL char CLR_1_NODE;
GLOBAL char CLR_2_NODE;

