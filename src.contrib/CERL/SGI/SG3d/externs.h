#include "gl.h"
#include "panel.h"
#include "Vect.h"

/* Defines */

#define FROM  0
#define TO    1
#define X 0
#define Y 1
#define Z 2
#define W 3


#define TOGGLE(x) ((x) = !(x))
#define DIR_FORWARD	1
#define DIR_BACKWARD	2
#define DIR_LEFT	3
#define DIR_RIGHT	4
#define DIR_DOWN	5
#define DIR_UP		6

#define DELTA_ANGLE  2

#define WIDTH 2089
#define HEIGHT 855

#define W_ROWS 504
#define W_COLS 468
#define X_OFFSET 275
#define Y_OFFSET 350
#define XMOD 15.
#define YMOD 15.

#define D_GRID  1
#define D_POLY  2
#define D_GPOLY 3

#define RES_MAX 100

#define VERTEX(x,y,z) vector[0] = (x);vector[1]=(y);vector[2]=(z);v3f(vector);



/* Extern variables */
#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif


GLOBAL int X_Min, X_Max, Y_Min, Y_Max;
GLOBAL float Z_Min, Z_Max, Z_Span, Z_Span_real;
GLOBAL float Z_Min_real, Z_Max_real, Z_Mid_real, Zoff;
GLOBAL float X_Mid, Y_Mid, Z_Mid;
GLOBAL float X_Res, Y_Res;

GLOBAL int   Vect_file;
GLOBAL float Vect_z;
GLOBAL struct Map_info Map;
GLOBAL struct line_pnts *Points;

GLOBAL int X_Size, Y_Size;		/* number of cells of resolution */
GLOBAL int *visual;
GLOBAL int *elev_buf;
GLOBAL float vector[3];
GLOBAL int persp;

GLOBAL float X_Mod, Y_Mod;  /* number of real cells per view cell */
GLOBAL float X_Modr, Y_Modr;  /* number of real cells per view cell,polygon */
GLOBAL Screencoord top, bottom, left, right, xcenter, ycenter;

GLOBAL float UNIT_FROM_TO[2][4];
GLOBAL float FROM_TO[2][4];
GLOBAL float NEW_FROM_TO[2][4];

GLOBAL struct Cell_head  wind;

GLOBAL int redraw_ok;
GLOBAL int shading;

GLOBAL float Z_exag;

GLOBAL int fast_res, slow_res;

GLOBAL int Display_type;
GLOBAL int Fringe_on;

GLOBAL double XBase, YBase, ZBase;
GLOBAL double XRange, YRange, ZRange, ZRange_real;

GLOBAL struct Colors Pcolor;
GLOBAL int Three_map;

/*
**  OBJECT  variables and defs 
*/

#define MAX_OBJ 10

struct object_info {
    float x, y, z;
    float xrot, yrot, zrot;
    int bind;
};

struct object_info obj_info[MAX_OBJ];


/* Panel defs */

/* Control Panel */
GLOBAL Actuator *Aslow1, *Aslow2, *Afast1, *Afast2;
GLOBAL Actuator *Afull, *Aquit, *Ashading, *Adraw, *Areset;
GLOBAL Actuator *Apoly, *Agrid, *Agridc, *Agpoly;
GLOBAL Actuator *Avect, *Afringe, *Arange1, *Arange2, *Ascript, *Anewcell;

GLOBAL int Range;

/* Movement Panel */
GLOBAL Actuator *Alook, *Aincl, *Axy, *Aheight, *Aexag, *Afine, *Apersp, *Avectz;
GLOBAL Actuator *Atwist;
GLOBAL Panel *P_Script;

/* object panel */
GLOBAL Actuator *AOxy, *AOheight, *AOzrot, *AOxrot, *AOyrot, *AObind;


/* Extern routines */
char *getenv ();
char *malloc ();

/*  viewing transformation matrixes */
GLOBAL Matrix land_matrix;
GLOBAL Matrix poly_matrix;

#ifdef MAIN
    Matrix ID_matrix = {
	1., 0., 0., 0., 
	0., 1., 0., 0., 
	0., 0., 1., 0., 
	0., 0., 0., 1.
    };
#else
extern Matrix ID_matrix;
#endif

typedef int FILEDESC;

#define WIDTH 2089
#define HEIGHT 855

#define W_ROWS 504
#define W_COLS 468
#define X_OFFSET 275
#define Y_OFFSET 350

GLOBAL char *AUTO_FILE;
GLOBAL int Write_script;

