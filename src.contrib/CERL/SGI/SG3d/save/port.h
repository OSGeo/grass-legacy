#include "gl.h"
#include "panel.h"
#include "gis.h"

/* Defines */

#define FROM  0
#define TO    1
#define X 0
#define Y 1
#define Z 2


#define TOGGLE(x) ((x) = (x) ? 0 : 1)
#define DIR_FORWARD	1
#define DIR_BACKWARD	2
#define DIR_LEFT	3
#define DIR_RIGHT	4
#define DIR_DOWN	5
#define DIR_UP		6

#define DELTA_ANGLE  2

#define XMOD 15.
#define YMOD 15.

#define D_GRID 1
#define D_POLY 2

#define RES_MAX 100

#define VERTEX(x,y,z) vector[0] = (x);vector[1]=(y);vector[2]=(z);v3f(vector);



/* Extern variables */
#ifdef MAIN
#define GLOBAL
#else
#define GLOBAL extern
#endif


GLOBAL int X_Min, X_Max, Y_Min, Y_Max, Z_Min, Z_Max;
GLOBAL float X_Mid, Y_Mid, Z_Mid;
GLOBAL float X_Res, Y_Res;

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

GLOBAL int fast_res, slow_res;

GLOBAL int Display_type;

GLOBAL double XBase, YBase, ZBase;
GLOBAL double XRange, YRange, ZRange;

/* Panel defs */
GLOBAL Actuator *Aslow1, *Aslow2, *Afast1, *Afast2;
GLOBAL Actuator *Aquit, *Ashading, *Adraw;
GLOBAL Actuator *Apoly, *Agrid;
GLOBAL Actuator *Alook, *Aincl, *Axy, *Aheight, *Afine;
GLOBAL Actuator *Apersp, *Areset;


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

