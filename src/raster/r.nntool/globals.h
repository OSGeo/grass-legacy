#include "defs.h"
#include <math.h>

#ifndef GLOBAL
#  define GLOBAL extern
#  define INIT(x)
#else
#  define INIT(x) = x
#endif

GLOBAL Window *INFO_WINDOW;
GLOBAL Window *MENU_WINDOW;
GLOBAL Window *PROMPT_WINDOW;

GLOBAL int SCREEN_TOP;
GLOBAL int SCREEN_BOTTOM;
GLOBAL int SCREEN_LEFT;
GLOBAL int SCREEN_RIGHT;
GLOBAL int SA;	/* Saved areas/polygons */
GLOBAL int Uj;	/* Universal j variable */
GLOBAL int SAMPLED; /* Has the training sites been sampled ? */

GLOBAL View *VIEW_MAP1;
GLOBAL View *VIEW_TITLE1;
GLOBAL View *VIEW_MAP1_ZOOM;
GLOBAL View *VIEW_TITLE1_ZOOM;

GLOBAL View *VIEW_MASK1;
GLOBAL View *VIEW_MENU;
GLOBAL View *VIEW_HISTO;

GLOBAL int XCOLORS[10];
#define BLACK	XCOLORS[0]
#define BLUE	XCOLORS[1]
#define BROWN	XCOLORS[2]
#define GREEN	XCOLORS[3]
#define GREY	XCOLORS[4]
#define ORANGE	XCOLORS[5]
#define PURPLE	XCOLORS[6]
#define RED	XCOLORS[7]
#define WHITE	XCOLORS[8]
#define YELLOW	XCOLORS[9]

#define NUM_BLACK	0
#define NUM_BLUE	1
#define NUM_BROWN	2
#define NUM_GREEN	3
#define NUM_GREY	4
#define NUM_ORANGE	5
#define NUM_PURPLE	6
#define NUM_RED 	7
#define NUM_WHITE	8
#define NUM_YELLOW	9
#define NAME_BLACK	"Black"
#define NAME_BLUE	"Blue"
#define NAME_BROWN	"Brown"
#define NAME_GREEN	"Green"
#define NAME_GREY	"Grey"
#define NAME_ORANGE	"Orange"
#define NAME_PURPLE	"Purple"
#define NAME_RED 	"Red"
#define NAME_WHITE	"White"
#define NAME_YELLOW	"Yellow"
#define MY_COLORS {{0,0,0},         /*black*/ \
		     {50,50,255},     /*blue*/  \
		     {170,200,70}, /*brown*/ \
		     {0,255,0},     /*green*/ \
		     {150,150,150}, /*grey*/  \
		     {220,170,0},   /*orange*/\
		     {200,0,200},   /*purple*/\
		     {255,0,0},     /*red*/   \
		     {255,255,255}, /*white*/ \
		     {255,255,0}} /*yellow*/
GLOBAL struct { int red, grn, blue; } Color_table[10] INIT( MY_COLORS );

GLOBAL char Group[50];
GLOBAL char Subgroup[50];
GLOBAL struct Ref Refer;
GLOBAL char Outsigfile[50];
GLOBAL FILE *Outsigfile_fd;
GLOBAL char Insigfile[50];
GLOBAL struct Signature Sigs;
GLOBAL struct Cell_head Band_cellhd;
GLOBAL struct coord {
        int x1, y1, x2, y2;
};
GLOBAL float CLASS[10][10][1000]; 
GLOBAL float **NCLS[2];
GLOBAL float **mean;

GLOBAL char interrupt_char;

/* group file list, target cell,vector files */
GLOBAL char *group_list INIT(NULL);
GLOBAL char *cell_list INIT(NULL);
GLOBAL char *vect_list INIT(NULL);

double row_to_northing();
double col_to_easting();
double northing_to_row();
double easting_to_col();

GLOBAL char EOFCHAR;
GLOBAL char ERASECHAR;
GLOBAL char KILLCHAR;
GLOBAL char *tempfile;
GLOBAL int *image;
GLOBAL int *Bandfd;
GLOBAL int colormap[3][256];
GLOBAL struct region Region;
/*GLOBAL struct mapinfo mapinfo ;*/
GLOBAL struct signalflag signalflag;
GLOBAL CELL **Bandbuf;

#undef INIT
