#include "defs.h"

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

GLOBAL char red_colors[256], grn_colors[256], blu_colors[256];

GLOBAL View *VIEW_TITLE;
GLOBAL View *VIEW_IMAGE;
GLOBAL View *VIEW_IMAGE2;
GLOBAL View *VIEW_MENU;
GLOBAL View *VIEW_RED;
GLOBAL View *VIEW_GRN;
GLOBAL View *VIEW_BLU;
GLOBAL View *VIEW_RED_IMAGE;
GLOBAL View *VIEW_GRN_IMAGE;
GLOBAL View *VIEW_BLU_IMAGE;
GLOBAL View *VIEW_FILES;

GLOBAL struct Cell_head window;

GLOBAL Group group;

GLOBAL char interrupt_char;
GLOBAL char *tempfile1;
GLOBAL char *tempfile2;

GLOBAL int COLORS[10];
#define BLACK	COLORS[0]
#define BLUE	COLORS[1]
#define BROWN	COLORS[2]
#define GREEN	COLORS[3]
#define GREY	COLORS[4]
#define ORANGE	COLORS[5]
#define PURPLE	COLORS[6]
#define RED	COLORS[7]
#define WHITE	COLORS[8]
#define YELLOW	COLORS[9]

#undef INIT
