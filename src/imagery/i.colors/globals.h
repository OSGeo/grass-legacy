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

GLOBAL int XXX_COLORS[10];
#define BLACK	XXX_COLORS[0]
#define BLUE	XXX_COLORS[1]
#define BROWN	XXX_COLORS[2]
#define GREEN	XXX_COLORS[3]
#define GREY	XXX_COLORS[4]
#define ORANGE	XXX_COLORS[5]
#define PURPLE	XXX_COLORS[6]
#define RED	XXX_COLORS[7]
#define WHITE	XXX_COLORS[8]
#define YELLOW	XXX_COLORS[9]

#undef INIT
