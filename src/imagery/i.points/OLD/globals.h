#include "imagery.h"
#include <curses.h>

#ifndef GLOBAL
#define GLOBAL extern
#endif

typedef struct
{
    int top, left, bottom, right;
} Window;

typedef struct
{
    int top, bottom ,left, right;
    int nrows, ncols;
    char *name;
} View;

typedef struct
{
    char *msg;
    int  key;
} Menu;

GLOBAL Window *INFO_WINDOW;
GLOBAL Window *MENU_WINDOW;
GLOBAL Window *PROMPT_WINDOW;

GLOBAL View *VIEW_TITLE1;
GLOBAL View *VIEW_TITLE2;
GLOBAL View *VIEW_MAP1;
GLOBAL View *VIEW_MAP2;
GLOBAL View *VIEW_MAP1_ZOOM;
GLOBAL View *VIEW_MAP2_ZOOM;
GLOBAL View *VIEW_MENU;

GLOBAL struct
{
    char name[50];
    struct Ref ref;
    struct Control_Points points;
} group;


GLOBAL char interrupt_char;
GLOBAL char *tempfile1;
GLOBAL char *tempfile2;
