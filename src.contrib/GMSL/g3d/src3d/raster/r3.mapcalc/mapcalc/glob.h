#include "gis.h"
#include "G3d.h"

#define MAP 257
#define UMINUS 258
#define INTEGER 259
#define DOUBLE 260
#define FUNCTION 261
#define GT 262
#define GE 263
#define LT 264
#define LE 265
#define EQ 266
#define OR 267
#define AND 268
#define NE 269
#define SET_VARIABLE 270
#define GET_VARIABLE 271

struct expression_stack
{
    int type;    /* opcode, INTEGER, MAP, DOUBLE, FUNCTION */
    int k;       /* integer,  map file descriptor, function number */
    int nargs;   /* number of args to function, map type */
    double x;    /* double */
    int row,col,depth; /* map row/col for neighborhoods */
};

struct execute_stack
{
    int type;  /* opcode, INTEGER, MAP, etc */
    double *xcell;
    int n;
};

#ifndef MAIN
#define GLOBAL extern
#else
#define GLOBAL
#endif

GLOBAL int expression_stack_depth;
GLOBAL int expression_stack_nalloc ;
GLOBAL struct expression_stack *expression_stack;

GLOBAL int execute_stack_depth;
GLOBAL int execute_stack_nalloc;
GLOBAL struct execute_stack *execute_stack;
GLOBAL int execute_stack_depth_max;

GLOBAL int floating_point_exception;
GLOBAL int floating_point_exception_occurred;
GLOBAL int max_nargs;
GLOBAL double **xargs;
GLOBAL int min_row, max_row;
GLOBAL int min_col, max_col;
GLOBAL int min_depth, max_depth;

GLOBAL int current_row;
GLOBAL int current_depth;
GLOBAL G3D_Region current_region;
int round();

#define ISNULL_D(x)  G3d_isNullValueNum (x, G3D_DOUBLE)
#define SETNULL_D(x) G3d_setNullValue (x, 1, G3D_DOUBLE)

