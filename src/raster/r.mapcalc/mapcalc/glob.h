#include "gis.h"

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
    int row,col; /* map row/col for neighborhoods */
};

struct execute_stack
{
    int type;  /* opcode, INTEGER, MAP, etc */
    CELL *cell;
    double *xcell;
    int n;
};



#ifndef MAIN
#define GLOBAL extern
#else
#define GLOBAL
#endif

GLOBAL int cp_cats;
GLOBAL char catmap[1024];

GLOBAL int expression_stack_depth;
GLOBAL int expression_stack_nalloc ;
GLOBAL struct expression_stack *expression_stack;

GLOBAL int execute_stack_depth;
GLOBAL int execute_stack_nalloc;
GLOBAL struct execute_stack *execute_stack;
GLOBAL int execute_stack_depth_max;

GLOBAL int floating_point_exception;
GLOBAL int floating_point_exception_occurred;
GLOBAL int overflow_occurred;
GLOBAL int max_nargs;
GLOBAL CELL **iargs;
GLOBAL double **xargs;
GLOBAL int HUGE;
GLOBAL CELL min_value, max_value;
GLOBAL int min_row, max_row;
GLOBAL int min_col, max_col;
GLOBAL int max_rows_in_memory;    /* for map neighboorhoods */

GLOBAL int current_row;
GLOBAL struct Cell_head current_region;

#define ISNULL(x)    G_is_c_null_value(x)
#define ISNULL_D(x)  G_is_d_null_value(x)

#define SETNULL(x)   G_set_c_null_value(x,1)
#define SETNULL_D(x) G_set_d_null_value(x,1)

