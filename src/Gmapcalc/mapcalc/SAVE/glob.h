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

struct expression_stack
{
    int type;    /* opcode, INTEGER, MAP, DOUBLE, FUNCTION */
    int k;       /* integer,  map file descriptor, function number */
    int nargs;   /* number of args to function */
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


#define EXPRESSION_DEPTH 100
#define EXECUTE_DEPTH 150

#ifndef MAIN
#define GLOBAL extern
#else
#define GLOBAL
#endif

GLOBAL int expression_stack_depth;
GLOBAL int execute_stack_depth;
GLOBAL struct expression_stack expression_stack[EXPRESSION_DEPTH];
GLOBAL struct execute_stack execute_stack[EXECUTE_DEPTH];
GLOBAL int floating_point_exception;
GLOBAL int floating_point_exception_occurred;
GLOBAL int max_nargs;
GLOBAL CELL **iargs;
GLOBAL double **xargs;
GLOBAL int HUGE;
GLOBAL CELL min_value, max_value;

CELL round();
