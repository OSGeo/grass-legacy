/**************************************************************************
 the following function table describes the currently implemented functions
 for the map calculator

 the function table is defined as follows

    name           name of the function as specified by the user
    icall          routine to call if the input args are CELL buffers
                   if set to NOFUNC, all input args are converted to double
                   and the xcall routine is called.
    xcall          routine to call if the input args are doubles.
                   if set to NOFUNC, the result is a zeroed buffer.
    nargs          routine to call to check number of args
		   should return 1 if ok, 0 otherwise and print error msg.
    double_result  for xcall only. if true the result from the xcall routine
                   is double. else it is assumed to be CELL (ie, the
                   xcall routine produces CELL from doubles)

 note: when writing a new function, make every effort to do something
       reasonable with floating point exceptions. there is a mechanism
       in place which makes this fairly simple to do. look at the 
       implementation of the log() function (xlog.c) to see how friendly
       this function is.
***************************************************************************/

#define NOFUNC ( (int(*)())0 )

struct function_list
{
    char *name;
    int (*icall)();	/* cell arguments, cell result  */
    int (*xcall)();	/* double arguments */
    int (*nargs)();     /* argument checker */
    int double_result;  /* xcall() produces double result or not */
};

#ifndef MAIN
    extern struct function_list function_list[];
#else 

/****************** FUNCTION DECLARATIONS *********************************/

#include "func_proto.h"

struct function_list function_list[]=
{
    {"abs",   i_abs,   x_abs,     n_abs,    1},
    {"eval",  i_eval,  x_eval,    n_eval,   1},
    {"float", NOFUNC,  x_2float,  n_2float, 1},
    {"if",    i_if,    x_if,      n_if,     1},
    {"int",   i_2int,  x_2int,    n_2int,   0},
    {"isnull",i_isnull,x_isnull,  n_isnull, 0},
    {"exp",   NOFUNC,  x_exp,     n_exp,    1},
    {"log",   NOFUNC,  x_log,     n_log,    1},
    {"max",   i_max,   x_max,     n_max,    1},
    {"min",   i_min,   x_min,     n_min,    1},
    {"median",i_median,x_median,  n_median, 1},
    {"mode",  i_mode,  x_mode,    n_mode,   1},
    {"null",  i_null,  x_null,    n_null,   1},
    {"rand",  i_rand,  x_rand,    n_rand,   1},
    {"round", i_round, x_round,   n_round,  0},
    {"sqrt",  NOFUNC,  x_sqrt,    n_sqrt,   1},
    {"sin",   NOFUNC,  x_sin,     n_sin,    1},
    {"cos",   NOFUNC,  x_cos,     n_cos,    1},
    {"tan",   NOFUNC,  x_tan,     n_tan,    1},
    {"atan",  NOFUNC,  x_atan,    n_atan,   1},
    {"col",   i_col,   NOFUNC,    n_col,    1},
    {"row",   i_row,   NOFUNC,    n_row,    1},
    {"x",     NOFUNC,  x_x,       n_x,      1},
    {"y",     NOFUNC,  x_y,       n_y,      1},
    {"ewres", NOFUNC,  x_ewres,   n_ewres,  1},
    {"nsres", NOFUNC,  x_nsres,   n_nsres,  1},

/* add new functions above this line */
    {"",      NOFUNC,  NOFUNC,    NOFUNC,   0}
};

#endif
