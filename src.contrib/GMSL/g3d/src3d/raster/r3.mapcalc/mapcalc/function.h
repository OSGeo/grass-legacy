/**************************************************************************
 the following function table describes the currently implemented functions
 for the map calculator

 the function table is defined as follows

    name           name of the function as specified by the user
    xcall          routine to call if the input args are doubles.
                   if set to NOFUNC, the result is a zeroed buffer.
    nargs          routine to call to check number of args
		   should return 1 if ok, 0 otherwise and print error msg.

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
    int (*xcall)();	/* double arguments */
    int (*nargs)();     /* argument checker */
};

#ifndef MAIN
    extern struct function_list function_list[];
#else 

/****************** FUNCTON DECLARATIONS *********************************/

int x_sqrt(),   n_sqrt();
int x_exp(),    n_exp();
int x_log(),    n_log();
int x_cos(),    n_cos();
int x_sin(),    n_sin();
int x_tan(),    n_tan();
int x_atan(),   n_atan();
int x_abs(),    n_abs();
int x_if(),     n_if();
int x_max(),    n_max();
int x_median(), n_median();
int x_min(),    n_min();
int x_mode(),   n_mode();
int x_round(),  n_round();
int x_eval(),   n_eval();
int x_rand(),   n_rand();
int x_depth(),  n_depth();
int x_col(),    n_col();
int x_row(),    n_row();
int x_x(),      n_x();
int x_y(),      n_y();
int x_z(),      n_z();
int x_ewres(),  n_ewres();
int x_nsres(),  n_nsres();
int x_tbres(),  n_tbres();
int x_null(),   n_null();
int x_isnull(), n_isnull();

struct function_list function_list[]=
{
    "abs",   x_abs,     n_abs,
    "eval",  x_eval,    n_eval,
    "if",    x_if,      n_if,
    "isnull",x_isnull,  n_isnull,
    "exp",   x_exp,     n_exp,
    "log",   x_log,     n_log,
    "max",   x_max,     n_max,
    "min",   x_min,     n_min,
    "median",x_median,  n_median,
    "mode",  x_mode,    n_mode,
    "null",  x_null,    n_null,
    "rand",  x_rand,    n_rand,
    "round", x_round,   n_round,
    "sqrt",  x_sqrt,    n_sqrt,
    "sin",   x_sin,     n_sin,
    "cos",   x_cos,     n_cos,
    "tan",   x_tan,     n_tan,
    "atan",  x_atan,    n_atan,
    "depth", x_depth,   n_depth,
    "col",   x_col,     n_col,
    "row",   x_row,     n_row,
    "x",     x_x,       n_x,
    "y",     x_y,       n_y,
    "z",     x_z,       n_z,
    "ewres", x_ewres,   n_ewres,
    "nsres", x_nsres,   n_nsres,
    "tbres", x_tbres,   n_tbres,

/* add new functions above this line */
    "",      NOFUNC,    NOFUNC
};

#endif
