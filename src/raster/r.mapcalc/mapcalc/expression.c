#include "glob.h"
#include <unistd.h>
#include "mapcalc.h"

int 
push_expression_map (int k, int flag, int row, int col)
{
    return push_expression (MAP, k, flag, (double)0.0, row, col) ;
}

int 
push_expression_set_variable (int k)
{
    return push_expression (SET_VARIABLE, k, 0, (double)0.0, 0, 0) ;
}

int 
push_expression_get_variable (int k)
{
    return push_expression (GET_VARIABLE, k, 0, (double)0.0, 0, 0) ;
}

int 
push_expression_integer (int k, int row, int col)
{
    return push_expression (INTEGER, k, 0, (double)0.0, row, col) ;
}

int 
push_expression_double (double x, int row, int col)
{
    return push_expression (DOUBLE, 0, 0, x, row, col) ;
}

int 
push_expression_op (int op)
{
    return push_expression (op, 0, 0, (double)0.0, 0, 0) ;
}

int 
push_expression_function (int k, int nargs)
{
    return push_expression (FUNCTION, k, nargs, (double)0.0, 0, 0) ;
}
int 
push_expression (int type, int k, int nargs, double x, int row, int col)
{
    if (expression_stack_depth >= expression_stack_nalloc)
    {
	expression_stack_nalloc += 16;
	expression_stack =
	   (struct expression_stack *) G_realloc (expression_stack,
	      expression_stack_nalloc * sizeof(struct expression_stack));
    }
    expression_stack[expression_stack_depth].type = type;
    expression_stack[expression_stack_depth].k = k;
    expression_stack[expression_stack_depth].nargs = nargs;
    expression_stack[expression_stack_depth].x = x;
    expression_stack[expression_stack_depth].row = row;
    expression_stack[expression_stack_depth].col = col;
    expression_stack_depth++;

    return 1;
}

int 
free_expression_stack (void)
{
    closemaps();
    expression_stack_depth = 0;

    return 0;
}
