#include "glob.h"
push_expression_map (k, flag, row, col, depth)
{
    return push_expression (MAP, k, flag, (double)0.0, row, col, depth) ;
}

push_expression_set_variable (k)
{
    return push_expression (SET_VARIABLE, k, 0, (double)0.0, 0, 0, 0) ;
}

push_expression_get_variable (k)
{
    return push_expression (GET_VARIABLE, k, 0, (double)0.0, 0, 0, 0) ;
}

push_expression_integer (k, row, col, depth)
{
    return push_expression (DOUBLE, 0, 0, (double) k, row, col, depth) ;
}

push_expression_double (x, row, col, depth)
    double x;
{
    return push_expression (DOUBLE, 0, 0, x, row, col, depth) ;
}

push_expression_op (op)
{
    return push_expression (op, 0, 0, (double)0.0, 0, 0, 0) ;
}

push_expression_function (k, nargs)
{
    return push_expression (FUNCTION, k, nargs, (double)0.0, 0, 0, 0) ;
}
push_expression (type, k, nargs, x, row, col, depth)
    double x;
{
    if (expression_stack_depth >= expression_stack_nalloc)
    {
	expression_stack_nalloc += 16;
	expression_stack =
	   (struct expression_stack *) G3d_realloc (expression_stack,
	      expression_stack_nalloc * sizeof(struct expression_stack));
    }
    expression_stack[expression_stack_depth].type = type;
    expression_stack[expression_stack_depth].k = k;
    expression_stack[expression_stack_depth].nargs = nargs;
    expression_stack[expression_stack_depth].x = x;
    expression_stack[expression_stack_depth].row = row;
    expression_stack[expression_stack_depth].col = col;
    expression_stack[expression_stack_depth].depth = depth;
    expression_stack_depth++;

    return 1;
}

free_expression_stack()
{
    closemaps();
    expression_stack_depth = 0;
}
