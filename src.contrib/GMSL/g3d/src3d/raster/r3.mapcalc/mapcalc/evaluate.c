#include "glob.h"
#include "function.h"
evaluate (nrows, ncols, ndepth)
{
    int i,n,n2,d;
    int nargs,k;
    int col;
    double *xcell,*pomxcell;
    char *get_buffer_from_pool() ;
    char *bp;

    int (*xcall)();
    int add_x();
    int subtract_x();
    int divide_x();
    int multiply_x();
    int modulus_x();
    int power_x();
    int gt_x();
    int ge_x();
    int lt_x();
    int le_x();
    int eq_x();
    int ne_x();
    int or_x();
    int and_x();

    execute_stack_depth = 0;

    for (i = 0; i < expression_stack_depth; i++)
    {
	d = execute_stack_depth - 1 ;

/* perform a double check on the execution stack.
 * binary ops, comparison, logicals must have two operands.
 * UMINUS, FUNCTIONS check themselves (see ahead)
 * all operands are typed as MAP  (see ahead)
 */
	switch (expression_stack[i].type)
	{
	case GT:
	case GE:
	case LT:
	case LE:
	case EQ:
	case NE:
	case OR:
	case AND:
	case '+':
	case '-':
	case '*':
	case '/':
	case '%':
	case '^':
	    if (d < 1) return 0;
	    if (execute_stack[d].type != MAP) return 0;
	    if (execute_stack[d-1].type != MAP) return 0;
	    execute_stack_depth--;
	}

	switch (expression_stack[i].type)
	{
	case '+': xcall = add_x;       break;
	case '-': xcall = subtract_x;  break;
	case '*': xcall = multiply_x;  break;
	case '/': xcall = divide_x;    break;
	case '%': xcall = modulus_x;   break;
	case '^': xcall = power_x;     break;
	case GT:  xcall = gt_x;        break;
	case GE:  xcall = ge_x;        break;
	case LT:  xcall = lt_x;        break;
	case LE:  xcall = le_x;        break;
	case EQ:  xcall = eq_x;        break;
	case NE:  xcall = ne_x;        break;
	case OR:  xcall = or_x;        break;
	case AND: xcall = and_x;       break;
	}
	switch (expression_stack[i].type)
	{
/* binary operations */
	case '+':
	case '-':
	case '/':
	case '*':
	case '%':
	case '^':
	case GT:
	case GE:
	case LT:
	case LE:
	case EQ:
	case NE:
	case OR:
	case AND:
            (*xcall) (execute_stack[d-1].xcell, execute_stack[d].xcell, ncols);
	    return_buffer_to_pool (execute_stack[d].n);
	    break;
	case UMINUS:
	    if (d < 0) return 0;
	    if (execute_stack[d].type != MAP) return 0;
	    col = ncols;

            xcell = execute_stack[d].xcell;
	    while (col-- > 0)
	    {
	       if (!ISNULL_D(xcell))
	       {
	            *xcell = -(*xcell);
	       }
	       xcell++;
	    }
	    break;

/* set a variable */
	case SET_VARIABLE:
	    if (d < 0) return 0;
	    if (execute_stack[d].type != MAP) return 0;
	    k = expression_stack[i].k;  /* variable number */

            copy_double_to_variable (execute_stack[d].xcell, k, ncols);
	    break;


/* operands */
	case GET_VARIABLE:
	    d = execute_stack_depth;
	    grow_execute_stack();
	    k = expression_stack[i].k;  /* variable number */

            xcell = (double *) get_buffer_from_pool (&n);
            copy_variable_to_double (xcell, k, ncols);
            
	    execute_stack[d].xcell = xcell;
	    execute_stack[d].n = n;
	    execute_stack[d].type = MAP;
	    break;

	case MAP:
	    d = execute_stack_depth;
	    grow_execute_stack();

	    bp = get_buffer_from_pool (&n);

            xcell = execute_stack[d].xcell = (double *)bp;

            execute_stack[d].n = n;
	    execute_stack[d].type = MAP;

            if (!readmap (expression_stack[i].k,
                execute_stack[d].xcell,
                expression_stack[i].row, expression_stack[i].col, expression_stack[i].depth,
		nrows, ncols, ndepth))
		    return 0;
	    switch (expression_stack[i].nargs)
	    {
	    case '@': /* convert from cats */
		pomxcell = (double *) get_buffer_from_pool (&n2);
		translate_from_cats (xcell, pomxcell, ncols, expression_stack[i].k);
                xcell = execute_stack[d].xcell = pomxcell;
		execute_stack[d].n = n2;
                return_buffer_to_pool (n);
		break;
	    case '#': /* convert from colors */
	    case 'r':
	    case 'g':
	    case 'b':
                /* need XCELL buffer to store result */
                pomxcell = (double *) get_buffer_from_pool (&n2);
		translate_from_colors (xcell, pomxcell, ncols, expression_stack[i].k, expression_stack[i].nargs);
                /* make pomxcell active and xcell inactive */
		xcell = execute_stack[d].xcell = pomxcell;
		execute_stack[d].n = n2;
		return_buffer_to_pool (n);
		break;
	    }
	    break;
	case DOUBLE:
	    d = execute_stack_depth;
	    grow_execute_stack();

	    xcell = (double *) get_buffer_from_pool (&n);
	    execute_stack[d].xcell = xcell;
	    execute_stack[d].n = n;
	    execute_stack[d].type = MAP;
	    col = ncols;
            while (col-- > 0)
                *xcell++ = (double) expression_stack[i].x;
	    break;

/* functions */
	case FUNCTION:
	/* check that we have nargs and all are type MAP */
	    nargs = expression_stack[i].nargs;
	    if (execute_stack_depth < nargs)
		return 0;
	    for (d = 1; d <= nargs; d++)
		if (execute_stack[execute_stack_depth - d].type != MAP)
		    return 0;

	/* convert arguments(s) to double if necessary */
	    k = expression_stack[i].k;	/* function number */
            for (d = 1; d <= nargs; d++)
	    {
		    xargs[nargs-d] = execute_stack[execute_stack_depth - d].xcell;
	    }
            xcell = (double *) get_buffer_from_pool (&n);
            if (function_list[k].xcall == NOFUNC)
	    {
       		for (d=0; d < ncols; d++)
			    xcell[d] = 0.0;
	    }
       	    (*function_list[k].xcall)(nargs,xargs,xcell,ncols);

            for (d = 1; d <= nargs; d++)
		return_buffer_to_pool (execute_stack[execute_stack_depth - d].n);

            execute_stack_depth -= nargs;
	    d = execute_stack_depth;
	    grow_execute_stack();
	    execute_stack[d].n = n;
	    execute_stack[d].type = MAP;
            execute_stack[d].xcell = xcell;
	    break;

/* other - shouldn't happen */
	default:
	    fprintf (stderr, "evaluate: SHOULDN'T HAPPEN!!\n");
	    return 0;
	}
    }
    return execute_stack_depth == 1;
}

grow_execute_stack()
{
    if (execute_stack_depth++ >= execute_stack_nalloc)
    {
	execute_stack_nalloc += 16;
	execute_stack =
	    (struct execute_stack *) G3d_realloc (execute_stack,
		execute_stack_nalloc * sizeof(struct execute_stack));
    }
    if (execute_stack_depth > execute_stack_depth_max)
	execute_stack_depth_max = execute_stack_depth;
}
