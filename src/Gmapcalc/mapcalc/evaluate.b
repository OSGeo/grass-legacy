#include "glob.h"
#include "function.h"
evaluate (row, nrows, ncols)
{
    int i,n,d;
    int nargs,k;
    int col;
    CELL *cell;
    double *xcell;
    int use_double;
    int get_double;
    char *get_buffer_from_pool() ;

    int (*icall)(), (*xcall)();
    int add(), add_x();
    int subtract(), subtract_x();
    int divide(), divide_x();
    int multiply(), multiply_x();
    int modulus(), modulus_x();
    int gt_i(), gt_x();
    int ge_i(), ge_x();
    int lt_i(), lt_x();
    int le_i(), le_x();
    int eq_i(), eq_x();
    int ne_i(), ne_x();
    int or_i(), or_x();
    int and_i(), and_x();

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
	    if (d < 1) return 0;
	    if (execute_stack[d].type != MAP) return 0;
	    if (execute_stack[d-1].type != MAP) return 0;
	    execute_stack_depth--;
	    if (buffer_is_double (d) || buffer_is_double(d-1))
	    {
		convert_to_double (d);
		convert_to_double (d-1);
		use_double = 1;
	    }
	    else
		use_double = 0;
	}

	switch (expression_stack[i].type)
	{
	case '+': icall = add;      xcall = add_x;      get_double=1; break;
	case '-': icall = subtract; xcall = subtract_x; get_double=1; break;
	case '*': icall = multiply; xcall = multiply_x; get_double=1; break;
	case '/': icall = divide;   xcall = divide_x;   get_double=1; break;
	case '%': icall = modulus;  xcall = modulus_x;  get_double=1; break;
	case GT:  icall = gt_i;     xcall = gt_x;       get_double=0; break;
	case GE:  icall = ge_i;     xcall = ge_x;       get_double=0; break;
	case LT:  icall = lt_i;     xcall = lt_x;       get_double=0; break;
	case LE:  icall = le_i;     xcall = le_x;       get_double=0; break;
	case EQ:  icall = eq_i;     xcall = eq_x;       get_double=0; break;
	case NE:  icall = ne_i;     xcall = ne_x;       get_double=0; break;
	case OR:  icall = or_i;     xcall = or_x;       get_double=0; break;
	case AND: icall = and_i;    xcall = and_x;      get_double=0; break;
	}
	switch (expression_stack[i].type)
	{
/* binary operations */
	case '+':
	case '-':
	case '/':
	case '*':
	case '%':
	case GT:
	case GE:
	case LT:
	case LE:
	case EQ:
	case NE:
	case OR:
	case AND:
	    if (use_double)
		(*xcall) (execute_stack[d-1].xcell, execute_stack[d].xcell, ncols);
	    else
		(*icall) (execute_stack[d-1].cell, execute_stack[d].cell, ncols);
	    return_buffer_to_pool (execute_stack[d].n);
	    if (!get_double)
		convert_to_cell (d-1);
	    break;
	case UMINUS:
	    if (d < 0) return 0;
	    if (execute_stack[d].type != MAP) return 0;
	    col = ncols;
	    if (buffer_is_double(d))
	    {
		xcell = execute_stack[d].xcell;
		while (col-- > 0)
		{
		    *xcell = -(*xcell);
		    xcell++;
		}
	    }
	    else
	    {
		cell = execute_stack[d].cell;
		while (col-- > 0)
		{
		    *cell = -(*cell);
		    cell++;
		}
	    }
	    break;

/* operands */
	case MAP:
	    d = execute_stack_depth;
	    grow_execute_stack();

	    cell = (CELL *) get_buffer_from_pool (&n);
	    execute_stack[d].cell = cell;
	    execute_stack[d].xcell = NULL;
	    execute_stack[d].n = n;
	    execute_stack[d].type = MAP;

	    if (!getrow (expression_stack[i].k, cell,
		row+expression_stack[i].row, expression_stack[i].col,
		nrows, ncols))
		    return 0;
	    if (expression_stack[i].nargs >= 0)	/* convert from cats */
	    {
		int n2;
		xcell = (double *) get_buffer_from_pool (&n2);
		translate_from_cats (cell, xcell, ncols, expression_stack[i].nargs);
		execute_stack[d].cell = NULL;
		execute_stack[d].xcell = xcell;
		execute_stack[d].n = n2;
		return_buffer_to_pool (n);
	    }
	    break;
	case INTEGER:
	    d = execute_stack_depth;
	    grow_execute_stack();

	    cell = (CELL *) get_buffer_from_pool (&n);
	    execute_stack[d].cell = cell;
	    execute_stack[d].xcell = NULL;
	    execute_stack[d].n = n;
	    execute_stack[d].type = MAP;
	    col = ncols;
	    while (col-- > 0)
		*cell++ = expression_stack[i].k;
	    break;
	case DOUBLE:
	    d = execute_stack_depth;
	    grow_execute_stack();

	    xcell = (double *) get_buffer_from_pool (&n);
	    execute_stack[d].xcell = xcell;
	    execute_stack[d].cell = NULL;
	    execute_stack[d].n = n;
	    execute_stack[d].type = MAP;
	    col = ncols;
	    while (col-- > 0)
		*xcell++ = expression_stack[i].x;
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
	    get_double = function_list[k].double_result;
	    use_double = (function_list[k].icall == NOFUNC);
	    if (!use_double)
		for (d = 1; d <= nargs; d++)
		    if (buffer_is_double (execute_stack_depth - d))
		    {
			use_double = 1;
			break;
		    }
	    if (use_double)
	    {
		for (d = 1; d <= nargs; d++)
		{
		    convert_to_double (execute_stack_depth - d);
		    xargs[nargs-d] = execute_stack[execute_stack_depth - d].xcell;
		}
		if (get_double)
		    xcell = (double *) get_buffer_from_pool (&n);
		else
		    cell = (CELL *) get_buffer_from_pool (&n);

		if (function_list[k].xcall == NOFUNC)
		{
		    if (get_double)
			for (d=0; d < ncols; d++)
			    xcell[d] = 0.0;
		    else
			for (d=0; d < ncols; d++)
			    cell[d] = 0;
		}
		else if (get_double)
		    (*function_list[k].xcall)(nargs,xargs,xcell,ncols);
		else
		    (*function_list[k].xcall)(nargs,xargs,cell,ncols);
	    }
	    else
	    {
		for (d = 1; d <= nargs; d++)
		    iargs[nargs-d] = execute_stack[execute_stack_depth - d].cell;
		cell = (CELL *) get_buffer_from_pool (&n);
		(*function_list[k].icall)(nargs,iargs,cell,ncols);
	    }
	    for (d = 1; d <= nargs; d++)
		return_buffer_to_pool (execute_stack[execute_stack_depth - d].n);
	    execute_stack_depth -= nargs;
	    execute_stack[execute_stack_depth].n = n;
	    if (use_double && get_double)
	    {
		execute_stack[execute_stack_depth].xcell = xcell;
		execute_stack[execute_stack_depth].cell = 0;
	    }
	    else
	    {
		execute_stack[execute_stack_depth].xcell = 0;
		execute_stack[execute_stack_depth].cell = cell;
	    }
	    grow_execute_stack();
	    break;

/* other - shouldn't happen */
	default:
	    fprintf (stderr, "evaluate: SHOULDN'T HAPPEN!!\n");
	    return 0;
	}
    }
    convert_to_cell (0);

    return execute_stack_depth == 1;
}

grow_execute_stack()
{
    if (execute_stack_depth++ >= execute_stack_nalloc)
    {
	execute_stack_nalloc += 16;
	execute_stack =
	    (struct execute_stack *) G_realloc (execute_stack,
		execute_stack_nalloc * sizeof(struct execute_stack));
    }
    if (execute_stack_depth > execute_stack_depth_max)
	execute_stack_depth_max = execute_stack_depth;
}
