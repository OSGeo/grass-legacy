
#include <stdlib.h>

#include "mapcalc.h"

/****************************************************************************/

static void initialize(expression *e);
static void evaluate(expression *e);

/****************************************************************************/

static void allocate_buf(expression *e)
{
	e->buf = malloc(columns * G_raster_size(e->res_type));
}

static void set_buf(expression *e, void *buf)
{
	e->buf = buf;
}

/****************************************************************************/

static void initialize_constant(expression *e)
{
	allocate_buf(e);
}

static void initialize_variable(expression *e)
{
	set_buf(e, e->data.var.bind->data.bind.val->buf);
}

static void initialize_map(expression *e)
{
	allocate_buf(e);
	e->data.map.idx = open_map(e->data.map.name, e->data.map.mod,
				   e->data.map.row, e->data.map.col);
}

static void initialize_function(expression *e)
{
	int i;

	allocate_buf(e);

	e->data.func.argv[0] = e->buf;

	for (i = 1; i <= e->data.func.argc; i++)
	{
		initialize(e->data.func.args[i]);
		e->data.func.argv[i] = e->data.func.args[i]->buf;
	}
}

static void initialize_binding(expression *e)
{
	initialize(e->data.bind.val);
	set_buf(e, e->data.bind.val->buf);
}

static void initialize(expression *e)
{
	switch (e->type)
	{
	case expr_type_constant:	initialize_constant(e);	break;
	case expr_type_variable:	initialize_variable(e);	break;
	case expr_type_map:		initialize_map(e);	break;
	case expr_type_function:	initialize_function(e);	break;
	case expr_type_binding:		initialize_binding(e);	break;
	default:
		G_fatal_error("internal error: initialize: unknown type: %d",
			      e->type);
	}
}

/****************************************************************************/

static void evaluate_constant(expression *e)
{
	int *ibuf = e->buf;
	float *fbuf = e->buf;
	double *dbuf = e->buf;
	int i;

	switch (e->res_type)
	{
	case CELL_TYPE:
		for (i = 0; i < columns; i++)
			ibuf[i] = e->data.con.ival;
		break;

	case FCELL_TYPE:
		for (i = 0; i < columns; i++)
			fbuf[i] = e->data.con.fval;
		break;

	case DCELL_TYPE:
		for (i = 0; i < columns; i++)
			dbuf[i] = e->data.con.fval;
		break;
	default:
		G_fatal_error("internal error: evaluate_constant: invalid type: %d", e->res_type);
	}
}

static void evaluate_variable(expression *e)
{
	/* this is a no-op */
}

static void evaluate_map(expression *e)
{
	get_map_row(e->data.map.idx,
		    e->data.map.mod,
		    current_row + e->data.map.row,
		    e->data.map.col,
		    e->buf, e->res_type);
}

static void evaluate_function(expression *e)
{
	int i;
	int res;

	for (i = 1; i <= e->data.func.argc; i++)
		evaluate(e->data.func.args[i]);

	res = (*e->data.func.func)(e->data.func.argc,
				   e->data.func.argt,
				   e->data.func.argv);

	switch (res)
	{
	case E_ARG_LO:
		G_fatal_error("too few arguments for function '%s'", e->data.func.name);
		break;
	case E_ARG_HI:
		G_fatal_error("too many arguments for function '%s'", e->data.func.name);
		break;
	case E_ARG_TYPE:
		G_fatal_error("invalid argument type for function '%s'", e->data.func.name);
		break;
	case E_RES_TYPE:
		G_fatal_error("invalid return type for function '%s'", e->data.func.name);
		break;
	case E_INV_TYPE:
		G_fatal_error("unknown type for function '%s'", e->data.func.name);
		break;
	case E_WTF:
		G_fatal_error("unknown error for function '%s'", e->data.func.name);
		break;
	}
}

static void evaluate_binding(expression *e)
{
	evaluate(e->data.bind.val);
}

/****************************************************************************/

static void evaluate(expression *e)
{
	switch (e->type)
	{
	case expr_type_constant:	evaluate_constant(e);	break;
	case expr_type_variable:	evaluate_variable(e);	break;
	case expr_type_map:		evaluate_map(e);	break;
	case expr_type_function:	evaluate_function(e);	break;
	case expr_type_binding:		evaluate_binding(e);	break;
	default:
		G_fatal_error("internal error: evaluate: unknown type: %d",
			      e->type);
	}
}

/****************************************************************************/

static expr_list *exprs;

static int error_handler(char *msg, int fatal)
{
	expr_list *l;

	for (l = exprs; l; l = l->next)
	{
		expression *e = l->exp;
		int fd = e->data.bind.fd;
		if (fd >= 0)
			G_unopen_cell(fd);
	}

	G_unset_error_routine();
	G_fatal_error("%s", msg);
}

/****************************************************************************/

void execute(expr_list *ee)
{
	expr_list *l;

	rows = G_window_rows();
	columns = G_window_cols();

	setup_maps();

	exprs = ee;
	G_set_error_routine(error_handler);

	for (l = ee; l; l = l->next)
	{
		expression *e = l->exp;
		const char *var;
		expression *val;

		if (e->type != expr_type_binding)
			G_fatal_error(
				"internal error: execute: invalid type: %d",
				e->type);

		print_expression(stderr, e);
		fprintf(stderr, "\n");

		initialize(e);

		var = e->data.bind.var;
		val = e->data.bind.val;

		e->data.bind.fd = open_output_map(var, val->res_type);
	}

	for (current_row = 0; current_row < rows; current_row++)
	{
		G_percent (current_row, rows, 2);

		for (l = ee; l; l = l->next)
		{
			expression *e = l->exp;
			const char *var = e->data.bind.var;
			int fd = e->data.bind.fd;

			evaluate(e);
			put_map_row(fd, e->buf, e->res_type);
		}
	}

	G_percent(current_row, rows, 2);

	for (l = ee; l; l = l->next)
	{
		expression *e = l->exp;
		const char *var = e->data.bind.var;
		expression *val = e->data.bind.val;
		int fd = e->data.bind.fd;

		close_output_map(fd);
		e->data.bind.fd = -1;

		if (val->type == expr_type_map)
		{
			if (val->data.map.mod == 'M')
			{
				copy_cats(var, val->data.map.idx);
				copy_colors(var, val->data.map.idx);
			}

			copy_history(var, val->data.map.idx);
		}
	}

	G_unset_error_routine();
}

/****************************************************************************/

