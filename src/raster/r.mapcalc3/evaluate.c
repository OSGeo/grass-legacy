
#include <stdlib.h>

#include "mapcalc.h"

/****************************************************************************/

static int initialize(expression *e);
static int evaluate(expression *e);

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

static int initialize_constant(expression *e)
{
	allocate_buf(e);
	return 0;
}

static int initialize_variable(expression *e)
{
	set_buf(e, e->data.var.bind->data.bind.val->buf);
	return 0;
}

static int initialize_map(expression *e)
{
	allocate_buf(e);
	e->data.map.idx = open_map(e->data.map.name, e->data.map.mod,
				   e->data.map.row, e->data.map.col);
	return 0;
}

static int initialize_function(expression *e)
{
	int i;

	allocate_buf(e);

	e->data.func.argv[0] = e->buf;

	for (i = 1; i <= e->data.func.argc; i++)
	{
		int res = initialize(e->data.func.args[i]);
		if (res != 0)
			return res;
		e->data.func.argv[i] = e->data.func.args[i]->buf;
	}

	return 0;
}

static int initialize_binding(expression *e)
{
	int res = initialize(e->data.bind.val);
	if (res != 0)
		return res;

	set_buf(e, e->data.bind.val->buf);
	return 0;
}

static int initialize(expression *e)
{
	switch (e->type)
	{
	case expr_type_constant:	return initialize_constant(e);
	case expr_type_variable:	return initialize_variable(e);
	case expr_type_map:		return initialize_map(e);
	case expr_type_function:	return initialize_function(e);
	case expr_type_binding:		return initialize_binding(e);
	default:
		fprintf(stderr,
			"internal error: initialize: unknown type: %d\n",
			e->type);
		return E_WTF;
	}
}

/****************************************************************************/

static int evaluate_constant(expression *e)
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
		return 0;

	case FCELL_TYPE:
		for (i = 0; i < columns; i++)
			fbuf[i] = e->data.con.fval;
		return 0;

	case DCELL_TYPE:
		for (i = 0; i < columns; i++)
			dbuf[i] = e->data.con.fval;
		return 0;
	}

	return E_ARG_TYPE;
}

static int evaluate_variable(expression *e)
{
	return 0;
}

static int evaluate_map(expression *e)
{
	return get_map_row(e->data.map.idx,
			   e->data.map.mod,
			   current_row + e->data.map.row,
			   e->data.map.col,
			   e->buf, e->res_type);
}

static int evaluate_function(expression *e)
{
	int i;

	for (i = 1; i <= e->data.func.argc; i++)
	{
		int res = evaluate(e->data.func.args[i]);
		if (res != 0)
			return res;
	}

	return (*e->data.func.func)(e->data.func.argc,
				    e->data.func.argt,
				    e->data.func.argv);
}

static int evaluate_binding(expression *e)
{
	return evaluate(e->data.bind.val);
}

/****************************************************************************/

static int evaluate(expression *e)
{
	switch (e->type)
	{
	case expr_type_constant:	return evaluate_constant(e);
	case expr_type_variable:	return evaluate_variable(e);
	case expr_type_map:		return evaluate_map(e);
	case expr_type_function:	return evaluate_function(e);
	case expr_type_binding:		return evaluate_binding(e);
	default:
		fprintf(stderr,
			"internal error: evaluate: unknown type: %d\n",
			e->type);
		return E_WTF;
	}
}

/****************************************************************************/

int execute(expression *e)
{
	const char *out_name;
	expression *ee;
	int out_fd;

	if (e->type != expr_type_binding)
		return E_WTF;

	out_name = e->data.bind.var;
	ee = e->data.bind.val;

	print_expression(stderr, e);
	fprintf(stderr, "\n");

	rows = G_window_rows();
	columns = G_window_cols();

	if (initialize(ee) != 0)
	{
		fprintf(stderr, "initialization error\n");
		return 1;
	}

	setup_maps();

	out_fd = open_output_map(out_name, ee->res_type);
	if (!out_fd)
		return -1;

	for (current_row = 0; current_row < rows; current_row++)
	{
		G_percent (current_row, rows, 2);

		if (evaluate(ee) != 0)
		{
			fprintf(stderr, "error evaluating expression\n");
			G_unopen_cell(out_fd);
			return -1;
		}

		if (put_map_row(out_fd, ee->buf, ee->res_type) != 0)
		{
			fprintf(stderr, "error writing output\n");
			G_unopen_cell(out_fd);
			return -1;
		}
	}

	G_percent(current_row, rows, 2);

	if (close_output_map(out_fd) < 0)
	{
		fprintf(stderr, "Can't close output file\n");
		return -1;
	}

	return 0;
}

/****************************************************************************/

