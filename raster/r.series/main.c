#include <string.h>
#include <stdlib.h>
#include <unistd.h>

#include "gis.h"
#include "glocale.h"

#include "local_proto.h"

struct menu
{
	cfunc *method;	/* routine to compute new value */
	char *name;  	/* method name */
	char *text;	/* menu display - full description */
} menu[] = {
	{c_ave,    "average",   "average value"},
	{c_count,  "count",     "count of non-NULL cells"},
	{c_median, "median",    "median value"},
	{c_mode,   "mode",      "most frequently occuring value"},
	{c_min,    "minimum",   "lowest value"},
	{c_minx,   "min_raster",     "raster with lowest value"}, 
	{c_max,    "maximum",   "highest value"},
	{c_maxx,   "max_raster",     "raster with highest value"}, 
	{c_stddev, "stddev",    "standard deviation"},
	{c_sum,    "sum",       "sum of values"},
	{c_var,    "variance",  "statistical variance"},
	{c_divr,   "diversity", "number of different values"},
	{c_reg_m,  "slope",     "linear regression slope"},
	{c_reg_c,  "offset",    "linear regression offset"},
	{NULL,     NULL,        NULL}
};

struct input
{
	char *name, *mapset;
	int fd;
	DCELL *buf;
};

static char *build_method_list(void)
{
	char *buf = G_malloc(1024);
	char *p = buf;
	int i;

	for (i = 0; menu[i].name; i++)
	{
		char *q;
		if (i)
			*p++ = ',';
		for (q = menu[i].name; *q; p++, q++)
			*p = *q;
	}
	*p = '\0';

	return buf;
}

int main (int argc, char *argv[])
{
	struct GModule *module;
	struct {
		struct Option *input, *output, *method;
	} parm;
	struct {
		struct Flag *quiet;
		struct Flag *nulls;
	} flag;
	int verbose;
	int method;
	cfunc *method_fn;
	int i;
	int num_inputs;
	struct input *inputs;
	char *out_name;
	int out_fd;
	DCELL *out_buf;
	DCELL *values;
	int nrows, ncols;
	int row, col;

	G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		_("Makes each output cell value a "
		"function of the values assigned to the corresponding cells "
		"in the input raster map layers.");

	parm.input = G_define_option() ;
	parm.input->key        = "input" ;
	parm.input->type       = TYPE_STRING ;
	parm.input->required   = YES ;
	parm.input->multiple   = YES ;
	parm.input->gisprompt  = "old,cell,raster" ;
	parm.input->description= _("Names of existing raster files") ;

	parm.output = G_define_option() ;
	parm.output->key        = "output" ;
	parm.output->type       = TYPE_STRING ;
	parm.output->required   = YES ;
	parm.output->gisprompt  = "any,cell,raster" ;
	parm.output->description= _("Name of the new raster file") ;

	parm.method = G_define_option() ;
	parm.method->key        = "method" ;
	parm.method->type       = TYPE_STRING ;
	parm.method->required   = YES ;
	parm.method->options    = build_method_list();
	parm.method->description= _("Aggregate operation") ;

	flag.quiet = G_define_flag();
	flag.quiet->key = 'q';
	flag.quiet->description = _("Run quietly");

	flag.nulls = G_define_flag();
	flag.nulls->key = 'n';
	flag.nulls->description = _("Propagate NULLs");

	if (G_parser(argc,argv))
		exit(1);

	verbose = !flag.quiet->answer;

	/* get the method */
	method = -1;
	for (i = 0; menu[i].name; i++)
		if (strcmp(menu[i].name, parm.method->answer) == 0)
		{
			method = i;
			break;
		}
	if (method < 0)
		G_fatal_error("unknown method <%s>", parm.method->answer);

	method_fn = menu[method].method;

	/* process the input maps */
	for (i = 0; parm.input->answers[i]; i++)
		;
	num_inputs = i;
	inputs = G_malloc(num_inputs * sizeof(struct input));

	for (i = 0; i < num_inputs; i++)
	{
		struct input *p = &inputs[i];
		p->name = parm.input->answers[i];
		p->mapset = G_find_cell2(p->name,"");
		if (!p->mapset)
			G_fatal_error("raster file <%s> not found", p->name);
		p->fd = G_open_cell_old(p->name, p->mapset);
		if (p->fd < 0)
			G_fatal_error("unable to open input map <%s> in mapset <%s>",
				      p->name, p->mapset);
		p->buf = G_allocate_d_raster_buf();
	}

	/* process the output map */
	out_name = parm.output->answer;

	out_fd = G_open_raster_new(out_name, DCELL_TYPE);
	if (out_fd < 0)
		G_fatal_error("unable to create output map <%s>", out_name);

	out_buf = G_allocate_d_raster_buf();

	/* initialise variables */
	values = G_malloc(num_inputs * sizeof(DCELL));

	nrows = G_window_rows();
	ncols = G_window_cols();

	/* process the data */
	if (verbose)
		fprintf (stderr, "Percent complete ... ");

	for (row = 0; row < nrows; row++)
	{
		if (verbose)
			G_percent(row, nrows, 2);

		for (i = 0; i < num_inputs; i++)
			G_get_d_raster_row (inputs[i].fd, inputs[i].buf, row);

		for (col = 0; col < ncols; col++)
		{
			int null = 0;

			for (i = 0; i < num_inputs; i++)
			{
				DCELL v = inputs[i].buf[col];

				if (G_is_d_null_value(&v))
					null = 1;

				values[i] = v;
			}

			if (null && flag.nulls->answer)
				G_set_d_null_value(&out_buf[col], 1);
			else
				(*method_fn)(&out_buf[col], values, num_inputs);
		}

		G_put_d_raster_row(out_fd, out_buf);
	}

	if (verbose)
		G_percent(row, nrows, 2);

	/* close maps */
	G_close_cell(out_fd);

	for (i = 0; i < num_inputs; i++)
		G_close_cell(inputs[i].fd);

	exit(0);
}

