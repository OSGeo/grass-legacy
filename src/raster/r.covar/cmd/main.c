#include <math.h>
#include "gis.h"

int main(int argc, char *argv[])
{
    int nrows, ncols;
    DCELL **dcell;
    char *name, *mapset;
    double *sum, **sum2;
    double count;
    double ii,jj;
    int *fd;
    int nfiles;
    int i,j;
    int row, col;
    int maskfd;
    int with_masking;
    int verbose;
    int correlation;
	struct GModule *module;
    struct Option *maps;
    struct
    {
	struct Flag *r;
	struct Flag *q;
    } flag;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Outputs a covariance/correlation matrix "
		"for user-specified raster map layer(s).";

    maps = G_define_option();
    maps->key = "map";
    maps->required = YES;
    maps->multiple = YES;
    maps->type = TYPE_STRING;
    maps->gisprompt  = "old,cell,raster" ;
    maps->description = "Raster map(s) to be read";

    flag.r = G_define_flag();
    flag.r->key = 'r';
    flag.r->description = "Print correlation matrix";

    flag.q = G_define_flag();
    flag.q->key = 'q';
    flag.q->description = "Quiet";

    if (G_parser(argc, argv))
	exit(1);

/* flags */
    verbose = !flag.q->answer;
    correlation = flag.r->answer;

/* count the number of raster maps */
    for (nfiles = 0; maps->answers[nfiles]; nfiles++)
	;

    fd   = (int *) G_malloc (nfiles * sizeof(int));
    dcell = (DCELL **) G_malloc (nfiles * sizeof (DCELL *));
    sum  = (double *) G_calloc (nfiles , sizeof(double));
    sum2 = (double **) G_malloc (nfiles * sizeof(double *));
    for (i = 0; i < nfiles; i++)
    {
	sum2[i] = (double *) G_calloc (nfiles , sizeof(double));
	dcell[i] = G_allocate_d_raster_buf();
	name = maps->answers[i];
	mapset = G_find_cell(name, "");
	if (!mapset)
	{
	    fprintf(stderr, "%s - raster map not found\n", name);
	    exit(1);
	}
	fd[i] = G_open_cell_old (name, mapset);
	if (fd[i] < 0)
	{
	    fprintf(stderr, "%s - can't open raster map\n", name);
	    exit(1);
	}
    }

    nrows = G_window_rows();
    ncols = G_window_cols();

    if (verbose)
	fprintf (stderr, "%s: complete ... ", G_program_name());
    count = 0;
    for (row = 0; row < nrows; row++)
    {
	if (verbose)
	    G_percent (row, nrows, 2);
	for (i = 0; i < nfiles; i++)
	{
  	    if (G_get_d_raster_row (fd[i], dcell[i], row) < 0)
		    exit(1);
	}
	for (col = 0; col < ncols; col++)
	{
	    /* ignore cells where any of the maps has null value */
	    for (i = 0; i < nfiles; i++)
		if(G_is_d_null_value(&dcell[i][col]))
		     break;
            if(i!=nfiles) continue;
	    count++;
	    for (i = 0; i < nfiles; i++)
	    {
		sum[i] += dcell[i][col];
		for (j = 0; j <= i; j++)
		    sum2[j][i] += dcell[i][col]*dcell[j][col];
	    }
	}
    }
    if (verbose)
	G_percent (row, nrows, 2);
    if (count <= 1.1)
    {
	fprintf (stderr, "No non-null values\n");
	exit(1);
    }

    ii = jj = 1.0;
    for (i = 0; i < nfiles; i++)
    {
	if (correlation)
	    ii = sqrt((sum2[i][i] - sum[i]*sum[i]/count)/(count-1));
	for (j = 0; j <= i; j++)
	{
	    if (correlation)
		jj = sqrt((sum2[j][j] - sum[j]*sum[j]/count)/(count-1));
	    fprintf (stdout,"%f ", (sum2[j][i] - sum[i]*sum[j]/count)/(ii*jj*(count-1)));
	}
	for (j = i+1; j < nfiles; j++)
	{
	    if (correlation)
		jj = sqrt((sum2[j][j] - sum[j]*sum[j]/count)/(count-1));
	    fprintf (stdout,"%f ", (sum2[i][j] - sum[i]*sum[j]/count)/(ii*jj*(count-1)));
	}
	fprintf (stdout,"\n");
    }
    exit(0);
}
