/* %W%  %G% */
#define MAIN
#include "filter.h"
#include "glob.h"

main(argc,argv) char *argv[];
{
    FILTER *filter;
    int nfilters;
    int repeat;
    int fd;

    char in_name[100], *in_mapset;
    char filt_name[300];
    char out_name[100];
    char title[100];
    char temp[300];
    int iflag, oflag, fflag, tflag;
    int rflag;
    int i;

    G_gisinit (argv[0]);

    silent = 0;
    preserve_edges = 0;
    zero_only = 0;
    iflag = oflag = fflag = tflag = rflag = 0;
    for (i = 1; i < argc; i++)
    {
	if (argv[i][0] == '-')
	{
	    char *o;
	    o = &argv[i][1];
	    if (*o == 0) o--;
	    while (*o)
	    {
		switch (*o++)
		{
		case 'v': silent++; break;
		case '0': zero_only = 1; break;
		case 'p': preserve_edges = 1; break;
		default:  usage (argv[0]);
		}
	    }
	    continue;
	}
	if (sscanf (argv[i],"if=%s",in_name) == 1)
	{
	    if (iflag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i],"of=%s",out_name) == 1)
	{
	    if (oflag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i],"ff=%s",filt_name) == 1)
	{
	    if (fflag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i],"repeat=%d",&repeat) == 1)
	{
	    if (rflag++) usage(argv[0]);
	    continue;
	}
	if (sscanf (argv[i],"title=%s",title) == 1)
	{
	    if (tflag++) usage(argv[0]);
	    continue;
	}
	usage (argv[0]);
    }
    if (!fflag || !iflag || !oflag) usage(argv[0]);
    if (!rflag) repeat = 1;
    if (repeat <= 0) usage(argv[0]);
    in_mapset = G_find_cell2 (in_name,"");
    if (in_mapset == NULL)
    {
	sprintf (temp, "%s: cell file not found", in_name);
	G_fatal_error (temp);
    }

    nrows = G_window_rows();
    ncols = G_window_cols();
    buflen = ncols * sizeof (CELL);

/* get the filter */
    filter = get_filter (filt_name, &nfilters, temp);

/* make sure filter matrix won't extend outside the cell file */
    for (i=0; i < nfilters; i++)
	if (filter[i].size > ncols || filter[i].size > nrows)
	{
	    sprintf (temp,"%s: cell file too small for the size of the filter", argv[0]);
	    G_fatal_error (temp);
	}

/* make a title for result */
    if (!tflag)
    {
	if (*temp == 0)
	    strcpy (temp, "unknown filter");
	sprintf (title, "%s filtered using %s", in_name, temp);
    }


    perform_filter (in_name, in_mapset, out_name, filter, nfilters, repeat);

    G_put_cell_title (out_name, title);
}
