#include <unistd.h>
#include <fcntl.h>
#include "rowio.h"
#include "glob.h"
#include "filter.h"
#include "local_proto.h"

int perform_filter (char *in_name, char *in_mapset, char *out_name,
    FILTER *filter, int nfilters, int repeat)
{
    int in;
    int out;
    int n;
    int pass;
    ROWIO r;
    char *tmp1, *tmp2;
    int count;
    int  row;
    CELL *cell;


    cell = G_allocate_cell_buf();

if (!silent)
{fprintf (stderr, "FILTERING [%s] in [%s]", in_name, in_mapset);
 if (repeat>1 || nfilters>1) fprintf (stderr,"\n");
}

    count=0;
    for (pass=0; pass < repeat; pass++ )
    {
if (!silent && (repeat > 1)) fprintf (stderr,"PASS %d%s", pass+1, nfilters>1?"\n":" ...");
	for (n = 0; n < nfilters; n++, count++)
	{
if (!silent)
{
    if(nfilters > 1) fprintf (stderr,"%sFILTER %d ...", repeat>1?" ":"",n+1);
    else if(repeat==1) fprintf (stderr," ...");
    fflush(stderr);
}

	    if (count==0)
	    {
		in = G_open_cell_old (in_name, in_mapset);
#ifdef DEBUG
fprintf (stderr,"open raster file %s in %s = %d\n", in_name, in_mapset, in);
#endif
		if (in < 0)
		{
		    char msg[100];
		    sprintf (msg, "unable to open raster file [%s] in [%s]",
			in_name, in_mapset);
		    G_fatal_error (msg);
		}
		close(creat(tmp1=G_tempfile(),0666));
		out = open (tmp1, 2);
		if (out < 0)
		    G_fatal_error ("unable to create a temporary file");
	    }
	    else if (count==1)
	    {
#ifdef DEBUG
fprintf (stderr,"close raster file\n");
#endif
		G_close_cell(in);
		in = out;
		close(creat(tmp2=G_tempfile(),0666));
		out = open (tmp2, 2);
		if (out < 0)
		    G_fatal_error ("unable to create a temporary file");
	    }
	    else
	    {
		int fd;

#ifdef DEBUG
fprintf (stderr,"swap temp files\n");
#endif
		fd = in;
		in = out;
		out = fd;
	    }

	    rowio_setup (&r, in, filter[n].size, buflen, count?getrow:getmaprow, NULL);

	    execute_filter (&r, out, &filter[n], cell);

	    rowio_release (&r);
if (!silent) fprintf (stderr,"\n");
	}
    }

    if (count==1)
	G_close_cell(in);
    else if (count > 1)
	close(in);

/* copy final result to output cell file */
    in = out;
    out = G_open_cell_new (out_name);
    if (out < 0)
    {
	char msg[100];
	sprintf (msg, "unable to create raster file [%s] in [%s]",
	    out_name, G_mapset());
	G_fatal_error (msg);
    }

if (!silent) fprintf (stderr,"WRITING [%s]\n", out_name);
    for (row=0; row<nrows; row++)
    {
	getrow (in, cell, row, buflen);
	/* G_put_map_row (out, cell); */
	G_put_c_raster_row (out, cell);
    }

/* remove the temporary files before closing so that the G_close_cell()
   has more disk to work with
*/
if (!silent) fprintf (stderr,"CREATING SUPPORT FILES\n");
    if (count > 0)
	unlink (tmp1);
    if (count > 1)
	unlink (tmp2);
    G_close_cell (out);

    return 0;
}
