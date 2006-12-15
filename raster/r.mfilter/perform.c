#include <unistd.h>
#include <fcntl.h>
#include <grass/rowio.h>
#include <grass/glocale.h>
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

    G_message(_("Filtering [%s] in [%s]"), in_name, in_mapset);

    count=0;
    for (pass=0; pass < repeat; pass++ ) {
        G_debug(1,"Pass %d", pass+1);
	for (n = 0; n < nfilters; n++, count++) {
                G_debug (1,"Filter %d", n+1);

	    if (count==0) {
		in = G_open_cell_old (in_name, in_mapset);
#ifdef DEBUG
                G_message (_("Open raster map %s in %s = %d"), in_name, in_mapset, in);
#endif
		if (in < 0) {
		    char msg[100];
		    sprintf (msg, "unable to open raster map [%s] in [%s]",
			in_name, in_mapset);
		    G_fatal_error (msg);
		}
		close(creat(tmp1=G_tempfile(),0666));
		out = open (tmp1, 2);
		if (out < 0)
		    G_fatal_error ("unable to create a temporary file");
	    }
	    else if (count==1) {
#ifdef DEBUG
                G_message (_("Closing raster map"));
#endif
		G_close_cell(in);
		in = out;
		close(creat(tmp2=G_tempfile(),0666));
		out = open (tmp2, 2);
		if (out < 0)
		    G_fatal_error ("unable to create a temporary file");
	    }
	    else {
		int fd;

#ifdef DEBUG
                G_message(_("Swap temp files"));
#endif
		fd = in;
		in = out;
		out = fd;
	    }

	    rowio_setup (&r, in, filter[n].size, buflen, count?getrow:getmaprow, NULL);

	    execute_filter (&r, out, &filter[n], cell);

	    rowio_release (&r);
	}
    }

    if (count==1)
	G_close_cell(in);
    else if (count > 1)
	close(in);

/* copy final result to output raster map */
    in = out;
    out = G_open_cell_new (out_name);
    if (out < 0) {
	char msg[100];
	sprintf (msg, "unable to create raster map [%s] in [%s]",
	    out_name, G_mapset());
	G_fatal_error (msg);
    }

    G_message (_("Writing [%s]"), out_name);
    for (row=0; row<nrows; row++){
	getrow (in, cell, row, buflen);
	G_put_raster_row (out, cell, CELL_TYPE);
    }

/* remove the temporary files before closing so that the G_close_cell()
   has more disk to work with
*/
    if (count > 0)
	unlink (tmp1);
    if (count > 1)
	unlink (tmp2);
    G_close_cell (out);

    return 0;
}
