/*
** Written by Joshua Caplan, H. Mitasova, L. Mitas, J. Hofierka, M.Zlocha 1994
   US Army Construction Engineering Research Lab, University of Illinois 
** Copyright  Joshua Caplan, H. Mitasova, L. Mitas, J. Hofierka, M.Zlocha 1994
*/

/*
The flowtracing program, both binary and source is copyrighted, but available 
without fee for education, research and non-commercial purposes. Users may 
distribute the binary and source code to third parties provided that the
copyright notice and this statement appears on all copies and that no
charge is made for such copies.  Any entity wishing to integrate all or
part of the source code into a product for  commercial use or resale,
should contact authors of the software, U.S.Army CERL and University
of Illinois.

THE SOFTWARE IS PROVIDED "AS IS" WITHOUT EXPRESS OR IMPLIED WARRANTY. THE
U.S.Army CERL, UNIVERSITY OF ILLINOIS OR AUTHORS SHALL NOT BE LIABLE FOR 
ANY DAMAGES SUFFERED BY THE USER OF THIS SOFTWARE.

By copying this program, you, the user, agree to abide by the copyright
conditions and understandings with respect to any software which is marked
with a copyright notice.
*/

#include "r.flow.11a.h"
#include "io.11a.h"
#include "mem.11a.h"

/************************** MEMORY MGMT/ACCESS **************************/

void
put_row_seg(l, row)
    layer l;
    int   row;
{
    if (segment_put_row(l.seg, l.buf[row] - l.col_offset, 
			row + l.row_offset) < 1)
    {
	sprintf(string, "r.flow: cannot write segment file for %s", l.name);
	G_fatal_error(string);
    }
}

void
initialize_globals(argc, argv)
    int		 argc;
    char	*argv[];
{
    G_gisinit(argv[0]);

    if (G_get_set_window(&region) == -1)
	G_fatal_error("r.flow: error getting current region");

    diag("r.flow Version 11a July 20 1994\n\n");

    parse_command_line(argc, argv);

    el.name = parm.elevin;
    if (parm.aspin)
	as.name = parm.aspin;
    else
	as.name = "internal aspects";
    ds.name = parm.dsout;
    el.row_offset = el.col_offset = 1;
    as.row_offset = as.col_offset = 0;
    ds.row_offset = ds.col_offset = 0;
}

void
allocate_heap()
{
    int    row;

    diag("Allocating memory: elevation");

    /* 3 elevation buffers needed for precomputing aspects */

    el.buf = (CELL **) G_calloc(region.rows+el.row_offset*2+3, sizeof(CELL *));
    for (row = 0; row < 3; row++)
	el.buf[row] = ((CELL *) G_calloc(region.cols + el.col_offset * 2,
					 sizeof (CELL))) + el.row_offset;
    for (row = 3; row <= region.rows + el.row_offset; row++)
	el.buf[row] = parm.seg ? el.buf[row % 3] :
	    	      ((CELL *) G_calloc(region.cols + el.col_offset * 2,
					 sizeof (CELL))) + el.row_offset;
    el.buf += el.col_offset;

    if (parm.seg)
    {
	diag(", segment");
	el.seg = (SEGMENT *) G_malloc(sizeof (SEGMENT));
	segment_init(el.seg, el.sfd, SEGSINMEM);
	as.seg = (SEGMENT *) G_malloc(sizeof (SEGMENT));
	segment_init(as.seg, as.sfd, SEGSINMEM);
	if (parm.dsout)
	{
	    ds.seg = (SEGMENT *) G_malloc(sizeof (SEGMENT));
	    segment_init(ds.seg, ds.sfd, SEGSINMEM);
	}
    }

    if (!parm.mem)
    {
	diag(", aspect");
	as.buf = (CELL **) G_calloc(region.rows, sizeof (CELL *));
	as.buf[0] = G_allocate_cell_buf();
	for (row = 0; row < region.rows; row++)
	    as.buf[row] = parm.seg ? as.buf[0] : G_allocate_cell_buf();
    }

    if (parm.barin)
    {
	diag(", barrier");
	bitbar = BM_create(region.cols, region.rows);
    }

    if (parm.dsout)
    {
	diag(", density");
	ds.buf = (CELL **) G_calloc(region.rows, sizeof (CELL *));
	ds.buf[0] = G_allocate_cell_buf();
	for (row = 0; row < region.rows; row++)
	    ds.buf[row] = parm.seg ? ds.buf[0] : G_allocate_cell_buf();
    }

    if (parm.flout)
    {
	diag(", flowline header");
	fl.head.organization[0] = 0;
	fl.head.date[0] = 0;
	fl.head.your_name[0] = 0;
	G_strcpy(fl.head.map_name, "from raster map ");
	G_strncpy(fl.head.map_name + 16, parm.elevin, 23);
	fl.head.source_date[0] = 0;
	fl.head.orig_scale = 0;
	fl.head.line_3[0] = 0;
	fl.head.plani_zone = region.zone;
	fl.head.digit_thresh = 0;
	fl.head.map_thresh = 0;
	fl.head.W = region.west;
	fl.head.E = region.east;
	fl.head.S = region.south;
	fl.head.N = region.north;
    }

    diag(", e/w distances");
    ew_dist = (double *) G_calloc(region.rows, sizeof (double));

    diag(", quantization tolerances");
    epsilon[HORIZ] = (double *) G_calloc(region.rows, sizeof (double));
    epsilon[VERT] = (double *) G_calloc(region.rows, sizeof (double));

    diag(".\n");
}

void
deallocate_heap()
{
    int row;

    diag("De-allocating memory...");

    if (parm.barin)
	BM_destroy(bitbar);
    free(el.buf[-1] - 1);
    if (parm.seg)
    {
	segment_release(el.seg);
	if (!parm.mem)
	    segment_release(as.seg);
	if (parm.dsout)
	    segment_release(ds.seg);
    }
    else
    {
	free(el.buf[region.rows] - 1);
	for (row = 0; row < region.rows; row++)
	    free(el.buf[row] - 1);
    }
    free(--el.buf);
    if (!parm.mem)
    {
	for (row = 0; row < (parm.seg ? 1 : region.rows); row++)
	    free(as.buf[row]);
	free(as.buf);
    }
    free(ew_dist);

    diag("done.\n");
}
