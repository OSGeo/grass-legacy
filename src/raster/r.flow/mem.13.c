/*
**  Original Algorithm:    H. Mitasova, L. Mitas, J. Hofierka, M. Zlocha 
**  GRASS Implementation:  J. Caplan, M. Ruesink  1995
**
**  US Army Construction Engineering Research Lab, University of Illinois 
**
**  Copyright  M. Ruesink, J. Caplan, H. Mitasova, L. Mitas, J. Hofierka, 
**	M. Zlocha  1995
**
**This program is free software; you can redistribute it and/or
**modify it under the terms of the GNU General Public License
**as published by the Free Software Foundation; either version 2
**of the License, or (at your option) any later version.
**
**This program is distributed in the hope that it will be useful,
**but WITHOUT ANY WARRANTY; without even the implied warranty of
**MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
**GNU General Public License for more details.
**
**You should have received a copy of the GNU General Public License
**along with this program; if not, write to the Free Software
**Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
**
*/


#include "r.flow.13.h"
#include "io.13.h"
#include "mem.13.h"

/************************** MEMORY MGMT/ACCESS **************************/

void put_row_seg(
    layer l,
    int   row)
{
    if (segment_put_row(l.seg, l.buf[row] - l.col_offset, 
			row + l.row_offset) < 1)
    {
	sprintf(string, "r.flow: cannot write segment file for %s", l.name);
	G_fatal_error(string);
    }
}

void initialize_globals(
    int		 argc,
    char	*argv[])
{
    G_gisinit(argv[0]);

    if (G_get_set_window(&region) == -1)
	G_fatal_error("r.flow: error getting current region");

    diag("r.flow Version 13 August 1995, update/fix October 1999\n\n");

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

void allocate_heap(void)
{
    int    row;

    diag("Allocating memory: elevation");

    /* 3 elevation buffers needed for precomputing aspects */

    el.buf = (DCELL **) G_calloc(region.rows+el.row_offset*2+3, sizeof(DCELL *));
    for (row = 0; row < 3; row++)
	el.buf[row] = ((DCELL *) G_calloc(region.cols + el.col_offset * 2,
					 sizeof (DCELL))) + el.row_offset;
    for (row = 3; row <= region.rows + el.row_offset; row++)
	el.buf[row] = parm.seg ? el.buf[row % 3] :
	    	      ((DCELL *) G_calloc(region.cols + el.col_offset * 2,
					 sizeof (DCELL))) + el.row_offset;
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
	as.buf = (DCELL **) G_calloc(region.rows, sizeof (DCELL *));
/*	as.buf[0] = G_allocate_cell_buf(); replaced by Helena Oct.99 by DCELL*/
	as.buf[0] = (DCELL *) G_allocate_raster_buf(DCELL_TYPE);
	for (row = 0; row < region.rows; row++)
	    as.buf[row] = parm.seg ? 
		as.buf[0] : (DCELL *) G_allocate_raster_buf(DCELL_TYPE);
    }

    if (parm.barin)
    {
	diag(", barrier");
	bitbar = BM_create(region.cols, region.rows);
    }

    if (parm.dsout)
    {
	diag(", density");
	ds.buf = (DCELL **) G_calloc(region.rows, sizeof (DCELL *));
	ds.buf[0] = (DCELL *) G_allocate_raster_buf(DCELL_TYPE);
	for (row = 0; row < region.rows; row++)
	    ds.buf[row] = parm.seg ? 
		ds.buf[0] : (DCELL *) G_allocate_raster_buf(DCELL_TYPE);
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

    return;
}

void deallocate_heap(void)
{
    int row;

    diag("De-allocating memory...");

    if (parm.barin)
	BM_destroy(bitbar);
    G_free(el.buf[-1] - 1);
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
	G_free(el.buf[region.rows] - 1);
	for (row = 0; row < region.rows; row++)
	    G_free(el.buf[row] - 1);
    }
    G_free(--el.buf);
    if (!parm.mem)
    {
	for (row = 0; row < (parm.seg ? 1 : region.rows); row++)
	    G_free(as.buf[row]);
	G_free(as.buf);
    }
    G_free(ew_dist);

    diag("done.\n");
}
