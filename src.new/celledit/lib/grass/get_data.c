/***********************************************************************

File     	:	get_data.c
Function 	:	GetDataVals(name, mapset, dataVals)
Args	 	:	    char *name; -- the name of the layer
  	    		    char *mapste; -- which mapset. 
  	    		    CELL *dataVal; -- the list of data values. 

Author   	:	Frank Goodman -- spanki@ced.berkeley.edu
Creation 	:	29 March 1990
Last Revised	:
Abstract 	:	This is a hack on the Gdescribe routines to
			get me a list of ALL the data values in a
			cell_file.
Returns  	:	1 if successful 0 if not;.

***********************************************************************/
#include <stdio.h>
#include "/home/grass3/src/libes/gis.h"

GetDataVals(name, mapset, dataVals, numData)
    char *name;
    char *mapset;
    CELL **dataVals;
    int  *numData;
    {
    int fd;
    struct Cell_stats statf;
    CELL *buf;
    CELL *tmp;
    CELL cat;
    struct Cell_head newWindow;
    struct Cell_head oldWindow;
    int nrows, ncols;
    register int row;
    register int i;
    int G_get_map_row_nomask();
    int count;
    char msg[100];


    G_get_set_window(&oldWindow);
    if (G_get_cellhd (name, mapset, &newWindow) < 0)
	{
	sprintf (msg, "can't get cell header for [%s] in [%s]",name,mapset);
	G_fatal_error (msg);
	}
    G_set_window (&newWindow);

    fd = G_open_cell_old (name, mapset);
    if (fd < 0) return(0);

    /* allocate the cell buffer */
    buf = G_allocate_cell_buf();

    /* start the cell stats */
    G_init_cell_stats (&statf);

    nrows = G_window_rows();
    ncols = G_window_cols();

    for (row = 0 ; row < nrows; row++)
	{
	if(G_get_map_row_nomask (fd, buf, row) < 0)
	    break;
	G_update_cell_stats (buf, ncols, &statf);
	}

    G_close_cell (fd);
    free (buf);

    G_rewind_cell_stats (&statf);
    /* now put them data values into a list */

    i = 0;
    tmp = (CELL *)G_calloc(1, (unsigned)sizeof(CELL));
    G_next_cell_stat (&cat, &count, &statf);
    tmp[i++] = (CELL)cat;
    while (G_next_cell_stat (&cat, &count, &statf))
	{
	tmp = (CELL *)G_realloc((char *)tmp, (unsigned)((i+1)*sizeof(CELL)));
	tmp[i] = (CELL)cat;
	i++;
	}

    /* give the args their new values */
    *numData = i;
    *dataVals = (CELL *)tmp;
    G_free_cell_stats (&statf);
    /* put back the old window */
    G_set_window (&oldWindow);
    return(1);
    }
