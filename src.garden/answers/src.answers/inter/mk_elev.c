/* %W% %G% */

/* 
    +---------------------------------------------------------+
    |            ANSWERS on GRASS Integration Project         |
    |  Developed in the Agriculture Engineering Department    |
    |                at Purdue University                     |
    |                        by                               |
    |           Chris Rewerts and Bernard Engel               |
    |                                                         |
    |   (c)Copyright, 1992 Purdue Research Foundation, West   |
    |   Lafayette, Indiana 47907. Permission to use, copy,    |
    |   modify, and distribute this software and its          |
    |   documentation for any purpose and without fee is      |
    |   hereby granted, provided that the above copyright     |
    |   notice appear in all copies.  This software is        |
    |   provided "as is" without express or implied warranty. |
    +---------------------------------------------------------+

   function:  mk_elev
   called by: get_elevation 
   
   creates the portion of the ANSWERS input with slope and aspect data
   for each element in watershed

   */

#include "answers.h"

mk_elev()
{
    char buf[200];
    int aspect_fd;
    int slope_fd;
    int mask_fd;
    int ct;
    FILE *data_fp;
    CELL *aspect_cell;
    CELL *slope_cell;
    CELL *mask_cell;
    int row, col;

    data_fp = G_fopen_new(data_dir, "in_elev");
    if (!data_fp)
        croak(1, "Could not create <in_elev> in project database");
     
    if ((slope_fd = G_open_cell_old (slope_layer, slope_mapset)) < 0) {
        sprintf(buf, "Could not open <%s in %s>",
        slope_layer, slope_mapset);
        croak(1, buf);
    }

    if ((aspect_fd = G_open_cell_old (aspect_layer, aspect_mapset)) < 0) {
        sprintf(buf, "Could not open <%s in %s>",
        aspect_layer, aspect_mapset);
        croak(1, buf);
    }

    if ((mask_fd = G_open_cell_old (mask_layer, mask_mapset)) < 0) {
        sprintf(buf, "Could not open <%s in %s>",
        mask_layer, mask_mapset);
        croak(1, buf);
    }

    mask_cell = G_allocate_cell_buf();
    aspect_cell = G_allocate_cell_buf();
    slope_cell = G_allocate_cell_buf();
    
    ct = 1;
    fprintf (stderr, "\n\n\npercent complete: ");
    for (row = 0; row < window.rows; row++)
    {
	percent (row, window.rows, 5);
	G_get_map_row (aspect_fd, aspect_cell, row);
	G_get_map_row (slope_fd, slope_cell, row);
	G_get_map_row (mask_fd, mask_cell, row);

        for (col = 0; col < window.cols; col++)
	{
            if(mask_cell[col] > 0)
            {
                if(slope_cell[col] < 1)
                {
                    fprintf(data_fp,"   1%4ld\n",
                    (long)aspect_cell[col]);
                }
                else
                {
                    fprintf(data_fp," %3ld%4ld\n",
                    slope_cell[col], (long)aspect_cell[col]);
                }
                if (aspect_cell[col] == 0)
                {
                    printf("\n\nwatershed element %d, row %d col %d\n",
                    ct, row, col);
                    croak(1, "Illegal aspect value of 0 (zero)");
                }
            ct++;
            }
	}
    }
    percent (row, window.rows, 5);

    G_close_cell (slope_fd);
    G_close_cell (mask_fd);
    G_close_cell (aspect_fd);
    fclose(data_fp);

    hit_return(); 
    complete[4] = 1;
    return(0);
}
