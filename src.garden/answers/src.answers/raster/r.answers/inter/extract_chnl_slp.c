/* %G% %W% */
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

   function: extract_chnl_slp
   called by:  get_chnl_slp

   this function is a modified version of extract... factors were
   different enought for the tile input that it was easier to
   write a separate function to take care of this

   function returns 1 if unhappy; 0 if it missed the error (happy)
   */

#include "answers.h"

extract_chnl_slp ()

{
    int map_fd;
    int chnl_map_fd;
    int mask_fd;
    int row, col;
    char line[100];
    FILE *data_fp;
    CELL *map_cell;
    CELL *chnl_map_cell;
    CELL *mask_cell;

    printf("\n\nExtracting data from <%s in %s>\n\n",
    chnl_slp_layer, chnl_slp_mapset);

/* open input layer */
    map_fd = G_open_cell_old(chnl_slp_layer, chnl_slp_mapset);
    if (  map_fd < 0 )
    {
        sprintf(line, "Could not open <%s in %s>",
        chnl_slp_layer, chnl_slp_mapset);
        croak(0, line);
        return(1);
    }
/* open channel layer (we will only look at the channel slope layer
   if there is indeed a channel segment in an element) */

    chnl_map_fd = G_open_cell_old(chnl_layer, chnl_mapset);
    if (  chnl_map_fd < 0 )
    {
        sprintf(line, "Could not open <%s in %s>", chnl_layer, chnl_mapset);
        croak(0, line);
        return(1);
    }

/* open project mask */
    mask_fd = G_open_cell_old(mask_layer, mask_mapset);
    if (  mask_fd < 0 )
    {
        sprintf(line, "Could not open <%s in %s>", mask_layer, mask_mapset);
        croak(0, line);
        return(1);
    }

/* open file for answers input (the results of this extraction process)*/
    data_fp = G_fopen_new(data_dir, "in_chnl_slp");
    if ( !data_fp)
    {
        sprintf(line, "Could not create <in_chnl_slp> in project database");
        croak(0, line);
        return(1);
    }

    map_cell = G_allocate_cell_buf();
    chnl_map_cell = G_allocate_cell_buf();
    mask_cell = G_allocate_cell_buf();

    fprintf(stderr, "Percent complete: ");

    for( row = 0; row < window.rows; row++)
    {
        G_get_map_row(map_fd, map_cell, row);
        G_get_map_row(chnl_map_fd, chnl_map_cell, row);
        G_get_map_row(mask_fd, mask_cell, row);

        percent( row, window.rows, 5);

        for (col = 0; col < window.cols; col++)
        {
/* only look at cells in mask */
            if(mask_cell[col] > 0)   
            {
/* if this is a channel and we have a value for slope, print it */
                if ((chnl_map_cell[col] > 0) && (map_cell[col] > 0))
                    fprintf(data_fp, "%3d\n", map_cell[col]);
                else
                    fprintf(data_fp, "  0\n");
            }   /* loop of mask */
        }       /* loop of cols */
    }           /* loop of rows */
    percent( row, window.rows, 5);
   
    G_close_cell(map_fd);
    G_close_cell(chnl_map_fd);
    G_close_cell(mask_fd);
    fclose(data_fp);
    return(0);
}
