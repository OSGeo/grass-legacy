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

   function: mk_cat_tbl
   called by: get_bmp, get_chnl, get_chnl_slp, get_cover, get_rain, get_soils,
              get_tile

    read a layer and fill the global structure array "cat_tbl"
    if "zero" flag = 1 we will complain if a zero cell value is
    found in the watershed.
    
    returns -1 if something didn't go right.
    
    */

#include "answers.h"
mk_cat_tbl(zero, layer, mapset)
    int zero;
    char *layer, *mapset;
{
    int 	i, j;
    CELL        *mask_cell, *cell, c;
    int         row;
    int         col;
    long	count;
    int		fd;
    int		mask_fd;
    long	*get_cats();
    char	*G_get_cat();
    struct Categories cats;
    struct Cell_stats statf;

G_clear_screen();     
printf("\n<reading categories in project MASK and REGION>\n");

  /* get the category numbers */
    if (G_read_cats (layer, mapset, &cats) < 0) {
        fprintf(stderr, "\n\7WARNING: Could not read cats for <%s> layer", layer);
        hit_return();
        return(-1);
    }

  /* open the cell file */

    fd = G_open_cell_old (layer, mapset);
    if (fd < 0) {
        fprintf(stderr, "\n\7WARNING: Could not open <%s>\n", layer);
        hit_return();
	return(-1);
    }

    if (zero == 1)
    {
        mask_fd = G_open_cell_old (mask_layer, mask_mapset);
        if (mask_fd < 0) {
            fprintf(stderr, "\n\7WARNING: Could not open <%s>\n", mask_layer);
            hit_return();
	    return(-1);
        }
        mask_cell  = G_allocate_cell_buf ();
    }

    cell  = G_allocate_cell_buf ();

/* initialize the stats */
    G_init_cell_stats (&statf);

/* read the cell file and gather the stats */

    for (row = 0; row < window.rows; row++)
    {
	if (G_get_map_row (fd, cell, row) < 0)  {
            fprintf(stderr, "\n\7WARNING: Failure to read row %d in <%s>\n", 
            row, layer);
            hit_return();
            return(-1);
        }
        if (zero == 1)
        {
            if (G_get_map_row (mask_fd, mask_cell, row) < 0)  {
                fprintf(stderr, "\n\7WARNING: Failure to read row %d in <%s>\n", 
                row, mask_layer);
                hit_return();
                return(-1);
            }
        }
        if (zero == 1)
        {
            for (col = 0; col < window.cols; col++)
            {
        
            if ((cell[col] < 1) && (mask_cell[col] > 0))
            {
                fprintf(stderr, 
                "\n\7WARNING: Illegal raster value less than 1 found\n");
                hit_return();
                return(-1);
            }
        }
        }

	G_update_cell_stats (cell, window.cols, &statf);
    }
    G_close_cell (fd);
    if (zero == 1)
        G_close_cell (mask_fd);
    G_rewind_cell_stats (&statf);
    
/* init/clear table array structure */
    for (i = 0; i < 100; i++)
        {
        cat_tbl[i].cat = 0;
        for(j = 0; j < 9; j++)
            cat_tbl[i].param[j] = 0;
        }
        
/* make a input table of the cats found in current mask and/or window */

    i = 1;

/* the first stat is zero (no data) so we call this once to read past that */

    G_next_cell_stat (&c, &count, &statf);

    while (G_next_cell_stat (&c, &count, &statf))
    {
        cat_tbl[i].cat =  (long)c;
        cat_tbl[i].label = G_get_cat(c, &cats);  
        i++;
        if (i == 100)
            {
            fprintf(stderr, 
            "\n\7WARNING: more than 100 categories found. Not configured for\n");
            fprintf(stderr, "that many. Short of reprogramming, you could rescale\n");
            fprintf(stderr, "or reclass the input map.\n");
            hit_return();
            return(-1);
            }
    }
     G_free_cell_stats(&statf);

/* so how many cats in the hat are there? set the first value in the 
   table array to record. */

    cat_tbl[0].cat = i - 1;

    if (cat_tbl[0].cat == 0) {
        fprintf(stderr, "\n\7WARNING: No categories found.\n\n");
        hit_return();
        return(-1);
    }
    return(0);
}
