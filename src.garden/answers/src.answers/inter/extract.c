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

   function: extract
   called by:  mk_chnl_tbl, mk_soil_tbl, mk_cover_tbl

   by the time this function is called, the user has selected a
   layer as input for channel, soil, or cover data. In each of
   these cases answers will need a section in the predata file
   to describe each input type. The user will fill out a table
   of parameters to satisfy this. For each input class found,
   answers will get it labeled by a sequential ints starting at
   1. Meanwhile, we will keep track of the class number found in
   the GRASS input label, thanks to the global cat_tbl array.

   this function opens the given input array and the project
   mask layer. we sequentially look at each element's value
   in the mask. then we lookup the value that answers associates
   with it, and print that to the input file in the project
   database

   function returns 1 if unhappy; 0 if it missed the error (happy)
   */

#include "answers.h"

extract(zero, layer, mapset, datafile)

    int zero;         /* if zero=0, it's ok if they are in the data */
    char *layer,      /* layer with data to be extracted via lookup */
    *mapset,
    datafile[10];     /* file in project database for extracted data*/

{
    int map_fd;
    int mask_fd;
    int ct;
    int i;
    int row, col;
    char line[100];
    FILE *data_fp;
    CELL *map_cell;
    CELL *mask_cell;

    printf("\n\nExtracting data from <%s in %s>\n\n", layer, mapset);

/* open input layer */
    map_fd = G_open_cell_old(layer, mapset);
    if (  map_fd < 0 )
    {
        fprintf(stderr, "\n\7WARNING: Could not open <%s in %s>\n", layer, mapset);
        hit_return();
        return(1);
    }

/* open project mask */
    mask_fd = G_open_cell_old(mask_layer, mask_mapset);
    if (  mask_fd < 0 )
    {
        fprintf(stderr, "\n\7WARNING: Could not open <%s in %s>\n", 
        mask_layer, mask_mapset);
        hit_return();
        return(1);
    }

/* open file for answers input (the results of this extraction process)*/
    data_fp = G_fopen_new(data_dir, datafile);
    if ( !data_fp)
    {
        fprintf(stderr, "\n\7WARNING: Could not create <%s> in project database\n", 
        datafile);
        hit_return();
        return(1);
    }

    map_cell = G_allocate_cell_buf();
    mask_cell = G_allocate_cell_buf();

    fprintf(stderr, "Percent complete: ");
    ct = 1;

    for( row = 0; row < window.rows; row++)
    {
        G_get_map_row(map_fd, map_cell, row);
        G_get_map_row(mask_fd, mask_cell, row);

        percent( row, window.rows, 5);

        for (col = 0; col < window.cols; col++)
        {
/* only look at cells in mask */
            if(mask_cell[col] > 0)   
            {
/* in some cases finding a cell value of zero is ok */
                if (map_cell[col] == 0)
                {
                    if(zero)
                    {
                        sprintf(line,
                        "Value of 0 (zero) found in <%s>, for watershed element %d\n",
                        layer, ct);
                        croak(1, line);
                    }
                    else
                        fprintf(data_fp, " 0\n");
                }
                else
/* do lookup of value */
                {
                    i = 1;
                    while(1)
                    {
/* don't loop through more than number of cats in input layer */
                        if(i > cat_tbl[0].cat)
                        {
                            sprintf(line, "Can't extract value for %d",
                            map_cell[col]);
                            strcat(line, "using \"??\"");
                            fprintf(data_fp, "??\n");
                            croak(0, line);
                            break;
                        }
/* found match. print to file. break out of lookup loop now */
                        if (cat_tbl[i].cat == map_cell[col])
                        {
                            fprintf(data_fp, "%2d\n", i);
                            break;
                        }
/* continue lookup loop */
                        i++;
                    }
                }
/* increment count of cells for reference in error message */
                ct++;
            }   /* loop of mask */
        }       /* loop of cols */
    }           /* loop of rows */
    percent( row, window.rows, 5);
   
    G_close_cell(mask_fd);
    G_close_cell(map_fd);
    fclose(data_fp);
    return(0);
}
