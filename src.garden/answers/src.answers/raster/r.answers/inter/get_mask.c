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

   function: get_mask
   called by step_1
   
   confirms a cell layer for use as the MASK for subsequent work.
   
 */

#include "answers.h"

get_mask()
{
    int i;

    G_clear_screen();
    fprintf (stderr,"\n      ANSWERS on GRASS Mask, Resolution, and Region Utility \n");
    fprintf (stderr,"      Part 1: MASK \n\n");
    if (complete[1] == 0) {
        fprintf (stderr,"A raster map is needed to identify the study watershed area\n");
        fprintf (stderr,"\"mask\" throughout the preparation of data and subsequent \n");
        fprintf (stderr,"running of the ANSWERS simulation.\n\n");
        
        fprintf (stderr,"This program will read the provided map. *All* raster values \n");
        fprintf (stderr,"greater than zero will be considered part of the project mask.\n");
        fprintf (stderr,"(Any currently set mask and region will be ignored).\n\n");
        fprintf (stderr,"* IMPORTANT: It is imperative that the map you wish to use as *\n");
        fprintf (stderr,"* a mask remain unchanged during the course of the project,   *\n");
        fprintf (stderr,"* since it will be used as a reference by all parts of the    *\n");
        fprintf (stderr,"* the ANSWERS on GRASS Project Manager.                       *\n");
        fprintf (stderr,"\n");
        }
    else {
        fprintf (stderr,"Currently defined MASK layer is <%s> in <%s>\n\n",
        mask_layer, mask_mapset);
        fprintf (stderr,"If a the mask layer is re-defined at this time, all \n");
        fprintf (stderr,"previously completed steps must be run again to\n");
        fprintf (stderr,"resample ANSWERS input data.\n\n");
        if(!G_yes("Do you wish to re-define the mask at this time?",0))
            return(0);
        fprintf (stderr,"\n");
        complete[1] = 2;
        }
    fprintf (stderr,"\nIdentify the raster layer to be used for the project MASK.\n");

    mask_mapset = G_ask_cell_old ("",mask_layer);
        if (!strcmp(mask_layer, "")) 
        {
            read_project();
            return(1);
        }
        if(!mask_mapset) {
            fprintf(stderr, "\n\7WARNING: Could not access <%s> layer.\n", mask_layer);
            hit_return();
            return(1);
            }
  /* since the other steps make use of the mask layer when doing their
     thing, then when a new mask is set, we will mark all other stages
     as changed (2) if they were not marked as incomplete before */

    for( i = 2; i <= 11; i++)
        if (complete[i] > 0)
            complete[i] = 2;
        else
            complete[i] = 0;

    return(0);
}
