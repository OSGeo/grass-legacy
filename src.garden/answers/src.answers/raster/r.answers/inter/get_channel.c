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

   function: get_chnl
   called by menu
   
   confirms a cell layer as a source for channel data categories
   in watershed.
*/

#include "answers.h"

get_chnl()
{
    int 	i, j, check;
    char        line[200];

    G_clear_screen();
    printf("\n\n          ANSWERS on GRASS Channel Catalogue Utility \n\n");
    printf("Watershed cells with a well-defined channel should be defined\n");
    printf("to ANSWERS. ANSWERS assumes the channel is rectangular in \n");
    printf("cross-section and is sufficiently deep to handle runoff.\n");
    printf("\n");
    printf("To prepare channel data for use with ANSWERS, the followng is\n");
    printf("needed: a raster map layer of the channels in the watershed\n");
    printf("and a description of width and roughness for each channel\n");
    printf("category found in the layer.\n\n");

  /* check to see if read_project found any previously recorded info
     in the project file about the channel layer. If so, make sure the
     user wants it. otherwise, prompt for a new one  */
    
    check = 0;
    if ((complete[8]) && (chnl_mapset) && (chnl_layer[0] != 0)) {
        check = 1;
        printf("This utility was previously completed. You can:\n\n");
        printf("- Use a different map layer (this erases previously set parameters)\n");
        printf("- Use a same layer and edit previously set parameters.\n\n");
        sprintf(line, "Use the raster map %s in %s for CHANNELS?",
                       chnl_layer, chnl_mapset);
        if(!G_yes(line, 1)) 
            check = 0;
     }

    if (check == 0){
        complete[8] = 0;
        printf("\n\nEnter the name of the CHANNELS raster map.\n");
        chnl_mapset = G_ask_cell_old ("",chnl_layer);
            if(!chnl_mapset) {
                return(0);
             }
        }
    printf("\n");
    if (mk_cat_tbl(0, chnl_layer, chnl_mapset) == -1)
    {
        complete[8] = 0;
        return(0);
    }
    printf("\nNumber of channel categories found: %d \n", cat_tbl[0].cat);
    for (i = 1, j = 0; i <= cat_tbl[0].cat; i++, j++)
        {
        if (j > 9) 
            {
            j = 0;
            hit_return();
            }
        printf(" %3ld %s\n", cat_tbl[i].cat, cat_tbl[i].label);
        }
    printf("\n");
    hit_return();
    mk_chnl_tbl();
    return (0);
}
