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

   function: get_soils
   called by menu
   
   confirms a cell layer as a source for soil data categories
   in watershed.
*/

#include "answers.h"

get_soils()
{
    int 	i, j, check;
    char        line[200];

    G_clear_screen();
    printf("\n\n          ANSWERS on GRASS Soil Catalogue Utility \n\n");
    printf("To prepare soils data for use with ANSWERS, the followng is\n");
    printf("needed: a raster map layer of the soil series in the watershed\n");
    printf("and a description of each soil series found in the layer.\n\n");

  /* check to see if read_project found any previously recorded info
     in the project file about the soils layer. If so, make sure the
     user wants it. otherwise, prompt for a new one  */
    
    check = 0;
    if ((complete[2] > 0) && (soil_mapset) && (soil_layer[0] != 0)) {
        check = 1;
        printf("This utility was previously completed. You can:\n\n");
        printf("- Use a different layer for soils \n");
        printf("  (this erases previously set parameters)\n");
        printf("- Use a same layer for soils and edit previously set \n");
        printf("  parameters.\n\n");
        sprintf(line, "Use the raster map %s in %s for SOILS?",
                       soil_layer, soil_mapset);
        if(!G_yes(line, 1)) 
            check = 0;
     }

    if (check == 0){
        complete[2] = 0;
        printf("\n\nEnter the name of the SOIL SERIES raster map.\n");
        soil_mapset = G_ask_cell_old ("",soil_layer);
            if(!soil_mapset) {
                return(0);
             }
        }
    printf("\n");
    if (mk_cat_tbl(1, soil_layer, soil_mapset) == -1)
    {
        complete[2] = 0;
        return(0);
    }
    printf("\nNumber of soil categories found: %d \n", cat_tbl[0].cat);
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
    if (cat_tbl[0].cat > 21) {
        printf("\n\n\nmore than 20 categories were found.\n");
        croak(1, "The ANSWERS could be reconfigured or the input layer reclassed.");
    }
    else
        hit_return();
    mk_soil_tbl();
    return (0);
}
