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

   function: get_cover
   called by menu
   
   confirms a cell layer as a source for cover data categories
   in watershed.

*/
#include "answers.h"

get_cover()
{
    int 	i, j, check;
    char        line[200];

    G_clear_screen();
    printf("\n\n       ANSWERS on GRASS Land Cover Catalogue Utility \n\n");
    printf("To prepare cropping/land cover data for use with ANSWERS, the \n");
    printf("following is needed: a raster map layer of the land cover in the \n");
    printf("watershed and a description of each category found.\n\n");

  /* check to see if read_project found any previously recorded info
     in the project file about the layer. If so, make sure the
     user wants it. otherwise, prompt for a new one  */
    
    check = 0;
    if ((complete[3]) && (cover_mapset) && (cover_layer[0] != 0)) {
        check = 1;
        printf("This utility was previously completed. You can:\n\n");
        printf("- Use a different layer for cover (this erases previously set parameters)\n");
        printf("- Use a same layer for cover and edit previously set parameters.\n\n");
        sprintf(line, "Use the raster map %s in %s for LAND COVER?",
                       cover_layer, cover_mapset);
        if(!G_yes(line, 1)) 
            check = 0;
     }

    if (check == 0){
        complete[3] = 0;
        printf("\n\nEnter the name of the LAND COVER raster map.\n");
        cover_mapset = G_ask_cell_old ("",cover_layer);
            if(!cover_mapset) {
                return(0);
             }
        }
    printf("\n");
    if (mk_cat_tbl(1, cover_layer, cover_mapset) == -1)
    {
        complete[3] = 0;
        return(0);
    }
 
    printf("\nNumber of land cover/cropping categories found: %d \n", cat_tbl[0].cat);
    printf("-----------------------------------------------\n");
    for (i = 1, j = 0; i <= cat_tbl[0].cat; i++, j++)
        {
        if (j > 19) 
            {
            j = 0;
            hit_return();
            }
        printf(" %3ld %s\n", cat_tbl[i].cat, cat_tbl[i].label);
        }
    printf("\n");
    if (cat_tbl[0].cat > 21) {
        printf("\n\n\nMore than 20 categories were found.\n");
        croak(1, 
        "The ANSWERS program can be reconfigured or input layer can be reclassed");
    }
    else
        hit_return();
    mk_cover_tbl();
    return (0);
}
