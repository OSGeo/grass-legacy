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

   function: get_tile
   called by: menu 

   find out if there is going to be subsurface drainage identified in the 
   watershed. may have all, none, or a layer showing which cells.
   
   */
#include "answers.h"

get_tile()
{

    int option;
    char buf[80];

while(1)
{
option = 0;
V_clear();
V_line(2,"           ANSWERS on GRASS Subsurface Drainage ");
V_line(4,"The location of subsurface tile drainage for the watershed");
V_line(5,"is delineated with this utility. ");
V_line(6,"Note: the drainage coefficient for areas with subsurface");
V_line(7,"is set with the other soils parameters in step 2.");
V_line(11,"   To identify areas with subsurface drainage, choose one of the ");
V_line(12,"   following options:");
V_line(14,"            1. There is no subsurface tile drainage in watershed");
V_line(15,"            2. All of the watershed area has subsurface drainage");
V_line(16,"            3. Use a raster map to indicate areas of subsurface drainage");
V_line(18,"   Option:");
V_ques(&option, 'i', 18, 12, 2);
V_intrpt_msg("return to Main Menu");
V_intrpt_ok();

if (! V_call())
    return(0);

    switch(option)
    {
    case 1:
        strcpy(tile_area, "none");
        break;
    case 2:
        strcpy(tile_area, "all");
        break;
    case 3:
        strcpy(tile_area, "layer");
        break;
    default:
        fprintf(stderr, "\nPlease choose option 1, 2, or 3\n");
        sleep(3);
        break;
    }
if((option > 0) && (option < 4))
    break;
}

    if(option == 3)
    {
        printf("\n\nEnter the name of SUBSURFACE TILE DRAINAGE raster map.\n");
        tile_mapset = G_ask_cell_old ("", tile_layer);
        if(!tile_mapset) 
                return(0);
        printf("\n");
        if (mk_cat_tbl(0, tile_layer, tile_mapset) == -1)
        {
            complete[7] = 0;
            return(0);
        }
        if(cat_tbl[0].cat != 1)
        {
            sprintf(buf, "expecting only 1 category. found %d", cat_tbl[0].cat);
            croak(1, buf);
        }
    }
    else
    {
        printf("\n\nSetting parameter:\n");
        printf("%s of the watershed area has subsurface tile drainage\n",
        tile_area);
        printf("\n");
    }


    if(extract_tile (option))
    {
        complete[7] = 0;
        croak(1, "Failure to extract ANSWERS input data");
    }
    else
    {
        printf("\n\nANSWERS subsurface drainage input data preparation complete.\n\n");
        hit_return();
        complete[7] = 1;
    }
    return(0);
}
