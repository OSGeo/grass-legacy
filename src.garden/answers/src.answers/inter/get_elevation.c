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

   function:  get_elevation
   called by: menu

   get the names of the elevation based layers (slope and aspect)

   */

#include "answers.h"

char *intro[] =
{
"     ANSWERS on GRASS  Slope & Aspect Layers",
"",
"ANSWERS requires slope and slope direction (aspect) information for",
"each element in the watershed. This utility keeps track of the names",
"of the raster map layers used for slope and aspect.",
"",
"Brief Specifications:",
"",
"- slope: category values should equal the predominate slope of the element",
"  multiplied by ten. (i.e. a slope of 12.4% would be category 124).",
"- aspect: category values for each element should equal the direction of  ",
"  flow in degrees.  The orientation of directions are illustrated below:",
"",
"                          90         ",
"                     135   |   45    ",
"                         \\ | /       ",
"                   180 ----+----- 0  ",
"                         / | \\       ",
"                     225   |   315   ",
"                          270        ",
"",
"",
"",
"",
"The r.fill.dir program can be used 'filter' an elevation map to",
"remove 'pits' in the watershed.",
"The r.direct program can be used to prepare an ANSWERS aspect",
"map from the elevation layer created by r.fill.dir.",
"The r.slope program can be used to prepare an ANSWERS slope",
"map from the elevation layer created by r.fill.dir.",
"",
"For further information about the data requirements of ANSWERS,",
"see the ANSWERS User Manual.",
"",
"",
0
};

get_elevation()
{
    int i;
    int ask_it = 0;
    char line[256];

    G_clear_screen();
    for (i = 0; intro[i]; i++)
    {
        if (i == 22) 
        {
            hit_return();
            G_clear_screen();
        }
	printf ("%s\n", intro[i]);
    }

    if (complete[4] > 0)
        printf("This step has been previously completed.\n\n");

    if ((slope_mapset) && (slope_layer[0] != 0))
    {
        sprintf(line, "Use the raster map %s in %s for SLOPE?",
        slope_layer, slope_mapset);
        if(G_yes(line, 1)) 
           ask_it = 1;
     }
     if (ask_it == 0)
     {
         complete[4] = 2;
         slope_mapset = G_ask_cell_old(
         "Enter the name for the SLOPE raster map\n", slope_layer);
         if (!slope_mapset) 
         {
             complete[4] = 0;
             return(0);
         }
     }

     ask_it = 0;

    if ((aspect_mapset) && (aspect_layer[0] != 0))
    {
        printf("\n\n");
        sprintf(line, "Use the raster map %s in %s for ASPECT?",
        aspect_layer, aspect_mapset);
        if(G_yes(line, 1)) 
            ask_it = 1;
     }
     if (ask_it == 0)
     {
         complete[4] = 2;
         aspect_mapset = G_ask_cell_old(
         "Enter the name for the ASPECT raster map\n", aspect_layer);
         if (!aspect_mapset) 
         {
             complete[4] = 0;
             return(0);
         }
     }
 
    if(complete[4] != 1)
        mk_elev();
    return(0);
}
