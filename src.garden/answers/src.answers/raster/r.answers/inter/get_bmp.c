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

   function: get_bmp
   called by bmp_menu
   
   get the name of map to use for bmp (there are 4 possibilities
   of bmps, the "num" tells us which one) The global structure bmp_tbl
   contains the info about status of all the bmps. 
*/

#include "answers.h"

get_bmp(num)

int num;
{

    int 	i, j, check;
    char        line[200];

/* num in our array starts at zero, the option we passed started at 1.
   adust. */

    num = num - 1;

    G_clear_screen();
    fprintf (stdout,"\n    ANSWERS on GRASS BMP %s Utility \n\n",
    bmp_tbl[num].title);
    fprintf (stdout,"Use this utility to enter the name of a raster layer to be\n");
    fprintf (stdout,"used to indicate locations of %s\n", bmp_tbl[num].title);
    fprintf (stdout,"Best Managment Practices in the watershed.\n");
    fprintf (stdout,"NOTE: Since ANSWERS will recognize one BMP for a given\n");
    fprintf (stdout,"watershed element, the most effective BMP should be used.\n");
    fprintf (stdout,"\n");
    
/* a little information specific to the type of bmp chosen */

    switch(num)
    {
        case 0:
            fprintf (stdout,"ANSWERS %s Assumptions:\n\n", bmp_tbl[num].title);
            fprintf (stdout," - Trap efficeincy of 90%%\n");
            fprintf (stdout," - Only lowermost terraces are described\n");
            fprintf (stdout,"Also, if a terrace exists only in a portion of an element,\n");
            fprintf (stdout,"the assumption is made that all incoming flow is influenced by\n");
            fprintf (stdout,"the BMP. Thus, elements which have only a small portion of the\n");
            fprintf (stdout,"practice within their boundries should not be given credit for\n");
            fprintf (stdout,"the practice. \n");
            break;
        case 1:
            fprintf (stdout,"ANSWERS %s Assumptions: \n\n", bmp_tbl[num].title);
            fprintf (stdout," - Trap efficeincy of 95%%\n");
            fprintf (stdout," - Only ponds in upland areas should be defined. Instream\n");
            fprintf (stdout,"   structures are treated differently.\n");
            fprintf (stdout,"Also, if a pond exists only in a portion of an element,\n");
            fprintf (stdout,"the assumption is made that all incoming flow is influenced by\n");
            fprintf (stdout,"the BMP. Thus, elements which have only a small portion of the\n");
            fprintf (stdout,"practice within their boundries should not be given credit for\n");
            fprintf (stdout,"the practice. \n");
            break;
        case 2:
            fprintf (stdout,"ANSWERS %s Assumptions:\n\n", bmp_tbl[num].title);
            fprintf (stdout," - The vegetated area with in the affected element is no longer\n");
            fprintf (stdout,"   subject to any sediment detachment.\n");
            fprintf (stdout," - The model deliberately prohibits depostition within\n");
            fprintf (stdout,"   the vegetation of a grass waterway, since any waterway\n");
            fprintf (stdout,"    that effectively traps sediment would soon fill and\n");
            fprintf (stdout,"    become ineffective.\n");
            fprintf (stdout,"\n");
            fprintf (stdout,"For each category found in the layer, you will be prompted\n");
            fprintf (stdout,"for width of the waterway\n");
            break;
        case 3:
            fprintf (stdout,"ANSWERS %s Assumptions\n\n", bmp_tbl[num].title);
            fprintf (stdout," - The vegetated area with in the affected element is no longer\n");
            fprintf (stdout,"   subject to any sediment detachment.\n");
            fprintf (stdout,"\n");
            fprintf (stdout,"For each category found in the layer, you will be prompted\n");
            fprintf (stdout,"for width of the field border.\n");
            break;
        default:
            break;
    }

    fprintf (stdout,"\n\n\n");
    hit_return();
    G_clear_screen();
  /* check to see if read_project found any previously recorded info
     in the project file about the channel layer. If so, make sure the
     user wants it. otherwise, prompt for a new one  */
    
    check = 0;
    if ((bmp_tbl[num].set == 1) && (bmp_tbl[num].mapset[0] != 0)) {
        check = 1;
        fprintf (stdout,"\n");
        fprintf (stdout,"This utility was previously completed. You can:\n");
        fprintf (stdout,"- Use a different map layer\n");
        fprintf (stdout,"- Use a same layer.\n\n");
        sprintf(line, "Use the raster map %s in %s to identify elements with\na %s?",
        bmp_tbl[num].layer, bmp_tbl[num].mapset, bmp_tbl[num].title);
        if(!G_yes(line, 1)) 
            check = 0;
     }

    if (check == 0){
        bmp_tbl[num].set = 0;
        fprintf (stdout,"\n\nEnter the name of the %s raster map.\n", 
        bmp_tbl[num].title);
        bmp_tbl[num].mapset = G_ask_cell_old ("",bmp_tbl[num].layer);
            if(!bmp_tbl[num].mapset) {
                strcpy(bmp_tbl[num].layer, "none");
                return(0);
             }
        }
    if (mk_cat_tbl(0, bmp_tbl[num].layer, bmp_tbl[num].mapset) == -1)
    {
        bmp_tbl[num].set = 0;
        return(0);
    }
    fprintf (stdout,"\nNumber of %s categories found: %d \n",
    bmp_tbl[num].title, cat_tbl[0].cat);
    for (i = 1, j = 0; i <= cat_tbl[0].cat; i++, j++)
        {
        if (j > 9) 
            {
            j = 0;
            hit_return();
            }
        fprintf (stdout," %3ld %s\n", cat_tbl[i].cat, cat_tbl[i].label);
        }
    fprintf (stdout,"\n");

/* if grass waterway or field border, get widths of each cat */

    if ((num == 2) || (num ==3))
    {
        if (mk_bmp_tbl(num) != 0)
            return(0);
    }

    if (extract_bmp(num) == 0)
    {
        fprintf (stdout,"\n\nSuccessfully extracted ANSWERS input for %s\n\n",
        bmp_tbl[num].title);
        bmp_tbl[num].set = 1;
    }
    else
    {
        fprintf (stdout,"\n\nFailure to extract ANSWERS input for %s\n\n",
        bmp_tbl[num].title);
        bmp_tbl[num].set = 0;
        strcpy(bmp_tbl[num].layer, "none");
    }

    hit_return();
    complete[10] = 1;
    return (0);
}
