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
    printf("\n    ANSWERS on GRASS BMP %s Utility \n\n",
    bmp_tbl[num].title);
    printf("Use this utility to enter the name of a raster layer to be\n");
    printf("used to indicate locations of %s\n", bmp_tbl[num].title);
    printf("Best Managment Practices in the watershed.\n");
    printf("NOTE: Since ANSWERS will recognize one BMP for a given\n");
    printf("watershed element, the most effective BMP should be used.\n");
    printf("\n");
    
/* a little information specific to the type of bmp chosen */

    switch(num)
    {
        case 0:
            printf("ANSWERS %s Assumptions:\n\n", bmp_tbl[num].title);
            printf(" - Trap efficeincy of 90%%\n");
            printf(" - Only lowermost terraces are described\n");
            printf("Also, if a terrace exists only in a portion of an element,\n");
            printf("the assumption is made that all incoming flow is influenced by\n");
            printf("the BMP. Thus, elements which have only a small portion of the\n");
            printf("practice within their boundries should not be given credit for\n");
            printf("the practice. \n");
            break;
        case 1:
            printf("ANSWERS %s Assumptions: \n\n", bmp_tbl[num].title);
            printf(" - Trap efficeincy of 95%%\n");
            printf(" - Only ponds in upland areas should be defined. Instream\n");
            printf("   structures are treated differently.\n");
            printf("Also, if a pond exists only in a portion of an element,\n");
            printf("the assumption is made that all incoming flow is influenced by\n");
            printf("the BMP. Thus, elements which have only a small portion of the\n");
            printf("practice within their boundries should not be given credit for\n");
            printf("the practice. \n");
            break;
        case 2:
            printf("ANSWERS %s Assumptions:\n\n", bmp_tbl[num].title);
            printf(" - The vegetated area with in the affected element is no longer\n");
            printf("   subject to any sediment detachment.\n");
            printf(" - The model deliberately prohibits depostition within\n");
            printf("   the vegetation of a grass waterway, since any waterway\n");
            printf("    that effectively traps sediment would soon fill and\n");
            printf("    become ineffective.\n");
            printf("\n");
            printf("For each category found in the layer, you will be prompted\n");
            printf("for width of the waterway\n");
            break;
        case 3:
            printf("ANSWERS %s Assumptions\n\n", bmp_tbl[num].title);
            printf(" - The vegetated area with in the affected element is no longer\n");
            printf("   subject to any sediment detachment.\n");
            printf("\n");
            printf("For each category found in the layer, you will be prompted\n");
            printf("for width of the field border.\n");
            break;
        default:
            break;
    }

    printf("\n\n\n");
    hit_return();
    G_clear_screen();
  /* check to see if read_project found any previously recorded info
     in the project file about the channel layer. If so, make sure the
     user wants it. otherwise, prompt for a new one  */
    
    check = 0;
    if ((bmp_tbl[num].set == 1) && (bmp_tbl[num].mapset[0] != 0)) {
        check = 1;
        printf("\n");
        printf("This utility was previously completed. You can:\n");
        printf("- Use a different map layer\n");
        printf("- Use a same layer.\n\n");
        sprintf(line, "Use the raster map %s in %s to identify elements with\na %s?",
        bmp_tbl[num].layer, bmp_tbl[num].mapset, bmp_tbl[num].title);
        if(!G_yes(line, 1)) 
            check = 0;
     }

    if (check == 0){
        bmp_tbl[num].set = 0;
        printf("\n\nEnter the name of the %s raster map.\n", 
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
    printf("\nNumber of %s categories found: %d \n",
    bmp_tbl[num].title, cat_tbl[0].cat);
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

/* if grass waterway or field border, get widths of each cat */

    if ((num == 2) || (num ==3))
    {
        if (mk_bmp_tbl(num) != 0)
            return(0);
    }

    if (extract_bmp(num) == 0)
    {
        printf("\n\nSuccessfully extracted ANSWERS input for %s\n\n",
        bmp_tbl[num].title);
        bmp_tbl[num].set = 1;
    }
    else
    {
        printf("\n\nFailure to extract ANSWERS input for %s\n\n",
        bmp_tbl[num].title);
        bmp_tbl[num].set = 0;
        strcpy(bmp_tbl[num].layer, "none");
    }

    hit_return();
    complete[10] = 1;
    return (0);
}
