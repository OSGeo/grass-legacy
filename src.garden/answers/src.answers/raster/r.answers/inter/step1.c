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

   function: step_1
   called by: main or menu 

   this function handles the various parts of "step one"... the 
   identification of mask layer, resolution, and dealing with the region.
   
   since this funtion sort of evolved over time,  everything is not a
   coder's dream. it communicated with the rest of the world by setting
   the complete[1] flag to either 1 for complete or 2 to indicate to 
   other steps that the status had been 1, then something was altered.
   if resoltion or mask has been changed, then any step completed before
   has to be re-ran, since any answers input it had created before must
   be extracted again.
   */
#include "answers.h"

step_1()
{
    int r;
    int done = 0;
    
    G_clear_screen();
    printf("\n\n        ANSWERS on GRASS Project Manager\n\n");
    printf("    Step 1: Set mask, region, and resolution\n\n");
    printf("    The first priority of the project is to\n");
    printf("    1. Establish the area of the watershed (mask)\n");
    printf("    2. Determine the region in the mapset for study, and\n");
    printf("    3. Define the resolution (raster map cell size) \n");
    printf("       to use for the simulation.\n");
    printf("    \n");
    printf("    This step will require a raster map to use as a\n");
    printf("    watershed mask.\n");
    printf("    \n");
    for (r = 2; r < 11; r++)
    {
        if (complete[r] == 1)
            done = 1;
    }
    if (done == 1)
    {
        printf("    This step has already been completed. If mask,\n");
        printf("    region, or resolution is changed, all previously\n");
        printf("    completed steps will have to be run again to resample\n");
        printf("    ANSWERS input data.\n");
    }
    printf("    \n");

    if (!G_yes("\n    Do you wish to continue with this step? ", 1))
        return(0);
        
/* call each step in turn, if they return a 1 we have had a problem,
   so we tell the calling function */

    while(1)
    {
        if (get_mask() == 1) 
            return(1);
        if (get_res() == 1) 
            return(1);
        if (trim_region() == 1) 
            return(1);
        if (complete[1] != 1)
            r = edit_region();
        if (r == 1) 
            return(1);
        else if(r == 2)
        {
        /* user wants to start over. loop again */
            complete[1] = 2;
            alter_status();
            continue;
        }
        else
            break;
    }
        if (set_mask() == 1) return(1);
        else
        {
            complete[1] = 1;
            printf("\n\n\n\n");
            hit_return();
            return(0);
        }
}

/* routine to set status of completed steps to 2 to indicate
   that resolution or mask has been changed */

alter_status()
{
    int i;

    for(i = 2; i <= 11; i++)
    {
        if(complete[i] > 0)
            complete[i] = 2;
    }
}
