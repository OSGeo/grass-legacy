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

   function: get_res
   called by: step_1

   gets the project resolution from user, sets the proj_resolution global
   
   */
#include "answers.h"

get_res()

{
    char string[100];
    int i, num_ok, num;
    G_clear_screen();
    fprintf (stdout,"\n");
    fprintf (stdout,"     ANSWERS on GRASS Project Resolution Utility\n");
    
/* if complete is 1 or 2, this means the user has already set the resolution
   but we offer the option in case they wish to re-set it */
    if (complete[1] > 0)
    {
	fprintf (stdout,"\nYou have already set the grid cell size at %.f meters.\n",
	proj_resolution);
        fprintf (stdout,"If the project is changed at this time, all previously\n");
        fprintf (stdout,"completed steps must be run again.\n\n");
	if (!G_yes("Do you wish to re-define a grid cell size at this time?", 0))
	    return (0);

/* if we are changing resolution, we will set complete[1] to 2 to tell the
   trim_region program it needs to re-size the window, er, region */
    
    complete[1] = 2;
    alter_status();
    }
    fprintf (stdout,"\n");
    fprintf (stdout,"\nThis is your opportunity to set the size of the raster elements\n");
    fprintf (stdout,"to be used for the ANSWERS simulation. Both the north-south and\n");
    fprintf (stdout,"east-west cell dimensions will be set the the value you input, since\n");
    fprintf (stdout,"ANSWERS is limited to using square elements.\n");
    fprintf (stdout,"\n");

    num_ok = 1;
    while (1)
    {
	fprintf (stdout,"\n");
	fprintf (stdout,"Press RETURN to cancel this operation.\n");
	fprintf (stdout,"\n");
	fprintf (stdout,"Input the cell size resolution (in meters) -> ");
	G_gets(string);
	
/* error check the user's input */

/* if they only hit return and we are running this for the second time,
    returning 0 will allow them to move on to setting the region.
    otherwise, we return 1 to cancel the whole step's operation */
    
	if (strlen(string) < 1)
	    {
	    if (complete[1] == 2)
	        return (0);
	    else 
	        return (1);
	    }

/* is it a number? */	
    
	for (i = 0; i < strlen(string); i++)
	{
	    if (!isdigit(string[i]))
		num_ok = 0;
	}
	if (!num_ok)
	{
	    fprintf (stdout,"\n\7[enter a whole number]\n\n");
	    hit_return();
	    fprintf (stdout,"\n");
	    num_ok = 1;
	    continue;
	}
	sscanf(string, "%d", &num);
	if (num < 0)
	{
	    fprintf (stdout,"\n\7[enter a whole number 0 or larger]\n\n");
	    hit_return();
	    fprintf (stdout,"\n");
	    continue;
	}
	proj_resolution = (float) num;
	fprintf (stdout,"\n");
	fprintf (stdout,"Project cell size being set to %.f meters square.\n", 
	proj_resolution);
	fprintf (stdout,"cell area: %.3f hectares (%.3f acres)\n",
	    (proj_resolution * proj_resolution / 10000) + .0005,
	    (proj_resolution * proj_resolution / 4046.856) + .0005);
	if (!G_yes("\nIs this ok? ", 1))
	    continue;
	else
	    break;
    }
    return (0);
}
