#include "gis.h"


/* Chris Rewerts and Raghaven Srinivasan
   Agricultural Engineering, Purdue University

   This is a modification of r.slope.aspect
   
   The purpose is to compute slope percent in the format ANSWERS
   requires (slope * 10; i.e. a slope of 1.1% will be a cat value
   of 11)
*/


char *intro[] =
{
"SLOPE",
"",
"This program produces a slope map from elevation data.",
"The elevation layer must be true elevation values, not rescaled.",
"The slope map will have slopes in percent multiplied by 10",
"(for example, a category value of 15 would indicate 1.5% slope)",
"Please enter the elevation file, and the name for the resulting",
"slope map.",
0
};

main (argc,argv) char *argv[];
{
    char elevation[30], *mapset;
    char slope[30];
    char command[1024];
    int i;

    G_gisinit (argv[0]);

    for (i = 0; intro[i]; i++)
	printf ("%s\n", intro[i]);
    
    mapset = G_ask_cell_old ("enter elevation file", elevation);
    if (!mapset) exit(0);

    if (!G_ask_cell_new("enter slope file", slope)) *slope = 0;
    if (*slope == 0)
    {
	exit(0);
    }


/* run the command-line version */
    sprintf (command, "r.slope 'elevation=%s'", 
	G_fully_qualified_name(elevation, mapset));
	strcat (command, " slope=");
	strcat (command, slope);

    exit (system(command));
}
