#include "gis.h"

char *intro[] =
{
"SLOPE and ASPECT",
"",
"This program produces aspect and slope maps from elevation data.",
"The elevation layer must be true elevation values, not rescaled.",
"The aspect map will indicate the direction of the slopes in 24",
"directions.  The slope map will have slopes in degrees.",
"Please enter the elevation file, and the names for the resulting",
"aspect and slope maps.",
0
};

main (argc,argv) char *argv[];
{
    char elevation[30], *mapset;
    char aspect[30];
    char slope[30];
    char command[1024];
    int i;
    int zero_is_data;


    G_gisinit (argv[0]);

    for (i = 0; intro[i]; i++)
	printf ("%s\n", intro[i]);
    
    mapset = G_ask_cell_old ("enter elevation file", elevation);
    if (!mapset) exit(0);

    G_set_ask_return_msg ("if you don't want this product");
    if (!G_ask_cell_new("enter aspect file", aspect)) *aspect = 0;
    G_set_ask_return_msg ("if you don't want this product");
    if (!G_ask_cell_new("enter slope file", slope)) *slope = 0;
    if (*aspect == 0 && *slope == 0)
    {
	printf ("no elevation products requested\n");
	exit(0);
    }
    if (G_yes("Do zero values in the elevation map represent true elevations? ", -1))
	zero_is_data = 1;
    else
	zero_is_data = 0;


/* run the command-line version */
    sprintf (command, "r.slope.aspect 'elevation=%s'", 
	G_fully_qualified_name(elevation, mapset));
    if (*aspect)
    {
	strcat (command, " aspect=");
	strcat (command, aspect);
    }
    if (*slope)
    {
	strcat (command, " slope=");
	strcat (command, slope);
    }
    if (zero_is_data)
	strcat (command, " -z");
    exit (system(command));
}
