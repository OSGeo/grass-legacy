/* %W% %G% */
#include "gis.h"

char *intro[] =
{
"SLOPE.ASPECT",
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
    char *tempfile;
    FILE *fd, *popen();
    int i;


    G_gisinit (argv[0]);

    G_clear_screen();
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

/* go into background now */
    printf ("you will recieve mail when products are complete\n");
    if (G_fork() > 0) exit(0);

/* run the backend function 
 * capture its output to be mailed to user
 */
    tempfile = G_tempfile();

    sprintf (command, "Gslope.aspect -v 'elevation=%s in %s'", elevation, mapset);
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
    strcat (command, " > ");
    strcat (command, tempfile);

    fd = popen (command,"w");

    if (fd)
    {
	pclose (fd);
	sprintf (command,"mail '%s' < %s", G_whoami(), tempfile);
	system (command);
    }
    else
    {
        sprintf (command,"mail '%s'", G_whoami());
	fd = popen (command,"w");
	fprintf (fd, "SLOPE PRODUCTS -- could not run Gslope.aspect\n");
	pclose (fd);
    }
}
