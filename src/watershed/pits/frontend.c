/* %W% %G% */
#include "gis.h"

char *intro[] =
{
"PITS",
"",
"This program finds all pits in a given elevation data file.",
"The coordinates of these pit points are then written to a" ,
"file to be used later in the watershed delineation program.",
"The pits are sorted in descending order of elevation before",
"being written out.",
0
};

main (argc,argv) char *argv[];
{
    char elev[30], *elev_mapset;
    char command[1024];
    int i;


    G_gisinit (argv[0]);

    G_clear_screen();
    for (i = 0; intro[i]; i++)
        printf ("%s\n", intro[i]);
    
    elev_mapset = G_ask_cell_old ("enter elevation input file name :",
      elev);
    if (!elev_mapset) exit(2);

/* go into background now, running the backend function */

    sprintf (command, "G%s -v 'elev=%s in %s'",
       argv[0], elev, elev_mapset );

    system (command);

}
