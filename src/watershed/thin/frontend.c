/* %W% %G% */
#include "gis.h"

char *intro[] =
{
"THIN",
"",
"This program is used to find a stream network from a drainage",
"accumulation file, where the detail of the network is determined",
"by the drainage threshold entered by the user.  Program THIN",
"will also thin this stream network to a depiction that is one",
"pixel in width.  The number of iterations needed to thin to",
"this extent will need to be estimated.",
0
};

main (argc,argv) char *argv[];
{
    char river_name[30], *river_mapset;
    char thin_name[30], *thin_mapset;
    int iters, dthres;
    char command[1024];
    int i;
    char buf[100];


    G_gisinit (argv[0]);

    G_clear_screen();
    for (i = 0; intro[i]; i++)
        printf ("%s\n", intro[i]);
    
    river_mapset = G_ask_cell_old("enter name of upslope accumulation file",
	river_name);
    if (!river_mapset) exit(2);

    thin_mapset = G_ask_cell_new("enter name for thinned network file",
	thin_name);
    if (!thin_mapset) exit(2);

    do
    {
        do printf("Enter number of thinning iterations: ");
        while (!G_gets(buf));
    }
    while (sscanf (buf, "%d", &iters) != 1);

    if (iters < 0) iters = 0;
    printf ("%d iterations\n",iters);

    do
    {
        do printf("Enter drainage area threshold: ");
        while (!G_gets(buf));
    }
    while (sscanf (buf, "%d", &dthres) != 1);
    printf ("threshold %d\n", dthres);

/* go into background now, running the backend function */

    sprintf (command,
      "G%s -v 'accum=%s in %s' 'thin=%s' iters=%d dthres=%d",
       argv[0], river_name, river_mapset, thin_name, iters, dthres );

    system (command);

}
