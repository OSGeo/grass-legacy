#include "gis.h"
main(argc, argv) char *argv[];
{
    char input[256], *mapset, output[256];
    double *distances, *getdists();
    char units[100];
    char command[1024];
    char dist[100];
    int i,ndist;

    G_gisinit (argv[0]);

    if(!(mapset = G_ask_cell_old (
     "Select existing map from which to compute distance buffers", input)))
	exit(0);
    if (!G_ask_cell_any (
	"Enter name for new map to contain the distance results", output))
	    exit(0);
    while(1)
    {
	distances = getdists(&ndist, units);
	if (ndist > 0) break;
	if(!G_yes("No distance zones specified. Do you want to try again? ", -1))
	    exit(0);
    }

/* build command line call */
    sprintf (command, "r.buffer input='%s' output='%s' units='%s' distances='",
	G_fully_qualified_name(input, mapset), output, units);
    for (i=0; i < ndist; i++)
    {
	if(i) strcat (command, ",");
	sprintf (units, "%lf", distances[i]); /* can use units array as temp */
	G_trim_decimal(units);
	strcat(command,units);
    }
    strcat(command,"'");
printf ("%s\n", command);
    system(command);
}
