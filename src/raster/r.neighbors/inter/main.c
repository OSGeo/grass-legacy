#include "gis.h"

main(argc,argv)	char *argv[];
{
    char method[100];
    int nsize;
    char input[100];
    char *mapset;
    char *output[100];
    char title[1024];
    char command[1024];

/* get user for mail message and pgm name */
    G_gisinit (argv[0]);

/* explain program to user */
    explain();

/* ask for the input raster file name */
    mapset = G_ask_cell_old("", input);
    if (!mapset)
	exit(0);


/* get the method and the neighborhood size */
    ask_method(method);
    nsize = ask_nsize();

/* ask for new raster file name */
    if(NULL == G_ask_cell_new ("", output))
	exit(0);

    ask_title (input, output, method, nsize, title);

    sprintf (command,
	"r.neighbors input='%s' output='%s' method='%s' size=%d title='%s'",
	G_fully_qualified_name(input, mapset), output, method, nsize, title);
    exit (system(command));
}
