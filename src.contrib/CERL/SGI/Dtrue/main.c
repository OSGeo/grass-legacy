/*  %W%  %G%  */

/*
 *   Dcell
 *
 *   Usage:  Dcell cell_file
 *
 */

#define USAGE	"cell_file"

#define MAIN
/*
#include "options.h"
*/
#include "gis.h"

main(argc, argv)
	int argc ;
	char **argv ;
{
	char buff[128] ;
	char *name1, *mapset1;
	char *name2, *mapset2;
	char *name3, *mapset3;

/* Initialize the GIS calls */
	G_gisinit("Dcell") ;

/* Check command line */

	if (argc != 4)
	    fprintf (stderr, "Usage: %s file1 file2 file3\n", argv[0]), exit(1);

	name1 = G_store (argv[1]);
	name2 = G_store (argv[2]);
	name3 = G_store (argv[3]);


/* Make sure maps are available */
	mapset1 = G_find_cell2 (name1, "") ;
	if (mapset1 == NULL)
	{
	    sprintf(buff,"Cellfile [%s] not available", name1);
	    G_fatal_error(buff) ;
	}
	mapset2 = G_find_cell2 (name2, "") ;
	if (mapset2 == NULL)
	{
	    sprintf(buff,"Cellfile [%s] not available", name2);
	    G_fatal_error(buff) ;
	}
	mapset3 = G_find_cell2 (name3, "") ;
	if (mapset3 == NULL)
	{
	    sprintf(buff,"Cellfile [%s] not available", name3);
	    G_fatal_error(buff) ;
	}

	Dcell(name1, mapset1, name2, mapset2, name3, mapset3) ;

	sleep (999999);
}
