
/*
**  Written by Dave Gerdes  Summer 1990
**  US Army Construction Engineering Research Lab
*/


#define USAGE	"cell_file"

#define MAIN
/*
#include "options.h"
*/
#include "gis.h"
#include "externs.h"

main(argc, argv)
	int argc;
	char **argv;
{
	char *elev;
	char *name1;
	char *name2;
	char *name3;
	char *vect;
	char prog_name[BUFSIZ];

	strcpy (prog_name, argv[0]);

	G_gisinit (prog_name);


	AUTO_FILE = NULL;

	/* check for AUTORUN */
	if (argc > 1 && *argv[1] == '-')
	{
	    AUTO_FILE = G_store (++(argv[1]));

	    argc--;	/* one less arg */
	    argv++;	/* point argv to next */
	}

/* Check command line */
	if (argc < 3 && argc > 6)
	{
	    fprintf (stderr, "\n");
	    fprintf (stderr, "Usage: %s elev file1 file2 file3 [vect]\n", prog_name);
	    fprintf (stderr, "  OR   %s elev file1 [vect]\n", prog_name);
	    exit(1);
	}

	elev  = G_store (argv[1]);
	name1 = G_store (argv[2]);

	if (argc < 5)
	{
	    Three_map = 0;
	    name2 = NULL;
	    name3 = NULL;
	    if (argc == 4)
		vect = G_store (argv[3]);
	    else 
		vect = NULL;
	}
	else
	{
	    Three_map = 1;
	    name2 = G_store (argv[3]);
	    name3 = G_store (argv[4]);
	    if (argc == 6)
		vect = G_store (argv[5]);
	    else 
		vect = NULL;
	}

	

	Dcell(elev, name1, name2, name3, vect);
}
