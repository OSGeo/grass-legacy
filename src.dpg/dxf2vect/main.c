#define MAIN
#include "dxf2vect.h"
#undef MAIN

char *rindex ();
main (argc, argv)
int	argc;
char	*argv[];
{
	FILE	*fopen(), *dxf_file;
	char *p;
	char command[300];

	if (argc != 3)
	{
		fprintf (stderr, "Usage: %s infile.dxf dig_directory\n", argv[0]);
		exit (-1);
	}
	if ((dxf_file = fopen (argv[1], "r")) == NULL)
	{
		fprintf (stderr, "\ncannot open [%s] for dxf file\n", argv[1]);
		exit (-2);
	}

	/* make basename from name of dxf file.  This will be used as
	** as prefix for all layers that are created to avoid layer name
	** conflicts between dxf files
	*/
	p = rindex (argv[1], '/');
	if (p == NULL)
	    p = argv[1];
	strcpy (basename, p);
	if (NULL != (p = rindex (basename, '.')))
	    if (p != basename)
		*p = '\0';  /* knock off any suffix */

	strcpy (dig_path, argv[2]);

	sprintf (command, "mkdir %s/dig_ascii > /dev/null", dig_path);
	system (command);
	sprintf (command, "mkdir %s/dig_att > /dev/null", dig_path);
	system (command);

	dxf_init_chars ();
	dxf_find_lines (dxf_file);
	fclose (dxf_file);
	dxf_add_extents ();
	exit (0);
}
