#define MAIN
#include "dxf2vect.h"
#undef MAIN

main (argc, argv)
int	argc;
char	*argv[];
{
	FILE	*fopen(), *dxf_file;

	if (argc != 3)
	{
		fprintf (stderr, "\n%d is the wrong # of args\n", argc);
		exit (-1);
	}
	if ((dxf_file = fopen (argv[1], "r")) == NULL)
	{
		fprintf (stderr, "\ncannot open [%s] for dxf file\n", argv[1]);
		exit (-2);
	}
	strcpy (dig_path, argv[2]);
	dxf_init_chars ();
	dxf_find_lines (dxf_file);
	fclose (dxf_file);
	dxf_add_extents ();
	exit (0);
}
