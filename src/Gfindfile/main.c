/* %W% %G% */
#include "gis.h"

#define PGM argv[0]
#define ELEMENT argv[1]
#define NAME argv[2]
#define MAPSET argv[3]

main(argc,argv) char *argv[];
{
	char file[1024], name[200], *mapset;
	char *search_mapset;
	G_gisinit (PGM);

	if (argc < 3 || argc > 4)
		usage(PGM);

	if (argc == 4)
	{
		if(strcmp (".", search_mapset = MAPSET) == 0)
			search_mapset = G_mapset();
	}
	else
		search_mapset = "";
	strcpy (name, NAME);
	mapset = G_find_file2 (ELEMENT, name, search_mapset);
	if (mapset)
	{
		printf ("name='%s'\n",name);
		printf ("mapset='%s'\n",mapset);
		G__file_name (file, ELEMENT, name, mapset);
		printf ("file='%s'\n",file);
	}
	else
	{
		printf ("name=\n");
		printf ("mapset=\n");
		printf ("file=\n");
	}
	exit(mapset==NULL);
}
usage(me) char *me;
{
	fprintf (stderr, "usage: %s element name\n", me);
	exit(1);
}
