#include	<stdio.h>
#include "gis.h"
#include	"Vect.h"


main(argc, argv)
	int argc ;
	char *argv[] ;
{
	FILE *binary, *ascii, *fopen() ;
	char *mapset;
	char file[200];
	char errmsg[200];
	char pfile[200];
	struct Map_info Map;

	G_gisinit (argv[0]);

	if (argc != 3)
	{
		printf("USAGE: %s old-plus-binary new-plus-ascii\n", argv[0]) ;
		exit(-1) ;
	}

	strcpy (file, argv[1]);

        if ((mapset = G_find_vector2 (file, "")) == NULL)
        {
                sprintf (errmsg, "Could not find vector file <%s>\n", file);
                G_fatal_error (errmsg);
        }

        Vect_set_open_level (1);        /* Just open level 1 to get started*/
        if ( (Vect_open_old (&Map, file, mapset) ) < 1)
        {
                sprintf(errmsg, "Could not open vector file <%s>\n", file);
                G_fatal_error (errmsg);
        }


	G__file_name (pfile, "dig_plus", file, mapset);
	if ( (binary = fopen(pfile, "r") ) == NULL )
	{
		printf("Not able to open <%s>\n", pfile) ;
		exit(-1) ;
	}

	if ( (ascii = fopen(argv[2], "w") ) == NULL )
	{
		printf("Not able to open <%s>\n", argv[2]) ;
		exit(-1) ;
	}

	write_plus_asc(&Map,  ascii, binary) ;

	fclose(binary) ;
	fclose(ascii) ;
}

