/* @(#)main.c	2.1   6/26/87 */

/*
 *   Dsave
 *
 *   Usage:  Dsave save_file
 *
 */

#define USAGE	"save_file"

#include <stdio.h>

main(argc, argv)
	int argc ;
	char **argv ;
{
	FILE *G_fopen_new() ;
	FILE *outfile ;
	char *file_name ;
	char buff[128] ;

/* Initialize the GIS calls */
	G_gisinit("screen save") ;

/* Check command line */
	if ( (argc > 2) || ! strncmp(argv[1], "help", 4) )
	{
		fprintf(stderr,"Usage: %s %s\n", argv[0], USAGE) ;
		fprintf(stderr,"       %s > filename\n", argv[0]) ;
		exit(-1) ;
	}

	if (argc == 2)
	{
		file_name = argv[1] ;

	/* Make sure map is available */
		outfile = G_fopen_new ("screen", file_name) ;
		if (outfile == NULL)
		{
			sprintf(buff,"Can't open screen save file [%s]", file_name);
			G_fatal_error(buff) ;
		}

		save_info(outfile) ;

		fclose(outfile) ;
	}
	else
	{
		save_info(stdout) ;
	}
}
