/*  @(#)b_a_main.c	2.1  6/26/87  */

#include <stdio.h>
#include "gis.h"
#include "format.h"

/*
*	old version is for 1.0 release dlg files
*	new version is for 2.0 release and beyond
*/

main(argc, argv)
	int argc ;
	char **argv ;
{
	FILE *dlg ;
	FILE *bin ;
	FILE *fopen() ;
	char  *ascii ;
	char  *binary ;

	extern	int	new_format ;

	if (argc != 3 && argc != 4)
	{
		printf("\nUsage: %s old-dlg-binary-file new-dlg-ascii-file\n",
			argv[0]) ;
		printf("  or : %s old-dlg-binary-file new-dlg-ascii-file -o\n",
			argv[0]) ;
		exit(-1) ;
	}

	if ( argc == 4)
	 {
		if ( strcmp ( argv[3], "-o")) 
		 {
			printf("\nUsage: %s old-dlg-binary-file new-dlg-ascii-file\n",
				argv[0]) ;
			printf("  or : %s old-dlg-binary-file new-dlg-ascii-file -o\n\n",
				argv[0]) ;
			exit(-1) ;
		 }

		new_format = 0 ;
	 }
	else
		new_format = 1 ;



	if ( (bin = fopen(argv[1], "r")) == NULL)
	{
		printf("Can't find %s\n", argv[1]) ;
		exit(-1) ;
	}

	if ( (dlg = fopen(argv[2], "w")) == NULL)
	{
		printf("Can't open %s\n", argv[2]) ;
		exit(-1) ;
	}

	if ( ! (binary = G_rindex(argv[1], '/')))
		binary = argv[1] ;
	else
		++binary ;

	if ( ! (ascii = G_rindex(argv[2], '/')))
		ascii = argv[2] ;
	else
		++ascii ;

/* Print warning */
	printf("\n\n Binary DLG to ascii DLG conversion routine\n") ;
	printf("\nConverting the %sbinary format DLG file: %s\n",
		new_format ? "": "OLD " , binary);
	printf("    to the ascii DLG format file: %s\n", ascii);

		

/* Read the header section */
	if (b_d_head(bin, dlg) == -1)
	{
		printf("Error in translating header\n") ;
		exit (-1) ;
	}

/* Read the main body */
	if (b_d_dlg(bin, dlg) == -1)
	{
		printf("Error in translating header\n") ;
		exit (-1) ;
	}

	fclose (dlg) ;
	fclose (bin) ;
	exit(0);
}
