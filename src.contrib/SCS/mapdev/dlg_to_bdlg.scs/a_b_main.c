/*  @(#)a_b_main.c	2.1  6/26/87  */

static char rcsid[]="$Header$";

#include <stdio.h>
#define MAIN
#include "dlghead.h"
#include "format.h"

/*
*	old version is for 1.0 release dlg files
*	new version is for 2.0 release and beyond
*/

main(argc, argv)
	int argc ;
	char *argv[] ;
{
	int i ;
	char  *ascii ;
	char  *binary ;
	FILE *dlg, *f_cats ;
	FILE *bin ;
	FILE *fopen() ;
	char filename[128],dlgname[128],binname[128],catsname[128] ;
	char  *rindex() ;
	char command[256];

	extern	int	new_format ;

	setbuf(stdout, 0) ;

/* Print warning */
	if (argc != 3 && argc != 4)
	{
		printf("\nUsage: %s old-dlg-ascii-file new-dlg-binary-file\n",
			argv[0]) ;
		printf("  or : %s old-dlg-ascii-file new-dlg-binary-file -o\n",
			argv[0]) ;
		exit(-1) ;
	}

	if ( argc == 4)
	 {
		if ( strcmp ( argv[3], "-o")) 
		 {
			printf("\nUsage: %s old-dlg-ascii-file new-dlg-binary-file\n",
				argv[0]) ;
			printf("  or : %s old-dlg-ascii-file new-dlg-binary-file -o\n\n",
				argv[0]) ;
			exit(-1) ;
		 }

		new_format = 0 ;
	 }
	else
		new_format = 1 ;

sprintf(dlgname,"%s",argv[1]);
sprintf(binname,"%s/%s/bdlg/%s",G_location_path(),G_mapset(),argv[2]);
sprintf(catsname,"%s/%s/dig_cats/%s",G_location_path(),G_mapset(),argv[2]);
/* Print warning */
	if ( (dlg = fopen(dlgname, "r")) == NULL)
	{
		printf("Can't find %s\n", dlgname) ;
		exit(-1) ;
	}
	if ( (f_cats = fopen(catsname, "w")) == NULL)
		{
		printf("Can't open %s\n", catsname) ;
		exit(-1) ;
		}

	if ( ! (ascii= rindex(dlgname, '/')))
		ascii = dlgname ;
	else
		++ascii ;

	if ( ! (binary= rindex(binname, '/')))
		binary = binname ;
	else
		++binary ;

    /*
	printf("\n\n Ascii DLG to binary DLG conversion routine\n") ;
	printf("\nConverting the ascii format file: %s\n", ascii);
	printf("     to the %sbinary file format: %s\n\n",
		new_format ? "": "OLD " , binary);
    */

/* Read the header section */
	if (read_dlg_head(dlg) == -1)
	{
		printf("Error in reading header\n") ;
		exit (-1) ;
	}

/* Check to see if we are going to make multiple files.  Binary
 * files contain only one category per file while "optional" DLG
 * files can contain up to 32 category overlays.
 */

	{
	/* Open file for writing */
		if ( (bin = fopen(binname, "w")) == NULL)
		{
			printf("Can't open %s\n", binname) ;
			exit(-1) ;
		}

	/* Write binary dlg head */
		if (write_bdlg_head(bin, 0) == -1)
		{
			printf("Error in writing binary dlg header\n") ;
			exit (-1) ;
		}
		
	/* Read and write the main body */
		sprintf(command,"cat %s >> %s",a_b_dlg(dlg, bin,f_cats),catsname);
		fclose (dlg) ;
		fclose (bin) ;
		fclose (f_cats);
		system(command);
		fprintf(stderr,"%s\n",command);

	}

	exit(0);
}
