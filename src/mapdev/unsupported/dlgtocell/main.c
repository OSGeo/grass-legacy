/* %W%  %G% */
/* Three programs take part in the dlg to cell conversion:
 *
 *   dlg_file
 *      +     --> DLG_TO_BMIF | SORT | BMIF_TO_CELL --> cell_file
 *   cell_header
 *
 *  DLG_TO_BMIF - generates an ascii (slow) file who's records encode
 *                the starting and ending columns for the various
 *                categories across each row.
 *  SORT -        Currently this is the UNIX sort(1) program which 
 *                sorts the BMIF records by row.
 *  BMIF_TO_CELL- Turns the sorted BMIF file into a grid_cell file.
 *
 *  Input: 
 *  dlg_file - This is a BINARY encoded version of the USGS defined 
 *             digital line graph.  (GRASS utilities exist to convert
 *             back and forth between binary and ascii versions of 
 *             DLG level 3 data.
 *  cell_header - This is a support file for the cell file that will be
 *             created.  It also provides DLG_TO_BMIF with the geographic
 *             location and number of rows and columns in the grid-cell file.
 */

#include <stdio.h>
#include "dlg.h"
#include "gis.h"
#include "bmif.h"


main(argc, argv)
	int argc ;
	char *argv[] ;
{
	FILE *fp_dlg, *fopen() ;
	struct Cell_head header ;
	struct dlg dlg_struct ;
	char buffer[128] ;
	char *G_gisdbase() ;
	char *G_mapset() ;
	char *G_location() ;
	int pkgs ;
	int cell_type ;
	int packages ;

/* Check for correct arguments */
	if (argc != 2)
	{
		fprintf(stderr, "USAGE: %s map_name\n", argv[0]) ;
		exit(-1) ;
	}

/* Initialize gis library */
	G_gisinit("DLG_TO_CELL") ;

	cell_type = sizeof(CELL) - 1 ;


/* Create header file */
	G_get_window(&header) ;
	header.format = cell_type ;
	G_put_cellhd(argv[1], &header) ;

	prime_convert(&header) ;

/* Open dlg file */
	sprintf(buffer,"%s/%s/%s/bdlg/%s",
		G_gisdbase(), G_location(), G_mapset(), argv[1]) ;
	if (NULL == (fp_dlg = fopen(buffer, "r")))
		G_fatal_error("Opening dlg file.") ;
	
/* Do initial read of DLG file */
	if (-1 == dlg_init(fp_dlg, &dlg_struct ) )
		G_fatal_error("Reading dlg file.") ;
	if (-1 == dlg_read(fp_dlg, &dlg_struct) )
		G_fatal_error("Reading dlg file.") ;

/**** Create history file,  not any more.  History file has changed for 3.0.

   		*****  commented out  *****

	fprintf( stderr, "Creating history file.\n") ;
	switch ( create_hist(argv[1], &dlg_struct) )
	{
		case 1:
			fprintf( stderr, "History file exists.\n\n") ;
			break ;
		case 0:
			break ;
		case -1:
			fprintf( stderr, "ERROR: Couldn't create history file.\n") ;
			exit(-1) ;
			break ;
		default:
			break ;
	}
  end of comment out **************\

	packages = 0 ;

/* Process areas */

	fprintf( stderr, "Processing areas..\n") ;
	pkgs = do_areas(fp_dlg, &dlg_struct) ;
	if (pkgs < 0)
		G_fatal_error("Working on dlg areas.") ;
	packages += pkgs ;

/* Process lines */
	fprintf( stderr, "Processing lines..\n") ;
	pkgs = do_lines(fp_dlg, &dlg_struct) ;
	if (pkgs < 0)
		G_fatal_error("Working on dlg lines.") ;
	packages += pkgs ;

/* Clean up and quit */
	wrapup() ;
	fclose(fp_dlg) ;
	exit(0) ;
}
