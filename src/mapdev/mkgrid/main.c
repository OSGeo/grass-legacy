
/*
*  Written by GRASS, Fall of 88,  Michael H.
*/


#include	<stdio.h>
#include	"gis.h"
#include	"grid_structs.h"


/**  data directories   **/
#define		B_DIG		"dig"

static char  *PROG ;


main (argc, argv)
	int  argc ;
	char  *argv[] ;
{

	/*  store filename and path  */
	char  dig_file[128] ;
	char  buffer[128] ;

	FILE  *fp_digit,  *fopen() ;

	struct  grid_description  grid_info ;
	struct Cell_head window ;

	PROG = argv[0] ;
	G_gisinit(PROG) ;
	system("clear") ;
	setbuf(stdout, NULL) ;

    /*  get the current window  */
	G_get_window(&window) ;


    /*  make sure dig directory is there  */
	G__make_mapset_element( B_DIG) ;

/*  information we need to collect from user: origin point x and y (lower left),
*   shift in x, shift in y,  number of rows, number of cols
*/

	welcome_mat() ;

	ask_for_name( dig_file, " VECTOR FILENAME ", buffer,
		B_DIG, "binary vector") ;

	if ( (fp_digit = fopen(dig_file, "w"))  ==  NULL)
	{
		fprintf(stderr, " %s: Can't open file for write: %s\n", PROG, dig_file) ;
		exit(-1) ;
	}

	ask_for_int ( &grid_info.num_rows, "\n Enter the number of rows: ") ;
	ask_for_int ( &grid_info.num_cols, " Enter the number of columns: ") ;

	ask_for_double ( &grid_info.origin_x, "\n Enter the very lower left easting (x) of the grid: " ) ;
	ask_for_double ( &grid_info.origin_y, " Enter the very lower left northing (y) of the grid: " ) ;

	ask_for_double ( &grid_info.length, "\n Enter the length of a box in the grid (distance to the east): " ) ;
	ask_for_double ( &grid_info.width, "\n Enter the width of a box in the grid (distance to the north): " ) ;

    /*  vector rows are the actual number of rows of vectors to make up the
    *   entire grid.   ditto for cols.
    */
	grid_info.num_vect_rows =  grid_info.num_rows + 1 ;
	grid_info.num_vect_cols =  grid_info.num_cols + 1 ;

    /*  initialize and write the digit vector header  */
	printf("\n Creating vector header...") ;
	init_header ( fp_digit, &window) ;

	printf("\n Creating vector grid...") ;
	write_grid ( fp_digit, &grid_info ) ;
	printf("\n Finished\n\n") ;

	fclose (fp_digit) ;


}		/*  main()  */

