/*  @(#)a_b_front.c     2.1  8/22/87   */
#include  "gis.h"

#define		B_DIR		"dig"
#define		A_DIR		"dig_ascii"
main()
{

	char  name[150] ;
	char  bin_file[80] ;
	char  asc_file[80] ;
	char  *mapset ;

	G_gisinit("Digit Conversion");


	mapset = G_ask_old( " ASCII VECTOR (DIGIT) FILE TO CONVERT",
		name, A_DIR, "ascii vector") ;

	if ( ! mapset)
		exit(0) ;

	G__file_name( asc_file, A_DIR, name, mapset) ;

	mapset = G_ask_new( " FILENAME TO STORE BINARY VECTOR",
		name, B_DIR, "binary vector") ;

	if ( ! mapset)
		exit(0) ;

	G__make_mapset_element( B_DIR) ;
	G__file_name( bin_file, B_DIR, name, mapset) ;

	sprintf( name, "%s/etc/a.b.vect  %s  %s  ", G_gisbase(),
		asc_file, bin_file) ;

	system( name) ;

	fprintf(stderr, "\n\nBefore this vector file can be used in the 'digit' program:\nRun the program support.vect to build the  needed support files.\n") ;

	exit(0) ;

}
