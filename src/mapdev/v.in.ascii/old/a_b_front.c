#include  "gis.h"

#define		B_DIR		"dig"
#define		A_DIR		"dig_ascii"
main(argc,argv)
	char **argv ;
{

	char  name[150] ;
	char command[200];
	char  bin_file[80] ;
	char  *asc_file;
	char  *mapset ;

	G_gisinit(argv[0]) ;


	mapset = G_ask_old( " ASCII VECTOR (DIGIT) FILE TO CONVERT",
		name, A_DIR, "ascii vector") ;

	if ( ! mapset)
		exit(0) ;

	asc_file = G_fully_qualified_name (name, mapset);
	/*G__file_name( asc_file, A_DIR, name, mapset) ;*/

	mapset = G_ask_vector_new( " FILENAME TO STORE BINARY VECTOR", name) ;

	if ( ! mapset)
		exit(0) ;

	/*
	G__make_mapset_element( B_DIR) ;
	G__file_name( bin_file, B_DIR, name, mapset) ;
	*/

	sprintf( command, "v.in.ascii  in='%s' out=%s", asc_file, name) ;

	system( command) ;

	fprintf(stderr, "\n\nBefore this vector file can be used in the 'digit' program:\nRun the program v.support to build the needed support files.\n") ;

	exit(0) ;

}
