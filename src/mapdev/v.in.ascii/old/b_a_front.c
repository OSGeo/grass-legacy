#include  "gis.h"

#define		B_DIR	"dig"
#define		A_DIR	"dig_ascii"

main(argc,argv)
	char **argv ;
{

	char  name[150] ;
	char command[200];
	char  *bin_file;
	char  asc_file[80] ;

	char  *mapset ;

	G_gisinit(argv[0]) ;

	mapset = G_ask_vector_old( " BINARY VECTOR (DIGIT) FILE TO CONVERT", name) ;
	if ( ! mapset)
		exit(0) ;

	bin_file = G_fully_qualified_name (name, mapset);

	/*sprintf (bin_file, "%s in %s", name, mapset);*/

	/*G__file_name( bin_file, B_DIR, name, mapset ) ;*/

	mapset = G_ask_new( " FILENAME TO STORE ASCII VECTOR FILE", name,
				A_DIR, "ascii vector") ;

	if ( ! mapset)
		exit(0) ;

	/*G__file_name( asc_file, A_DIR, name, mapset ) ;*/
	/*G__make_mapset_element( A_DIR) ;*/

	sprintf( command, "v.out.ascii in='%s' out=%s", 
		bin_file, name) ;

	system( command) ;
	exit(0) ;
}
