#include  "gis.h"

#define		B_DIR	"dig"
#define		A_DIR	"dig_ascii"

main()
{

	char  name[150] ;
	char  bin_file[80] ;
	char  asc_file[80] ;

	char  *mapset ;

	G_gisinit("Digit Conversion");

	mapset = G_ask_old( " BINARY VECTOR (DIGIT) FILE TO CONVERT", name,
				B_DIR, "binary vector") ;
	if ( ! mapset)
		exit(0) ;

	G__file_name( bin_file, B_DIR, name, mapset ) ;

	mapset = G_ask_new( " FILENAME TO STORE ASCII VECTOR FILE", name,
				A_DIR, "ascii vector") ;
	if ( ! mapset)
		exit(0) ;

	G__file_name( asc_file, A_DIR, name, mapset ) ;
	G__make_mapset_element( A_DIR) ;

	sprintf( name, "%s/etc/b.a.vect  %s  %s  ", G_gisbase(),
		bin_file, asc_file) ;


	system( name) ;
	exit(0) ;
}
