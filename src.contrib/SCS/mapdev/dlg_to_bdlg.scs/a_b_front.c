static char rcsid[]="$Header$";

#include  "gis.h"

main(argc,argv)
	char **argv ;
{

	char  name[150] ;
	char  old_file[80] ;
	char  new_file[80] ;
	char  *mapset ;

	G_gisinit(argv[0]) ;


	mapset = G_ask_old( " ASCII DLG FILE TO CONVERT",
		name, "dlg", "ascii dlg") ;

	if ( ! mapset)
		exit(0) ;

	G__file_name( old_file, "dlg", name, mapset) ;

	mapset = G_ask_new( " FILENAME TO STORE BINARY DLG",
		name, "bdlg", "binary dlg") ;

	if ( ! mapset)
		exit(0) ;

	G__make_mapset_element("bdlg") ;
	G__file_name( new_file, "bdlg", name, mapset) ;

	sprintf( name, "%s/etc/a.b.dlg  %s  %s  ", G_gisbase(),
		old_file, new_file) ;

	system( name) ;

	exit(0) ;

}
