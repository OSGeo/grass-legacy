/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
**
**  GRASS4.0: coverted for new parser, 1/91 - dks
*/

#include  "gis.h"
#include "digit.h"
#include "dig_head.h"


#define V_PROG  "v.to.rast"   /* non-interactive version */

struct Map_info Map;
struct dig_head d_head;

main(argc, argv)
    char *argv[];
{
	FILE *out;
	int att, line;
	register int num, i;
	char  name[150] ;
	char  command[200];
	char  out_name[150] ;
	char  in_name[150] ;
	char  *mapset ;
	char *X, *Y;
	int n_points;

	G_gisinit(argv[0]);


	mapset = G_ask_vector_old( " \nVector file to convert from",
		name) ;

	if ( ! mapset)
		exit(0) ;

	/*
	G__file_name( in_name, "dig", name, mapset) ;
	*/
	strcpy (in_name, name);

	if ( ! G_ask_cell_new( "Resultant Raster file", name) )
		exit(0) ;


	sprintf (command, "%s input=%s output=%s", V_PROG, G_fully_qualified_name (in_name, mapset), name);
	system (command);
	printf ("hit RETURN to continue -->");
	gets (command);
	exit (0);
}
