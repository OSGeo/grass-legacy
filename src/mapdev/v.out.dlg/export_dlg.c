/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/

#include  "gis.h"
#include "Vect.h"


#define V_PROG  "v.out.dlg"   /* non-interactive version */

struct Map_info Map;
struct dig_head d_head;

int 
main (int argc, char *argv[])
{
	FILE *out;
	int att, line;
	register int num, i;
	char  name[150] ;
	char  command[200];
	char  out_name[150] ;
	char  *in_name;
	char  *mapset ;
	char  *tmapset ;
	char *X, *Y;
	int n_points;

	G_gisinit(argv[0]);


	mapset = G_ask_vector_old( " \nVECTOR FILE TO EXPORT FROM",
		name) ;

	if ( ! mapset)
		exit(0) ;

	/*
	G__file_name( in_name, "dig", name, mapset) ;
	*/
	/*strcpy (in_name, name);*/
	in_name = G_fully_qualified_name (name, mapset);

	tmapset = G_ask_new( "RESULTING DLG FILE", name, "dlg", "dlg") ;

	if ( ! tmapset)
		exit(0) ;

	sprintf (command, "%s input='%s' output=%s", V_PROG, in_name, name);
	system (command);
	fprintf (stdout,"hit RETURN to continue -->");
	gets (command);
	exit (0);
}
