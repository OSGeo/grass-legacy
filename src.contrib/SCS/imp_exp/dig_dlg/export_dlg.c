/* %W% %G% */
/* @(#)v.export_dlg.c   1.1   03/12/91 GRASS4.0 */
/*
**  Written by R.L.Glenn
**  USDA, Soil Conservation Service, CGIS Division
*/

#include  "gis.h"
#include "Vect.h"

#define V_PROG  "v.out.dlg.scs"   /* non-interactive version */

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
	char  *tmapset ;
	char *X, *Y;
	int n_points;

	G_gisinit(argv[0]);


	mapset = G_ask_vector_old( " \nVECTOR FILE TO EXPORT FROM",
		name) ;

	if ( ! mapset)
		exit(0) ;

	strcpy (in_name, name);

	tmapset = G_ask_new( "RESULTING DLG FILE", name, "dlg", "dlg") ;

	if ( ! tmapset)
		exit(0) ;

	sprintf (command, "%s input='%s in %s' output=%s", V_PROG, in_name, mapset, name);
	system (command);
	fprintf (stdout,"hit RETURN to continue -->");
	gets (command);
	exit (0);
}
