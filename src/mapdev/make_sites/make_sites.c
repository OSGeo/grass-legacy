/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/

#include  "gis.h"
#include "digit.h"
#include "dig_head.h"


#define V_PROG  "Vmake.sites"   /* non-interactive version */

struct Map_info Map;
struct head Head;

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


	mapset = G_ask_vector_old (" VECTOR (DIGIT) FILE TO CREATE SITES FROM",
		name) ;

	if ( ! mapset)
		exit(0) ;

	strcpy (in_name, name);

	tmapset = G_ask_sites_new ("Resultant SITE file", name) ;

	if ( ! tmapset)
		exit(0) ;

	sprintf (command, "%s/bin/%s \"%s in %s\" %s", G_gisbase (), V_PROG, in_name, mapset, name);
	system (command);
	printf ("hit RETURN to continue -->");
	gets (command);
	exit (0);
}
