/* %W% %G% */
/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
*/

#include  "gis.h"
#include "digit.h"
#include "dig_head.h"


#define V_PROG  "Vprune"   /* non-interactive version */

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


	mapset = G_ask_old( " VECTOR (DIGIT) FILE TO PRUNE FROM",
		name, "dig", "vector") ;

	if ( ! mapset)
		exit(0) ;

	strcpy (in_name, name);

	tmapset = G_ask_new( "Resultant pruned file", name, "site", "site") ;

	if ( ! tmapset)
		exit(0) ;

	printf ( "Enter threshold either as number of ground units of accuracy on real\n");
	printf ( "or number of map inches based on the scale of the file.\n");
	printf ( "If you use map inches, the number must be formated like: 0.04i\n");
	printf ( "Note that the 'i' must follow immediately after the number\n");
	printf ( "\nEnter threshold: ");
	gets (buf);
	if (!strlen (buf))
	    return (0);

	sprintf (command, "%s/bin/%s \"%s in %s\" %s %s", G_gisbase (), V_PROG, in_name, mapset, name, buf);
	system (command);
	printf ("hit RETURN to continue -->");
	gets (command);
	exit (0);
}
