/*
**  Written by Dave Gerdes  6/89
**  US Army Construction Engineering Research Lab
**
**  modified for new command parser, dec 90 - dks
*/

#include  "gis.h"
#include "digit.h"
#include "dig_head.h"


#define V_PROG  "v.prune" 

struct Map_info Map;
struct dig_head D_head;

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
	char buf[100];
	char  *mapset ;
	char  *tmapset ;
	char *X, *Y;
	int n_points;
	int inches;

	G_gisinit(argv[0]);


	mapset = G_ask_vector_old( "\nVECTOR (DIGIT) FILE TO PRUNE FROM",
		name) ;

	if (mapset == NULL)
		exit(0) ;

	strcpy (in_name, name);

	tmapset = G_ask_vector_new( "\nRESULTANT PRUNED FILE:", name) ;

	if (tmapset == NULL)
		exit(0) ;

	printf ( "\nBefore entering threshold value, you must specify whether the value\n");
	printf ( "is the number of ground units of accuracy,\n");
	printf ( "or number of map inches based on the scale of the file.\n\n");
	inches = G_yes ("Threshold value in inches? ", 0);


	do {
		printf ( "\nEnter threshold value: ");
	} while (! G_gets (buf));

	if (!strlen (buf))
	    return (0);

/*call to v.prune = call to link, which will route call to cmd-line*/
/*version of v.prune*/

	sprintf (command, "v.prune %s input='%s' output='%s' thresh='%s'",
	   inches?"-i":"", G_fully_qualified_name (in_name, mapset), name, buf);
	system (command);

	/*
	printf ("hit RETURN to continue -->");
	gets (command);
	*/
	exit (0);
}
