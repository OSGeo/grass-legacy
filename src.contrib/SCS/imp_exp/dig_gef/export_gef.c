/* %W% %G% */
/*
**  Written by R.L.Glenn 2/90
**  USDA, Soil COnservation Service, CGIS Division
*/

#include  "gis.h"
#define V_PROG  "v.out.scsgef"   /* non-interactive version */

main(argc, argv)
    char *argv[];
{
	int   ret;
	char  name[150],  command[200], in_name[150] ;
	char  *mapset ;

	G_gisinit(argv[0]);


	mapset = G_ask_old( " VECTOR (DIGIT) FILE TO EXPORT FROM",
		name, "dig", "vector") ;

	if ( ! mapset)
		exit(0) ;

	strcpy (in_name, name);

	tmapset = G_ask_new( "Resultant SCS-GEF file", name, "gef", "gef") ;

	if ( ! tmapset)
		exit(0) ;

	sprintf (command, "%s/bin/%s \"%s in %s\" %s", G_gisbase (), V_PROG, in_name, mapset, name);

        ret = system (command);
	if (ret & 0xff00)
	   G_fatal_error ("File export failed\n");
	fprintf (stdout,"hit RETURN to continue -->");
	gets (command);
	exit (0);
}
