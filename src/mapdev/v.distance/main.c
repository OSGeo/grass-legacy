/*
**  v.distance
**  J.Soimasuo 15.9.1994 
**  University of Joensuu, Faculty of Forestry, Finland
*/

#include <stdio.h>
#include <string.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
    register int ret, error;
    char vectname[1024];
    char *mapset, maps[1024];
    double trim_factor;
    struct Option *opt1;
    struct Map_info Map;
    int level;

    opt1 = G_define_option() ;
    opt1->key        = "input" ;
    opt1->type       = TYPE_STRING ;
    opt1->gisprompt  = "old,dig,Vector";
    opt1->required   = YES ;
    opt1->description= "Name of existing vector file" ;


    setbuf (stdout, NULL);
    G_gisinit (argv[0]);


    if (G_parser(argc, argv))
        exit(-1);

   
    error = 0; 

    /* Open mapput and output vector files */

    strcpy (vectname, opt1->answer);


    if (error)
	exit (-1);

/* Copy header stuff */
    mapset = G_find_file2 ("dig", vectname, "");

    level = Vect_open_old (&Map, vectname, mapset);
    if (0 > level)
	G_fatal_error ("Can't open vector file");
    if (2 > level)
	G_fatal_error ("Must first run v.support on vector file");


    ret = distance (&Map);
    Vect_close (&Map);
    exit (0);
}

int oops (int line, char *buf, char *msg)
{
    static int first = 1;
    if (!isatty(0))
    {
	if (first)
	{
	    fprintf (stderr, "%s: ** input errors **\n",
		G_program_name());
	    first = 0;
	}
	fprintf (stderr, "line %d: %s\n", line, buf);
    }
    fprintf (stderr, "** %s **\n", msg);

    return 0;
}
