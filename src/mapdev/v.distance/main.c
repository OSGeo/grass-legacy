/*
**  added vector att output MN 1/2002
**  added coords opt MN 11/2001
**
**  output format usable for s.in.ascii piping
**
**  E|N|sitesID|dist|vect_line_ATT
**
**  e.g.
**  1680949.350649|5100196.753247|31|16.854858|22002
**
**  v.distance
**  J.Soimasuo 15.9.1994 
**  University of Joensuu, Faculty of Forestry, Finland
*/

#include <stdlib.h>
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
    char *mapset;
    struct GModule *module;
    struct Option *opt1, *opt2;
    struct Map_info Map;
    int level;
    int areas_no;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->description =
	"Calculates distance from a point to nearest line or point in vector layer.";

    opt1 = G_define_option() ;
    opt1->key        = "map" ;
    opt1->type       = TYPE_STRING ;
    opt1->gisprompt  = "old,dig,Vector";
    opt1->required   = YES ;
    opt1->description= "Name of existing vector file" ;

    opt2 = G_define_option();
    opt2->key = "east_north";
    opt2->type = TYPE_DOUBLE;
    opt2->key_desc = "east,north";
    opt2->multiple = YES;
    opt2->required = NO;
    opt2->description = "Coordinates for query";

    setbuf (stdout, NULL);

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

    areas_no=V2_num_areas (&Map);
    if (areas_no > 0)
        G_warning("Found %i areas in map - this may be confusing for the results. \
                  Note that the module only considers vector lines and ignores these areas.", areas_no);

    if( V2_num_lines (&Map) == 0)
    {
      Vect_close (&Map);
      G_fatal_error("This is not a line vector map (found %i areas in map)", areas_no);
    }
      
    ret = distance (opt2->answers, &Map);
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
