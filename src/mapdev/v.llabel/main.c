/* updated by Roger Miller <rgrmill@rt66.com> 4/02 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include "gis.h"
#include "dig_atts.h"
#include "Vect.h"

int
main (argc, argv)
    char *argv[];
{
    struct Map_info Map;
	struct GModule *module;
    struct Option *vectfile, *value, *clabel, *typopt;
    struct Flag *incr, *Nosup;
    struct Categories cats;
    struct Cell_head window;
    char *mapset;
    char errmsg[1000];
    int level;
    struct line_pnts *Points;
    FILE *afp;
    double X, Y;
    int i;
    int cnt = 0;
    int label, type, otype;
    char tp;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Bulk-labels unlabeled lines in a binary GRASS vector file.";

    vectfile = G_define_option();
    vectfile->key 		= "map";
    vectfile->type		= TYPE_STRING;
    vectfile->required		= YES;
    vectfile->multiple		= NO;
    vectfile->gisprompt		= "old,dig,Vector";
    vectfile->description	= "vector file";

    typopt = G_define_option();
    typopt->key              = "type";
    typopt->type             =  TYPE_STRING;
    typopt->required         =  NO;
    typopt->answer           =  "all";
    typopt->options          =  "point,line,edge";
    typopt->description      =  "Select type of arc to label.";

    value = G_define_option();
    value->key 		= "value";
    value->type		= TYPE_INTEGER;
    value->required		= NO;
    value->multiple		= NO;
    value->description	= "category number";
    value->answer = "1";

    clabel              = G_define_option();
    clabel->key         = "label";
    clabel->description = "category label";
    clabel->type        = TYPE_STRING;

    incr = G_define_flag ();
    incr->key 		= 'i';
    incr->description 	= "Label incrementally";

    Nosup = G_define_flag ();
    Nosup->key 		= 'n';
    Nosup->description 	= "Do NOT run v.support after labelling";

    if (G_parser (argc, argv))
	exit(-1);

    Points = Vect_new_line_struct ();

    otype = DOT | LINE | AREA;
    if (typopt->answer[0] == 'p')
        otype = DOT;
    else if (typopt->answer[0] == 'l')
        otype = LINE;
    else if (typopt->answer[0] == 'e')
        otype = AREA;


    if (!*(vectfile->answer))
    {
	fprintf (stderr, "%s: Command line error: missing vector file name.\n", argv[0]);
	G_usage();
	exit (-1);
    }

    if (NULL == (mapset = G_find_file2 ("dig", vectfile->answer, G_mapset())))
    {
	sprintf (errmsg, "Could not find file '%s'", vectfile->answer);
	G_fatal_error (errmsg);
    }

    if (strcmp (mapset, G_mapset()))
	G_fatal_error ("File must be in current mapset");

    level = Vect_open_old (&Map, vectfile->answer, mapset);

    if (level < 1)
	G_fatal_error ("File open failed");
/*
    if (level < 2)
    {
	printf ("\n");
	printf ("v.support has not been run.  \n");
	printf ("\n");
	exit (1);
    }
*/

#ifdef FOO
    if (NULL != (afp = G_fopen_old ("dig_att", vectfile->answer)))
    {
	fclose (afp);
	fprintf (stderr, "File %s already has a dig_att file!  Will not overwrite.\n", vectfile->answer);
	Vect_close (&Map);
	exit (1);
    }
#endif

    if (NULL == (afp = G_fopen_append ("dig_att", vectfile->answer)))
    {
	fprintf (stderr, "Unable to open dig_att file\n");
	Vect_close (&Map);
	exit (1);
    }

    G_get_default_window (&window);
    window.north = Map.head.N;
    window.south = Map.head.S;
    window.east  = Map.head.E;
    window.west  = Map.head.W;
    label = atoi (value->answer);
    G_suppress_warnings (0);
    if (G_read_vector_cats (vectfile->answer, mapset, &cats) != 0)
    {
        G_init_cats ((CELL) 0, vectfile->answer, &cats);
    }
    G_suppress_warnings (1);
    for (i = 1 ; i <= Map.n_lines ; i++)
    {
	if (0 != V2_line_att (&Map, i))
	    continue;

        if ( 0 >= (type = V2_read_line(&Map, Points,i)))
        {
            sprintf(errmsg,"Could not label line %d\n",i);
	    G_warning (errmsg);
	    continue;
	}
	
	if ( !(type & otype) )
	    continue;
        
	if (type == DOT)
	{ 
	    X = Points->x[0];
            Y = Points->y[0];
	    tp = 'P';    
        }
	else if (type == LINE || type == AREA)
	{ 
	    X = Points->x[1] + 0.5*(Points->x[0] - Points->x[1]);
            X = G_adjust_easting (X, &window); /* for LL wrap ? */
            Y = Points->y[1] + 0.5*(Points->y[0] - Points->y[1]);
	    tp = 'L';
        }

	write_att (afp, tp, X, Y, label);
        G_set_cat ((CELL) label,
                (clabel->answer) ? clabel->answer : "", &cats);
	if (incr->answer)
	    label++;
	cnt++;
    }


    fclose (afp);
    Vect_destroy_line_struct(Points);
    Vect_close (&Map);

    if (G_write_vector_cats (vectfile->answer, &cats) != 1)
        G_warning ("failed to write dig_cats.");

    fprintf (stderr, "Labeled %d new lines.\n\n", cnt);

    if (cnt == 0)
    {
	if (!Nosup->answer)
	    fprintf (stderr, "File not changed.  will not run v.support\n");
	else
	    fprintf (stderr, "File not changed.  v.support does not need to be run\n");
	exit (1);
    }

    if (!Nosup->answer)
    {
	fprintf (stderr, "Running support now...\n\n");
	execlp ("v.support", "v.support", vectfile->answer, NULL);
    }
    else
    {
        fprintf (stderr, "You must run v.support before using this file.\n");
	/*
	**  make sure they don't use it before running support 
	*/
	G_remove ("dig_plus", vectfile->answer);
    }

    exit (0);
}
