#include <stdio.h>
#include "gis.h"
#include "Vect.h"

main (argc, argv)
    char *argv[];
{
    struct Map_info Map;
	struct GModule *module;
    struct Option *vectfile, *value, *typopt;
    struct Flag *incr, *Nosup;
    char *mapset;
    char errmsg[1000];
    int level;
    struct line_pnts *Points;
    FILE *afp;
    double X, Y;
    int i, ret;
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
    typopt->answer           =  "line";
    typopt->options          =  "point,line,both";
    typopt->description      =  "Select point or line";

    value = G_define_option();
    value->key 		= "value";
    value->type		= TYPE_INTEGER;
    value->required		= NO;
    value->multiple		= NO;
    value->description	= "value of label";
    value->answer = "1";

    incr = G_define_flag ();
    incr->key 		= 'i';
    incr->description 	= "Label incrementally";

    Nosup = G_define_flag ();
    Nosup->key 		= 'n';
    Nosup->description 	= "Do NOT run v.support after labelling";

    Points = Vect_new_line_struct ();

    if (G_parser (argc, argv))
	exit(-1);

    otype = 0;
    if (typopt->answer[0] == 'p')
        otype = DOT;
    else if (typopt->answer[0] == 'l')
        otype = LINE;
    else if (typopt->answer[0] == 'b')
        otype = DOT | LINE;	


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

    if (level < 2)
    {
	printf ("\n");
	printf ("v.support has not been run.  \n");
	printf ("\n");
	exit (1);
    }


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

    label = atoi (value->answer);
    for (i = 1 ; i <= Map.n_lines ; i++)
    {
	if (0 != Map.Line[i].att)
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
	else if (type == LINE)
	{ 
	    X = Points->x[1] + 0.5*(Points->x[0] - Points->x[1]);
            Y = Points->y[1] + 0.5*(Points->y[0] - Points->y[1]);
	    tp = 'L';
        }

	write_att (afp, tp, X, Y, label);
	if (incr->answer)
	    label++;
	cnt++;
    }


    fclose (afp);
    Vect_destroy_line_struct(Points);
    Vect_close (&Map);

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
