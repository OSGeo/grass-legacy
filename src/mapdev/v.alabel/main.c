#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include "gis.h"
#include "dig_atts.h"
#include "Vect.h"

int main (int argc, char *argv[])
{
    struct Map_info Map;
	struct GModule *module;
    struct Option *vectfile, *value;
    struct Flag *incr, *Nosup;
    char *mapset;
    char errmsg[1000];
    int level;
    FILE *afp;
    double X, Y;
    int i, ret;
    int cnt = 0;
    int label;

    G_gisinit(argv[0]);

	module = G_define_module();
	module->description =
		"Bulk-labels unlabeled area features in "
		"a binary GRASS vector file.";

    vectfile = G_define_option();
    vectfile->key 		= "map";
    vectfile->type		= TYPE_STRING;
    vectfile->required		= YES;
    vectfile->multiple		= NO;
    vectfile->gisprompt		= "old,dig,Vector";
    vectfile->description	= "vector file";

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

    if (G_parser (argc, argv))
	exit(-1);

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
	fprintf (stdout,"\n");
	fprintf (stdout,"v.support has not been run.  \n");
	fprintf (stdout,"\n");
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
    for (i = 1 ; i <= Map.n_areas ; i++)
    {
	if (0 != Map.Area[i].att)
	    continue;
	ret = Vect_get_point_in_area (&Map, i, &X, &Y);

	if (ret<0)
	{
	    fprintf (stderr, "****  Could not label area %d\n", i); 
	    continue;
	}

	write_att (afp, 'A', X, Y, label);
	if (incr->answer)
	    label++;
	cnt++;
    }


    fclose (afp);

    Vect_close (&Map);

    fprintf (stderr, "Labeled %d new areas.\n\n", cnt);

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
