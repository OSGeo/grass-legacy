#include <stdio.h>
#include "gis.h"

static char fs = '\t';
static struct Categories cats;

main(argc, argv)
    char *argv[];
    int argc;
{
    char *name;
    char *mapset;
    char msg[100];
    long x, y;
    int i;

    struct
    {
	struct Option *raster, *fs, *cats;
    } parm;

    parm.raster = G_define_option() ;
    parm.raster->key        = "map";
    parm.raster->type       = TYPE_STRING;
    parm.raster->required   = YES;
	parm.raster->gisprompt  = "old,cell,raster" ;
    parm.raster->description= "Name of a raster map" ;

    parm.cats = G_define_option() ;
    parm.cats->key        = "cats" ;
    parm.cats->key_desc   = "range";
    parm.cats->type       = TYPE_STRING ;
    parm.cats->multiple   = YES;
    parm.cats->required   = NO;
    parm.cats->description= "Category list: e.g. 1,3-8,13" ;

    parm.fs = G_define_option() ;
    parm.fs->key        = "fs" ;
    parm.fs->key_desc   = "character|space|tab";
    parm.fs->type       = TYPE_STRING ;
    parm.fs->required   = NO;
    parm.fs->description= "Output separator character (default: tab)" ;


    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
	exit(-1);

    name = parm.raster->answer;

    if (parm.fs->answer != NULL)
    {
	if (strcmp(parm.fs->answer, "space") == 0)
	    fs = ' ';
	else if (strcmp(parm.fs->answer, "tab") == 0)
	    fs = '\t';
        else
	    fs= parm.fs->answer[0];
    }
 
    mapset = G_find_cell2 (name,"");
    if (mapset == NULL)
    {    
        sprintf(msg, "%s: <%s> raster file not found",G_program_name(),name);
        G_fatal_error (msg); 
        exit(1);
    }
    if (G_read_cats (name, mapset, &cats) < 0)
    {
        fprintf (stderr, "%s: %s in %s - can't read category file\n", 
	             G_program_name(), name, mapset);
        exit(1);
    }

/* if no cats requested, use r.describe to get the cats */
    if (parm.cats->answer == NULL)
    {
	get_cats (name, mapset);
        while (next_cat (&x))
            print_label (x);
    }
    else
    {
        for (i = 0; parm.cats->answers[i]; i++)
            if (!scan_cats (parm.cats->answers[i], &x, &y))
	    {
                G_usage();
		exit(1);
	    }
        for (i = 0; parm.cats->answers[i]; i++)
        {
            scan_cats (parm.cats->answers[i], &x, &y);
            while (x <= y)
                print_label (x++);
        }
    }
    exit(0);
}

print_label (x)
    long x;
{
    char *label, *G_get_cat();

    G_squeeze(label = G_get_cat ((CELL)x, &cats));
    printf ("%ld%c%s\n", x, fs, label);
}

scan_cats (s, x, y)
    char *s;
    long *x, *y;
{
    char dummy[2];

    *dummy = 0;
    if (sscanf (s, "%ld-%ld%1s", x, y, dummy) == 2)
	return (*dummy == 0 && *x <= *y);
    *dummy = 0;
    if (sscanf (s, "%ld%1s", x, dummy) == 1 && *dummy == 0)
    {
	*y = *x;
	return 1;
    }
    return 0;
}
