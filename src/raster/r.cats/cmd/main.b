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
    char command[512];
    FILE *fd, *popen();
    long x, y;
    int i;

    struct Option *opt1, *opt2, *opt3 ;

    opt1 = G_define_option() ;
    opt1->key        = "map";
    opt1->type       = STRING;
    opt1->required   = YES;
    opt1->description= "Name of a raster map" ;

    opt2 = G_define_option() ;
    opt2->key        = "fs" ;
    opt2->key_desc   = "character|space|tab";
    opt2->type       = STRING ;
    opt2->required   = NO;
    opt2->description= "Output separator character" ;

    opt3 = G_define_option() ;
    opt3->key        = "cats" ;
    opt3->key_desc   = "range";
    opt3->type       = STRING ;
    opt3->multiple   = YES;
    opt3->required   = NO;
    opt3->description= "Category list: e.g. 1,3-8,13" ;


    G_gisinit (argv[0]);

    if (G_parser(argc, argv))
	exit(-1);

    name = opt1->answer;

    if (opt2->answer != NULL)
    {
	if (strcmp(opt2->answer, "space") == 0)
	    fs = ' ';
	else if (strcmp(opt2->answer, "tab") == 0)
	    fs = '\t';
        else
	    fs= opt2->answer[0];
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
    if (opt3->answer == NULL)
    {
        sprintf (command, "r.describe -1 'map=%s in %s'", name, mapset);
        fd = popen (command, "r");
        while (fscanf (fd, "%ld", &x) == 1)
            print_label (x);
        pclose (fd);
    }
    else
    {
        for (i = 0; opt3->answers[i]; i++)
            if (!scan_cats (opt3->answers[i], &x, &y))
	    {
                G_usage();
		exit(1);
	    }
        for (i = 0; opt3->answers[i]; i++)
        {
            scan_cats (opt3->answers[i], &x, &y);
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
