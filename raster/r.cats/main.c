#include <stdio.h>
#include "gis.h"
#include "local_proto.h"

static char fs = '\t';
static struct Categories cats;

int 
main (int argc, char *argv[])
{
    char *name;
    char *mapset;
    char msg[100];
    long x, y;
    double dx;
    RASTER_MAP_TYPE map_type;
    int i;
	struct GModule *module;

    struct
    {
	struct Option *raster, *fs, *cats, *vals;
    } parm;

	module = G_define_module();
	module->description =
		"Prints category values and labels associated "
		"with user-specified raster map layers.";

    parm.raster = G_define_option() ;
    parm.raster->key        = "map";
    parm.raster->type       = TYPE_STRING;
    parm.raster->required   = YES;
	parm.raster->gisprompt  = "old,cell,raster" ;
    parm.raster->description= "Name of a raster map" ;

    parm.vals = G_define_option() ;
    parm.vals->key        = "vals" ;
    parm.vals->key_desc   = "value";
    parm.vals->type       = TYPE_STRING ;
    parm.vals->multiple   = YES;
    parm.vals->required   = NO;
    parm.vals->description= "Comma separated value list: e.g. 1.4,3.8,13" ;

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

    map_type = G_raster_map_type(name, mapset);
/* if no cats requested, use r.describe to get the cats */
    if (parm.cats->answer == NULL)
    {
	if(map_type==CELL_TYPE)
	{
           get_cats (name, mapset);
           while (next_cat (&x))
           print_label (x);
           exit(0);
	}
    }
    else
    {
	if(map_type!=CELL_TYPE)
	   G_warning("The map is floating point! Ignoring cats list, using vals list");
	else /* integer map */
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
	   exit(0);
        }
    }
    if(parm.vals->answer == NULL)
        G_fatal_error("vals argument is required for floating point map!");
    for (i = 0; parm.vals->answers[i]; i++)
       if (!scan_vals (parm.vals->answers[i], &dx))
       {
            G_usage();
            exit(1);
       }
    for (i = 0; parm.vals->answers[i]; i++)
    {
       scan_vals (parm.vals->answers[i], &dx);
       print_d_label (dx);
    }
    exit(0);
}

int 
print_label (long x)
{
    char *label;

    G_squeeze(label = G_get_cat ((CELL)x, &cats));
    fprintf (stdout,"%ld%c%s\n", x, fs, label);

    return 0;
}

int 
print_d_label (double x)
{
    char *label, tmp[40];
    DCELL dtmp;

    dtmp = x;
    G_squeeze(label = G_get_d_raster_cat(&dtmp, &cats));
    sprintf(tmp, "%.10f", x);
    G_trim_decimal(tmp);
    fprintf (stdout,"%s%c%s\n", tmp, fs, label);

    return 0;
}

int 
scan_cats (char *s, long *x, long *y)
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

int 
scan_vals (char *s, double *x)
{
    char dummy[10];
    *dummy=0;
    if (sscanf (s, "%lf%1s", x, dummy) == 1 && *dummy == 0)
	return 1;
    return 0;
}
