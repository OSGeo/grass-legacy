/****************************************************************************
 *
 * MODULE:       r.cats
 *
 * AUTHOR(S):    Michael Shapiro - CERL
 *
 * PURPOSE:      Prints category values and labels associated with
 *		 user-specified raster map layers.
 *
 * COPYRIGHT:    (C) 2006 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 ***************************************************************************/

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <grass/gis.h>
#include <grass/glocale.h>
#include "local_proto.h"

static char fs = '\t';
static struct Categories cats;


int 
main (int argc, char *argv[])
{
    char *name;
    char *mapset;
    long x, y;
    double dx;
    RASTER_MAP_TYPE map_type;
    int i;
	struct GModule *module;

    struct
    {
	struct Option *raster, *fs, *cats, *vals;
    } parm;

    G_gisinit (argv[0]);

    module = G_define_module();
    module->keywords = _("raster");
    module->description =
      _("Prints category values and labels associated "
	"with user-specified raster map layers.");

    parm.raster = G_define_standard_option(G_OPT_R_MAP);

    parm.vals = G_define_option() ;
    parm.vals->key        = "vals" ;
    parm.vals->key_desc   = "value";
    parm.vals->type       = TYPE_STRING ;
    parm.vals->multiple   = YES;
    parm.vals->required   = NO;
    parm.vals->description= _("Comma separated value list: e.g. 1.4,3.8,13") ;

    parm.cats = G_define_option() ;
    parm.cats->key        = "cats" ;
    parm.cats->key_desc   = "range";
    parm.cats->type       = TYPE_STRING ;
    parm.cats->multiple   = YES;
    parm.cats->required   = NO;
    parm.cats->description= _("Category list: e.g. 1,3-8,13") ;

    parm.fs = G_define_option() ;
    parm.fs->key        = "fs" ;
    parm.fs->key_desc   = "character|space|tab";
    parm.fs->type       = TYPE_STRING ;
    parm.fs->required   = NO;
    parm.fs->description= _("Output separator character (default: tab)") ;

    if (G_parser(argc, argv))
	exit(EXIT_FAILURE);

    name = parm.raster->answer;

    /* see v.in.ascii for a better solution */
    if (parm.fs->answer != NULL)
    {
	if (strcmp(parm.fs->answer, "space") == 0)
	    fs = ' ';
	else if (strcmp(parm.fs->answer, "tab") == 0)
	    fs = '\t';
	else if (strcmp(parm.fs->answer, "\\t") == 0)
	    fs = '\t';
        else
	    fs= parm.fs->answer[0];
    }
 
    mapset = G_find_cell2 (name,"");
    if (mapset == NULL)
        G_fatal_error (_("Raster map <%s> not found"),
		       name);
    if (G_read_cats (name, mapset, &cats) < 0)
        G_fatal_error (_("Cannot read category file of raster map <%s> in <%s>"),
		       name, mapset);

    map_type = G_raster_map_type(name, mapset);
/* if no cats requested, use r.describe to get the cats */
    if (parm.cats->answer == NULL)
    {
	if(map_type==CELL_TYPE)
	{
           get_cats (name, mapset);
           while (next_cat (&x))
           print_label (x);
           exit(EXIT_SUCCESS);
	}
    }
    else
    {
	if(map_type!=CELL_TYPE)
	   G_warning( _("The map is floating point! Ignoring cats list, using vals list"));
	else /* integer map */
	{
           for (i = 0; parm.cats->answers[i]; i++)
               if (!scan_cats (parm.cats->answers[i], &x, &y))
	       {
                   G_usage();
		   exit(EXIT_FAILURE);
	       }
           for (i = 0; parm.cats->answers[i]; i++)
           {
               scan_cats (parm.cats->answers[i], &x, &y);
               while (x <= y)
                   print_label (x++);
           }
	   exit(EXIT_SUCCESS);
        }
    }
    if(parm.vals->answer == NULL)
        G_fatal_error( _("vals argument is required for floating point map!"));
    for (i = 0; parm.vals->answers[i]; i++)
       if (!scan_vals (parm.vals->answers[i], &dx))
       {
            G_usage();
            exit(EXIT_FAILURE);
       }
    for (i = 0; parm.vals->answers[i]; i++)
    {
       scan_vals (parm.vals->answers[i], &dx);
       print_d_label (dx);
    }
    exit(EXIT_SUCCESS);
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
