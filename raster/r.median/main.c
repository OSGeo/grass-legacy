#include <unistd.h>
#include <string.h>
#include "gis.h"
#include "stats.h"
#include "local_proto.h"

int main (int argc, char *argv[])
{
	struct GModule *module;
    struct {
	struct Option *base, *cover, *output;
    } parm;
    char *basemap, *base_mapset;
    char *covermap, *cover_mapset;
    char *outmap;
    char command[1024];
    struct Categories cover_cats;
    FILE *stats_fd, *reclass_fd;
    int first;
    long basecat, covercat, catb, catc;
    double area;
    struct stats stats;

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Finds the median of values in a cover map within "
		"areas assigned the same category value in a "
		"user-specified base map.";

    parm.base = G_define_option();
    parm.base->key = "base";
    parm.base->description = "base raster map";
    parm.base->required = YES;
    parm.base->type = TYPE_STRING;
    parm.base->gisprompt = "old,cell,raster";

    parm.cover = G_define_option();
    parm.cover->key = "cover";
    parm.cover->description = "cover raster map";
    parm.cover->required = YES;
    parm.cover->type = TYPE_STRING;
    parm.cover->gisprompt = "old,cell,raster";

    parm.output = G_define_option();
    parm.output->key = "output";
    parm.output->description = "resultant raster map";
    parm.output->required = YES;
    parm.output->type = TYPE_STRING;
    parm.output->gisprompt = "new,cell,raster";

    if(G_parser(argc,argv))
	exit(1);
    
    basemap = parm.base->answer;
    covermap = parm.cover->answer;
    outmap = parm.output->answer;

    base_mapset = G_find_cell2 (basemap, "");
    if (base_mapset == NULL)
    {
	fprintf(stderr,"%s: base raster map not found\n", basemap);
	exit(1);
    }

    cover_mapset = G_find_cell2 (covermap, "");
    if (cover_mapset == NULL)
    {
	fprintf(stderr,"%s: cover raster map not found\n", covermap);
	exit(1);
    }
    if (G_legal_filename(outmap) < 0)
    {
	fprintf(stderr,"%s: illegal map name\n", outmap);
	exit(1);
    }
    if (strcmp(G_mapset(),base_mapset)==0 && strcmp(basemap, outmap) == 0)
    {
	fprintf(stderr,"%s: base map and output map must be different\n",
		outmap);
	exit(1);
    }
    if (G_read_cats (covermap, cover_mapset, &cover_cats) < 0)
    {
	fprintf (stderr, "%s: can't read category labels\n", covermap);
	exit(1);
    }

    strcpy (command, "r.stats -a '");
    strcat (command, G_fully_qualified_name (basemap, base_mapset));
    strcat (command, ",");
    strcat (command, G_fully_qualified_name (covermap, cover_mapset));
    strcat (command, "'");

/* strcpy (command,"cat /tmp/t"); */
    stats_fd = popen (command, "r");

    sprintf (command, "r.reclass i='%s' o='%s'",
	G_fully_qualified_name (basemap, base_mapset), outmap);

    reclass_fd = popen (command, "w");

    first = 1;
    while (read_stats(stats_fd, &basecat, &covercat, &area))
    {
	if (first)
	{
	    stats.n = 0;
	    stats.nalloc = 16;
	    stats.cat = (long *) 
		G_calloc (stats.nalloc, sizeof(long));
	    stats.area = (double *) 
		G_calloc (stats.nalloc, sizeof(double));
	    first = 0;
	    catb = basecat;
	}
	if (basecat != catb)
	{
	    catc = median (&stats);
	    write_reclass (reclass_fd, catb, catc, G_get_cat (catc, &cover_cats));
	    catb = basecat;
	    stats.n = 0;
	}
	stats.n++;
	if (stats.n > stats.nalloc)
	{
	    stats.nalloc *= 2;
	    stats.cat = (long *)
		G_realloc (stats.cat, stats.nalloc * sizeof(long));
	    stats.area = (double *)
		G_realloc (stats.area, stats.nalloc * sizeof(double));
	}
	stats.cat[stats.n-1] = covercat;
	stats.area[stats.n-1] = area;
    }
    if (!first)
    {
	catc = median (&stats);
	write_reclass (reclass_fd, catb, catc, G_get_cat (catc, &cover_cats));
    }

    pclose (stats_fd);
    pclose (reclass_fd);

    exit(0);
}
