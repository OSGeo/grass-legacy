#include <unistd.h>
#include "gis.h"
#include "site.h"
#include "report.h"
#include "local_proto.h"

int process (FILE *meta_report, SITE_LIST *site_list,
    FILE *layer_list, int quadsize, int with_stats)
{
    char layer[100];
    char mapset[100];
    int n;
    int r, c;
    double north, east;
    char *desc;
    struct Cell_head window;
    char buff1[50], buff2[50];

    G_get_window (&window);

    fprintf (meta_report,"location|%s|%s\n", G_location(), G_myname());
    fprintf (meta_report,"mapset|%s\n", G_mapset());
    fprintf (meta_report,"north|%s\n",format_north(window.north, buff1, -1));
    fprintf (meta_report,"south|%s\n",format_north(window.south, buff1, -1));
    fprintf (meta_report,"west|%s\n",format_east(window.west, buff1, -1));
    fprintf (meta_report,"east|%s\n",format_east(window.east, buff1, -1));
    fprintf (meta_report,"nsres|%s\n",format_res(window.ns_res, buff1, -1));
    fprintf (meta_report,"ewres|%s\n",format_res(window.ew_res, buff1, -1));

/* print the site list */

    fprintf(meta_report,"site list|%s|%s\n",
	    site_list->name, site_list->desc);

    fprintf(meta_report,"\n# point <site reference> <easting> <northing> <site description>\n\n");

    rewind_site_list (site_list);
    for (n = 1; next_site (site_list, &north, &east, &desc); n++)
	    fprintf(meta_report,"point|%d|%s|%s|%s\n",
		    n, format_east(east, buff1, -1), 
                    format_north(north, buff2, -1), desc);

/* print layer names */

    fprintf(meta_report,"\n# layer <layer reference> <layer name> <full name>\n\n");

    for (n = 1; fscanf (layer_list, "%s %s", layer, mapset) == 2; n++)
    {
	fprintf(meta_report,"layer|%d|%s|%s\n", n, layer,
		G_get_cell_title (layer, mapset));
    }

/* do layer stats next */

    new_report_screen();
    fprintf (stdout,"data extraction phase 1 - category info");
    if (with_stats)
	    fprintf (stdout," (with stats)");
    fprintf (stdout,"\n\n");

    fprintf(meta_report, "\n# cat <layer reference> <category> <cell count> <category name>\n\n");

    fseek (layer_list, 0L, 0);
    for (n = 1; fscanf (layer_list, "%s %s", layer, mapset) == 2; n++)
	    layer_stats (meta_report, layer, mapset, n, with_stats);
    fprintf (stdout,"\ncomplete\n");
    sleep (2) ;

/* do site data next */

    new_report_screen();
    fprintf (stdout,"data extraction phase 2 - site data (%d sites)\n\n",
	    count_sites(site_list,0));


    fprintf(meta_report,"\n# matrix { <east displacement> <south displacement> } ...\n\n");

    n = quadsize*2 + 1;
    fprintf(meta_report,"matrix size|%d\n\n", n*n);

    for (r = -quadsize; r <= quadsize; r++)
    {
	fprintf(meta_report,"matrix");
	for (c = -quadsize; c <= quadsize; c++)
	    fprintf(meta_report,"|%d|%d", c, r);
	fprintf(meta_report,"\n");
    }

    fprintf(meta_report,"\n# data <layer reference> <site reference> <category> ...\n\n");

    fseek (layer_list, 0L, 0);
    for (n = 1; fscanf (layer_list, "%s %s", layer, mapset) == 2; n++)
	site_data (meta_report, site_list, layer, mapset, n, &window, quadsize);
    
    fprintf (stdout,"\ncomplete\n");
    sleep (3);

    return 0;
}
