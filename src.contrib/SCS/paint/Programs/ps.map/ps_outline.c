#include "Vect.h"
#include "ps_info.h"

extern int verbose;

/* the ps_outline function creates a vector file called "tmp.outl" in
** the current location.  This file is removed after it has been
** plotted.
*/

ps_outline()
{
    struct Map_info Map;


    /* let user know what's happenning */
    if (verbose > 1)
    {
        printf("PS-PAINT: outlining areas in raster file <%s in %s> ...",
	    PS.cell_name, PS.cell_mapset);
        fflush(stdout);
    }

    /* create temporary vector file containing outlines */
    o_io_init();
    o_open_file(PS.cell_name, "tmp.outl");
    o_extract_areas();
    o_re_map_areas();
    o_close_file();

    /* set the outline color */
    set_rgb_color(PS.outline_color);

    /* plot the temporary vector file, then delete it */
    Vect_open_old(&Map, "tmp.outl", PS.cell_mapset);
    PS_vector_plot(&Map);
    G_remove("dig", "tmp.outl");

    if (verbose > 1) printf("\n");
}


/* The outlinefile function is just slightly modified p.map code. */
#define KEY(x) (strcmp(key,x)==0)
static char *help[] =
{
    "color  color",
    ""
};
outlinefile()
{	
    char buf[1024];
    char *key, *data;
    int color;

    color = BLACK;
    while (input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;
	if (KEY("color"))
	{
	    color = get_color_number(data);
	    if (color < 0)
	    {
		color = BLACK;
		error(key, data, "illegal color request");
	    }
	    continue;
	}
	error(key, data, "illegal outline sub-request");
    }
    PS.outline_color = color;
    PS.do_outline = 1;
}
