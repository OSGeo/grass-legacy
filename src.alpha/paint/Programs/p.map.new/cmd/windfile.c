#include "gis.h"
#include "misc.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color  color",
    "width  #",
    ""
};

windfile (name,mapset)
    char *name;
    char *mapset;
{
    char fullname[100];
    char buf[1024];
    char *key, *data;
    int width;
    int color;
    int r,g,b;
    int i;
    double east, west, incr;
    struct Cell_head window;

    sprintf (fullname, "%s in %s", name, mapset);

    if (G__get_window (&window, "windows", name, mapset) < 0)
    {
	error (fullname,"","can't read region definition file");
	gobble_input();
	return 0;
    }

    width  = 1 ;
    color  = BLACK;

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("width"))
	{
	    width = -1;
	    if (sscanf (data, "%d%s", &width, mapset) != 1 || width < 1)
	    {
		width = 1;
		error (key,data,"illegal width");
	    }
	    continue;
	}

	if (KEY("color"))
	{
	    if (!scan_color (data, &color, &r,&g,&b))
	    {
		color = BLACK;
		error (key,data,"illegal color request");
	    }
	    continue;
	}

	error (key,"","illegal request");
    }

/* draw horizontal lines in 3 pieces - lat-lon lines must not
 * extend more than half the globe
 */
    west = window.west;
    incr = (window.east-window.west)/3;
    for (i = 0; i < 3; i++)
    {
	east = west+incr;
	sprintf (buf, "L 0 %lf %lf %lf %lf %d %d",
	    west, window.north, east, window.north,
	    color, width);
	add_to_plfile (buf);

	sprintf (buf, "L 0 %lf %lf %lf %lf %d %d",
	    west, window.south, east, window.south,
	    color, width);
	add_to_plfile (buf);

	west = east;
    }

    sprintf (buf, "L 0 %lf %lf %lf %lf %d %d",
	window.east, window.north, window.east, window.south,
	color, width);
    add_to_plfile (buf);

    sprintf (buf, "L 0 %lf %lf %lf %lf %d %d",
	window.west, window.north, window.west, window.south,
	color, width);
    add_to_plfile (buf);

    return 1;
}
