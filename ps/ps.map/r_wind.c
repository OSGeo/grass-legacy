#include "ps_info.h"
#include <string.h>
#include "local_proto.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color  color",
    "width  #",
    ""
};

int 
read_wind (char *name, char *mapset)
{
    char fullname[100];
    char buf[1024];
    char *key, *data;
    double width;
    int color;
    int i;
    double east, west, incr;
    struct Cell_head window;

    sprintf (fullname, "%s in %s", name, mapset);

    if (G__get_window (&window, "windows", name, mapset) == NULL )
    {
	error (fullname,"","can't read region definition file");
	gobble_input();
	return 0;
    }

    width  = 1. ;
    color  = BLACK;

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("width"))
	{
	    width = -1.;
	    *mapset = 0;
	    if (sscanf (data, "%lf%s", &width, mapset) < 1 || width < 0.)
	    {
		width = 1.;
		error (key,data,"illegal width");
	    }
	    if(mapset[0] == 'i') width = width/72.0;
	    continue;
	}


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
	sprintf (buf, "L 0 %f %f %f %f %d %.8f",
	    west, window.north, east, window.north,
	    color, width);
	add_to_plfile (buf);

	sprintf (buf, "L 0 %f %f %f %f %d %.8f",
	    west, window.south, east, window.south,
	    color, width);
	add_to_plfile (buf);

	west = east;
    }

    sprintf (buf, "L 0 %f %f %f %f %d %.8f",
	window.east, window.north, window.east, window.south,
	color, width);
    add_to_plfile (buf);

    sprintf (buf, "L 0 %f %f %f %f %d %.8f",
	window.west, window.north, window.west, window.south,
	color, width);
    add_to_plfile (buf);

    return 1;
}
