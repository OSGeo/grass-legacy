/* Function: infofile
**
** Author: Paul W. Carlson	April 1992
*/

#include "ps_info.h"
#include "map_info.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[] =
{
    "where x y",
    "font   fontname",
    "size   fontsize",
    "color  color",
    ""
};

infofile()
{	
    char buf[1024];
    char *key, *data;
    int color, size;
    double x, y;

    size = 0;
    color = BLACK;
    x = y = 0.0;
    while (input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;

        if (KEY("where"))
 	{
	    if (sscanf(data, "%lf %lf", &x, &y) != 2)
	    {
		x = y = 0.0;
		error(key, data, "illegal where request");
	    }
	    else continue;
	}

	if (KEY("size"))
	{
	    size = atoi(data);
	    if (size < 4 || size > 50) size = 0;
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

	if (KEY("font"))
	{
	    get_font(data);
	    m_info.font = G_store(data);
	    continue;
	}
	error(key, data, "illegal maploc sub-request");
    }
    m_info.x = x;
    m_info.y = y;
    m_info.color = color;
    if (size) m_info.size = size;
}
