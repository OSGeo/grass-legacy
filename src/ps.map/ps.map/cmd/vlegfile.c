/* Function: vlegfile
**
** Author: Paul W. Carlson	April 1992
*/

#include "ps_info.h"
#include "vector.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[] =
{
    "where      x y",
    "font       fontname",
    "fontsize   fontsize",
    ""
};

vlegfile()
{	
    char buf[1024];
    char *key, *data;
    int fontsize;
    double x, y;

    fontsize = 0;
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

	if (KEY("fontsize"))
	{
	    fontsize = atoi(data);
	    if (fontsize < 4 || fontsize > 50) fontsize = 0;
	    continue;
	}

	if (KEY("font"))
	{
	    get_font(data);
	    vector.font = G_store(data);
	    continue;
	}
	error(key, data, "illegal vlegend sub-request");
    }
    vector.x = x;
    vector.y = y;
    if (fontsize) vector.fontsize = fontsize;
}
