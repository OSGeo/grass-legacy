#include "gis.h"
#include "parms.h"
#include "misc.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color  color",
    ""
};

outlinefile()
{	
    char buf[1024];
    char *key, *data;
    int color;
    int r,g,b;

    color = BLACK;

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("color"))
	{
	    if (!scan_color (data, &color, &r,&g,&b))
	    {
		color = BLACK;
		error (key,data,"illegal color request");
	    }
	    continue;
	}
	error (key,data,"illegal outline sub-request");
    }

    parms.outline_color = color;
}
