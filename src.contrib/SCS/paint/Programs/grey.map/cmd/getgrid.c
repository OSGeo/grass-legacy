#include "gis.h"
#include "parms.h"
#include "misc.h"

#define KEY(x) (strcmp(x,key)==0)

static char *help[]=
{
    "color     color",
    "numbers   # [color]",
    ""
};

getgrid ()
{
    int r,g,b;
    int spacing;
    int color;
    char temp[30];
    char buf[1024];
    char *key, *data;

    parms.grid_color = BLACK;
    parms.grid_numbers = 0;

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("color"))
	{
	    if (!scan_color (data, &color,&r,&g,&b))
		error (key,data,"illegal color request");
	    else
		parms.grid_color = color;
	    continue;
	}

	if (KEY("numbers"))
	{
	    spacing = -1;
	    switch (sscanf (data, "%d %[^\n]", &spacing, temp))
	    {
	    case 1: color = BLACK; break;
	    case 2: if (!scan_color (temp, &color,&r,&g,&b))
			spacing = -1;
		    break;
	    }
	    if (spacing < 0)
		error (key,data,"illegal numbers request");
	    else
	    {
		parms.grid_numbers = spacing;
		parms.grid_numbers_color = color;
	    }
	    continue;
	}

	error (key,data,"illegal request");
    }
}
