/* Function: getgrid
**
** Author: Paul W. Carlson	May 1992
*/

#include "ps_info.h"

#define KEY(x) (strcmp(x,key)==0)

static char *help[]=
{
    "font       fontname",
    "fontsize   fontsize",
    "color      color",
    "numbers    # [color]",
    ""
};

getgrid()
{
    int spacing;
    int color, fontsize;
    char temp[30];
    char buf[1024];
    char *key, *data;

    PS.grid_font = G_store("Helvetica");
    PS.grid_fontsize = 0;
    PS.grid_color = BLACK;
    PS.grid_numbers = 0;

    while (input(2, buf, help))
    {
	if (!key_data (buf, &key, &data)) continue;

	if (KEY("color"))
	{
	    color = get_color_number(data);
	    if (color < 0) error(key, data, "illegal color request");
	    else PS.grid_color = color;
	    continue;
	}

	if (KEY("numbers"))
	{
	    spacing = -1;
	    switch (sscanf(data, "%d %[^\n]", &spacing, temp))
	    {
	    	case 1: color = BLACK; 
			break;
	    	case 2: color = get_color_number(temp);
			if (color < 0) spacing = -1;
		        break;
	    }
	    if (spacing < 0) error(key, data, "illegal numbers request");
	    else
	    {
		PS.grid_numbers = spacing;
		PS.grid_numbers_color = color;
	    }
	    continue;
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
	    PS.grid_font = G_store(data);
	    continue;
	}
	error(key, data, "illegal request");
    }

    PS.grid_fontsize = fontsize;
}
