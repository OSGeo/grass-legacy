/* Function: sitefile
**
** Author: Paul W. Carlson	May 1992
*/

#include "sites.h"
#include "ps_info.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color color",
    "icon  iconfile",
    "size  #",
    "font  fontname",
    "desc  [y|n]",
    ""
};

sitefile(name, mapset)
char *name;
char *mapset;
{
    char fullname[100];
    char buf[1024];
    char *key, *data;
    double size;
    int color;

    sprintf(fullname, "%s in %s", name, mapset);

    if (site.count >= MAXSITES)
    {
	error(fullname, "", "no more site lists allowed");
	gobble_input();
	return 0;
    }

    site.name[site.count] = G_store(name);
    site.mapset[site.count] = G_store(mapset);

    site.color[site.count] = BLACK;
    site.icon[site.count] = G_store("default");
    site.font[site.count] = G_store("Helvetica");
    site.size[site.count] = 1.0;
    site.with_text[site.count] = 0;

    while (input(2, buf, help))
    {
	if (!key_data(buf, &key, &data))
	    continue;

	if (KEY("desc"))
	{
	    site.with_text[site.count] = yesno(key, data) ;
	    continue;
	}

	if (KEY("icon"))
	{
	    char name[50], mapset[50];

	    if (scan_gis("ps_icons", "icon", key, data, name, mapset, 0))
		site.icon[site.count] = G_store(name);
	    else 
	    {
		site.icon[site.count] = G_store("default");
		error(data, "", "can't read icon");
	    }
	    continue;
	}

	if (KEY("size"))
	{
	    if (sscanf(data, "%lf", &size) != 1 || size <= 0.0)
	    {
		size = 1.0;
		error(key, data, "illegal size request");
	    }
	    site.size[site.count] = size;
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
	    site.color[site.count] = color;
	    continue;
	}

	if (KEY("font"))
	{
	    get_font(data);
	    site.font[site.count] = G_store(data);
	    continue;
	}
	error(key, "", "illegal sites request");
    }

    site.count++;
    return 1;
}
