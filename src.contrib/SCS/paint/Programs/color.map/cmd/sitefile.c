#include "gis.h"
#include "sites.h"
#include "misc.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color color",
    "icon  iconfile",
    "size  #",
    "desc  [y|n]",
    ""
};

sitefile (name,mapset)
    char *name;
    char *mapset;
{
    char fullname[100];
    char buf[1024];
    char *key, *data;
    ICON icon;
    float size;
    int color;
    int r,g,b;


    sprintf (fullname, "%s in %s", name, mapset);

    if (site.count >= MAXSITES)
    {
	error (fullname,"","no more site lists allowed");
	gobble_input();
	return 0;
    }

    site.name[site.count] = G_store (name);
    site.mapset[site.count] = G_store (mapset);

    site.color[site.count] = BLACK;
    get_default_icon (&icon);
    size = 1.0;
    site.with_text[site.count] = 0 ;

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("desc"))
	{
	    site.with_text[site.count] = yesno(key,data) ;
	    continue;
	}

	if (KEY("icon"))
	{
	    char name[50], mapset[50];
	    if (scan_gis("icons","icon",key,data,name,mapset,0))
	    {
		release_icon (&icon);
		if (get_icon (name, mapset, &icon) <= 0)
		{
		    get_default_icon (&icon);
		    error (data,"","can't read icon");
		}
	    }
	    continue;
	}

	if (KEY("size"))
	{
	    if (sscanf (data,"%f", &size) != 1 || size <= 0.0)
	    {
		size = 1.0;
		error (key, data, "illegal size request");
	    }
	    continue;
	}

	if (KEY("color"))
	{
	    if (!scan_color (data, &color, &r, &g, &b))
	    {
		color = BLACK;
		error (key,data,"illegal color request");
	    }
	    site.color[site.count] = color;
	    continue;
	}

	error (key,"","illegal sites request");
    }

    scale_icon (&icon, &site.icon[site.count], size);
    release_icon (&icon);
    site.count++;
    return 1;
}
