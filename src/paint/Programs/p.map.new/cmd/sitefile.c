#include "gis.h"
#include "sites.h"
#include "misc.h"
#include "local_proto.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color color",
    "icon  iconfile",
    "size  #",
    "placement # #",
    "textcolor #",
    "textsize  #",
    "desc  [y|n]",
    ""
};

int 
sitefile (char *name, char *mapset)
{
    char fullname[100];
    char buf[1024];
    char *key, *data;
    ICON icon;
    float size;
    char north[50], east[50]; 
    double e, n;
    float textsize ;
    int textcolor;
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
    site.textcolor[site.count] = BLACK;
    get_default_icon (&icon);
    size = 1.0;
    site.with_text[site.count] = 0 ;
    site.north[site.count] = -1;
    site.east[site.count] = -1;
    site.textsize[site.count] = -1;

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

	if (KEY("placement"))
	{
	   if (sscanf (data, "%s %s", east, north)==2
		&& (scan_easting (east, &e) &&
		   (scan_northing (north, &n))) )
	   {
		site.north[site.count] = n;
		site.east[site.count] = e;

	   }
	   else
	   {
		site.north[site.count] = -1;
		site.east[site.count] = -1;
	   }
	   continue;
	}

	if (KEY("textcolor"))
	{
	    if (!scan_color (data, &textcolor, &r, &g, &b))
	    {
		textcolor = BLACK;
		error (key,data,"illegal color request");
	    }
	    site.textcolor[site.count] = textcolor;
	    continue;
	}
	if (KEY("textsize"))
	{
	    if (sscanf (data,"%f", &textsize) != 1 || size <= 0.0)
	    {
		textsize = 1.0;
		error (key, data, "illegal textsize request");
	    }
	    else
		site.textsize[site.count] = textsize;
	    continue;
	}



	error (key,"","illegal sites request");
    }

    scale_icon (&icon, &site.icon[site.count], size);
    release_icon (&icon);
    site.count++;
    return 1;
}
