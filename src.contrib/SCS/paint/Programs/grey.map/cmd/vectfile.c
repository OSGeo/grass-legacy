#include "gis.h"
#include "vector.h"
#include "misc.h"
#include "Vect.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color   color",
    "width   #",
    "hcolor  color",
    "hwidth  #",
    "masked  [y|n]",
    "style   solid|[0-9]...",
    ""
};

vectfile (name,mapset)
    char *name;
    char *mapset;
{
    char fullname[100];
    char buf[1024];
    char temp[100];
    char *key, *data, *dp;
    int width;
    int color;
    int r,g,b;
    int i;
    struct Map_info Map;

    sprintf (fullname, "%s in %s", name, mapset);

    if (vector.count >= MAXVECTORS)
    {
	error (fullname,"","no more vector files allowed");
	gobble_input();
	return 0;
    }

    Vect_set_open_level (1);
    if (1 != Vect_open_old (&Map, name, mapset))
    {
	error (fullname,"","can't open vector file");
	gobble_input();
	return 0;
    }
    Vect_close (&Map);

    vector.name[vector.count]   = G_store (name);
    vector.mapset[vector.count] = G_store (mapset);
    vector.masked[vector.count] = 0 ;
    vector.width[vector.count]  = 1 ;
    for (i = 0; i < 9; i++)
	vector.colors[vector.count][i] = BLACK;
    vector.linestyle[vector.count] = NULL;
    vector.hwidth[vector.count] = 0 ;
    vector.hcolor[vector.count] = WHITE;

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("style"))
	{
	    G_strip (data);
	    if (strcmp (data, "solid") == 0)
	    {
		vector.linestyle[vector.count] = NULL;
		continue;
	    }
	    for (dp = data; *dp; dp++)
		if (*dp < '0' || *dp > '9')
		    break;
	    if (*dp != 0 || dp == data)
	    {
		error (key,data,"illegal line style");
		continue;
	    }
	    vector.linestyle[vector.count] = G_store (data);
	    continue;
	}

	if (KEY("masked"))
	{
	    vector.masked[vector.count] = yesno (key,data) ;
	    continue;
	}

	if (KEY("width"))
	{
	    width = -1;
	    if (sscanf (data, "%d%s", &width, mapset) != 1 || width < 1)
	    {
		error (key,data,"illegal width");
		continue;
	    }
	    vector.width[vector.count] = width;
	    continue;
	}

	if (KEY("hwidth"))
	{
	    width = -1;
	    if (sscanf (data, "%d%s", &width, mapset) != 1 || width < 1)
	    {
		error (key,data,"illegal hwidth");
		continue;
	    }
	    vector.hwidth[vector.count] = width;
	    continue;
	}

	if (KEY("color"))
	{
	    if (sscanf (data, "%d%[^\n]", &i, temp) == 2)
	    {
		if (i >= 1 && i <= 9 && scan_color (temp, &color, &r,&g,&b))
		{
		    vector.colors[vector.count][i-1] = color;
		    continue;
		}
	    }
	    if (!scan_color (data, &color, &r,&g,&b))
	    {
		error (key,data,"illegal color request");
		continue;
	    }
	    for (i=0; i < 9; i++)
		vector.colors[vector.count][i] = color;
	    continue;
	}

	if (KEY("hcolor"))
	{
	    if (!scan_color (data, &color, &r,&g,&b))
	    {
		error (key,data,"illegal color request");
		continue;
	    }
	    vector.hcolor[vector.count] = color;
	    if (!vector.hwidth[vector.count])
		vector.hwidth[vector.count] = 1;
	    continue;
	}

	error (key,"","illegal request");
    }

    vector.count++;
    return 1;
}
