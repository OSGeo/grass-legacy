/* Function: vectfile
**
** This PostScript version is just slightly modified p.map code.
**
** Modified by: Paul W. Carlson		March 1992
*/
#include "ps_info.h"
#include "vector.h"
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

vectfile(name, mapset)
char *name;
char *mapset;
{
    char fullname[100];
    char buf[1024];
    char temp[100];
    char *key, *data, *dp;
    int width;
    int color;
    int r, g, b;
    int i;
    int got_color;
    struct Map_info Map;

    sprintf (fullname, "%s in %s", name, mapset);

    if (vector.count >= MAXVECTORS)
    {
	error(fullname, "", "no more vector files allowed");
	gobble_input();
	return 0;
    }

    Vect_set_open_level(1);
    if (1 != Vect_open_old (&Map, name, mapset))
    {
	error(fullname, "", "can't open vector file");
	gobble_input();
	return 0;
    }
    Vect_close(&Map);

    vector.name[vector.count]   = G_store(name);
    vector.mapset[vector.count] = G_store(mapset);
    vector.masked[vector.count] = 0 ;
    vector.width[vector.count]  = 1 ;
    for (i = 0; i < 9; i++) vector.colors[vector.count][i] = BLACK;
    vector.linestyle[vector.count] = NULL;
    vector.hwidth[vector.count] = 0 ;
    vector.hcolor[vector.count] = WHITE;

    got_color = 0;
    while (input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;

	if (KEY("style"))
	{
	    G_strip(data);
	    if (strcmp(data, "solid") == 0)
	    {
		vector.linestyle[vector.count] = NULL;
		continue;
	    }
	    for (dp = data; *dp; dp++)
		if (*dp < '0' || *dp > '9') break;
	    if (*dp != 0 || dp == data)
	    {
		error(key, data, "illegal line style");
		continue;
	    }
	    vector.linestyle[vector.count] = G_store(data);
	    continue;
	}

	if (KEY("masked"))
	{
	    vector.masked[vector.count] = yesno(key,data) ;
	    continue;
	}

	if (KEY("width"))
	{
	    width = -1;
	    if (sscanf(data, "%d%s", &width, mapset) != 1 || width < 1)
	    {
		error(key, data, "illegal width");
		continue;
	    }
	    vector.width[vector.count] = width;
	    continue;
	}

	if (KEY("hwidth"))
	{
	    width = -1;
	    if (sscanf(data, "%d%s", &width, mapset) != 1 || width < 1)
	    {
		error(key, data, "illegal hwidth");
		continue;
	    }
	    vector.hwidth[vector.count] = width;
	    continue;
	}

	if (KEY("color"))
	{
	    /* NOTE: the PostScript version of p.map will use only 1 color
	    ** which will be the first one encounterered.  The number
	    ** preceding the color will be ignored.
	    */
	    if (got_color) continue;
	    if (sscanf(data, "%d%[^\n]", &i, temp) == 2)
	    {
		if (i >= 1 && i <= 9)
		{
		    color = get_color_number(temp);
		    if (color >= 0)
		    {
		    	vector.colors[vector.count][i-1] = color;
		    	got_color = 1;
		    }
		}
		continue;
	    }
	    color = get_color_number(data);
	    if (color < 0)
	    {
		error (key,data,"illegal color request");
		continue;
	    }
	    for (i = 0; i < 9; i++) vector.colors[vector.count][i] = color;
	    got_color = 1;
	    continue;
	}

	if (KEY("hcolor"))
	{
	    color = get_color_number(data);
	    if (color < 0)
	    {
		error (key,data,"illegal color request");
		continue;
	    }
	    vector.hcolor[vector.count] = color;
	    if (!vector.hwidth[vector.count]) vector.hwidth[vector.count] = 1;
	    continue;
	}

	error(key, "", "illegal request");
    }

    vector.count++;
    return 1;
}
