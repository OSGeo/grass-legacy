/* Function: vectfile
**
** This PostScript version is just slightly modified p.map code.
**
** Modified by: Paul W. Carlson		March 1992
** Modified by: Janne Soimasuo August 1994 line_cat added
** Modified by: Radim Blazek Jan 2000 acolor, label added
*/
#include "vector.h"
#include "Vect.h"
#include "ps_info.h"
#include "local_proto.h"

#define KEY(x) (strcmp(key,x)==0)

static char *help[]=
{
    "color    color",
    "width    #",
    "cwidth   #",
    "hcolor   color",
    "hwidth   #",
    "offset   #",    
    "coffset   #",
    "masked   [y|n]",
    "style    solid|[0-9]...",
    "line_cat #",
    "acolor   r g b",
    "label    label",
    "lpos     0|1-20",
    "ref      left|right",
    ""
};

int 
vectfile (char *name, char *mapset)
{
    char fullname[100];
    char buf[1024];
    char temp[100];
    char *key, *data, *dp;
    double  width;
    int line_cat, itmp;
    int color, r, g, b;
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
    vector.width[vector.count]  = 1. ;
    vector.cwidth[vector.count]  = 0. ;    
    vector.offset[vector.count]  = 0. ;
    vector.coffset[vector.count]  = 0. ;            
    for (i = 0; i < 9; i++) vector.colors[vector.count][i] = BLACK;
    vector.linestyle[vector.count] = NULL;
    vector.ref[vector.count] = LINE_REF_CENTER;    
    vector.hwidth[vector.count] = 0. ;
    vector.hcolor[vector.count] = WHITE;
    vector.line_cat[vector.count] = -1 ;
    vector.area[vector.count] = 0 ;    
    vector.label[vector.count] = NULL ;    
    vector.lpos[vector.count] = -1 ;

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
	    if (vector.masked[vector.count]) PS.mask_needed = 1;
	    continue;
	}

	if (KEY("width"))
	{
	    width = -1.;
	    *mapset = 0;
	    if (sscanf(data, "%lf%s", &width, mapset) < 1 || width < 0.)
	    {
		width = 1.;
		error(key, data, "illegal width");
		continue;
	    }
	    if(mapset[0] == 'i') width = width/72.;
	    vector.width[vector.count] = width;
	    continue;
	}
	
	if (KEY("cwidth"))
	{
	    width = -1.;
	    *mapset = 0;
	    if (sscanf(data, "%lf%s", &width, mapset) < 1 || width < 0.)
	    {
		width = 1.;
		error(key, data, "illegal cwidth");
		continue;
	    }
	    if(mapset[0] == 'i') width = width/72.;
	    vector.cwidth[vector.count] = width;
	    continue;
	}	
	
	if (KEY("offset"))
	{
	    *mapset = 0;
	    if (sscanf(data, "%lf%s", &width, mapset) < 1 )
	    {
		width = 0.;
		error(key, data, "illegal offset");
		continue;
	    }
	    if(mapset[0] == 'i') width = width/72.;
	    vector.offset[vector.count] = width;
	    continue;
	}		
	
	if (KEY("coffset"))
	{
	    *mapset = 0;
	    if (sscanf(data, "%lf%s", &width, mapset) < 1 )
	    {
		width = 0.;
		error(key, data, "illegal coffset");
		continue;
	    }
	    if(mapset[0] == 'i') width = width/72.;
	    vector.coffset[vector.count] = width;
	    continue;
	}			

	if (KEY("hwidth"))
	{
	    width = -1.;
	    if (sscanf(data, "%lf%s", &width, mapset) < 1 || width < 0.)
	    {
		width = 0.;
		error(key, data, "illegal hwidth");
		continue;
	    }
	    if(mapset[0] == 'i') width = width/72.;
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
	/*  if (!vector.hwidth[vector.count]) vector.hwidth[vector.count] = 1;*/
	    continue;
	}
        if (KEY("line_cat"))
        {
            if (sscanf(data, "%d", &line_cat) < 1 )
            {
                line_cat = -1;
                error(key, data, "illegal line_cat(egory)");
                continue;
            }
            vector.line_cat[vector.count] = line_cat;
	    continue;
         }
        if (KEY("acolor")) /* area color */
        {
            if (sscanf(data, "%d %d %d", &r, &g, &b) < 3 )
            {
                error(key, data, "illegal color request");
                continue;
            }
	    vector.area[vector.count] = 1 ;    	    
            vector.acolor[vector.count].r = r;
	    vector.acolor[vector.count].g = g;
	    vector.acolor[vector.count].b = b;
	    continue;
         }
	if (KEY("label")) /* map legend label */
	{
	    G_strip(data);
	    vector.label[vector.count] = G_store(data);
	    continue;
	}
        if (KEY("lpos"))
        {
            if (sscanf(data, "%d", &itmp) < 1 || itmp < 0 )
            {
                itmp = -1;
                error(key, data, "illegal lpos");
                continue;
            }
            vector.lpos[vector.count] = itmp;
	    continue;
         }
	if (KEY("ref"))
	{
	    G_strip(data);
	    if (strcmp(data, "left") == 0)
	    {
		vector.ref[vector.count] = LINE_REF_LEFT;
		continue;
	    }
	    if (strcmp(data, "right") == 0)
	    {
		vector.ref[vector.count] = LINE_REF_RIGHT;
		continue;
	    }
	    error(key, data, "illegal ref request");
	    continue;
	}

	error(key, "", "illegal request");
    }

    vector.count++;
    return 1;
}
