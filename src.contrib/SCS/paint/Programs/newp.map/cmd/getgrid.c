
#include "gis.h"
#include "parms.h"
#include "grid.h"
#include "misc.h"

#define KEY(x) (strcmp(x,key)==0)
#define EQ(x,y) (strcmp(x,y)==0)

static char *help[]=
{
    "color     color",
    "numbers   # [color]",
	"width	   #",
	"hcolor	   color",
	"hwidth	   #",
	"gridon    [all|data|nodata]",
	"style	   solid|[0-9]",
	"pattern   [notick|tick # #]",
    ""
};

getgrid ()
{
    int r,g,b;
    int spacing;
	int i;
	int color;
	int width;
	int hwidth;
    char temp[30];
    char buf[1024];
	char t1[128], t2[128];
    char *key, *data, *dp;

    parms.grid_color = BLACK;
    parms.grid_numbers = 0;


	color 	= BLACK;
	width	= 1;
	hwidth 	= 0;
	grid.linestyle 	= NULL;

	for (i=0; i<9; i++)
	grid.colors[i]	= BLACK;

	grid.width		= 1;
	grid.hcolor		= WHITE;
	grid.hwidth	= 0;

	strcpy (grid.gridon, "all");
	strcpy (grid.pattern, "notick");

	grid.patternw	= 10;
	grid.patternh	= 10;
	

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;


	if (KEY("style"))
	{
		G_strip (data);
		if (strcmp (data, "solid") == NULL)
		{
		grid.linestyle	= NULL;
		continue;
		}
		for (dp = data; *dp; dp++)
		if (*dp < '0' || *dp > '9')
			break;
		if (*dp != 0 || dp == data) 
		{
			error (key, data, "illegal line style");
			continue;
		}
		grid.linestyle	= G_store(data);
		continue;
	}

	if (KEY("gridon"))
	{
	if (sscanf (data, "%s %s", t1, t2) != 1) {
		error (key, data, "illegal gridon input"); 
		continue;
		}
	
	if (EQ(t1,"all") || EQ(t1,"data") || EQ(t1, "nodata")) 
		strcpy (grid.gridon, t1);
	else
		error (key, data, "illegal gridon input"); 

	continue;
	}

	if (KEY("pattern"))
	{
	if (sscanf (data, "%s %s", t1, t2) == 1) {
	if (EQ(t1,"notick")) { 
		strcpy (grid.pattern, t1);
		continue;
	}}
	else {
	char t3[128];
	if (sscanf (data, "%s %s %s", t1, t2, t3) == 3) {
	if (EQ(t1,"tick") ) {
		strcpy (grid.pattern, t1);
		grid.patternw	= atoi(t2);
		grid.patternh	= atoi(t3);
		continue;
		}
		

	} }
		error (key, data, "illegal pattern input"); 
		continue;
	}
	



    if (KEY("color"))
	{
	if (sscanf (data, "%d%[^\n]", &i, temp) == 2)
	{
	if (i >= 1 && i <= 9 && scan_color (temp, &color, &r,&g,&b)) 
	{
	grid.colors[i-1] = color;
	parms.grid_color = color;
	continue;
	}
	}
	if (!scan_color (data, &color, &r,&g,&b)) {
	error (key,data,"illegal color request");
	continue;
	}
	for (i=0; i < 9; i++) 
		grid.colors[i] = color;
	continue;
	}

	if (KEY("width"))
	{
		width = -1 ;
		if (sscanf (data, "%d", &width) != 1 || width <1)		
		{
		error (key, data, "illegal width");
		continue;
		}
		grid.width	= width;
		continue;
	}




	if (KEY("hcolor"))
	{
	if (!scan_color (data, &color, &r,&g,&b)) {
	error (key,data,"illegal hcolor request");
	continue;
	}
	grid.hcolor = color;
	if (!grid.hwidth) grid.hwidth = 1;
	continue;
	}


	if (KEY("hwidth"))
	{
	hwidth	= -1;
		if (sscanf (data, "%d", &hwidth) != 1 || width <1)		
		{
		error (key, data, "illegal hwidth");
		continue;
		}
		grid.hwidth	= hwidth;
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
