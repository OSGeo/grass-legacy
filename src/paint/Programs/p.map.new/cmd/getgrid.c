#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "parms.h"
#include "grid.h"
#include "misc.h"
#include "sgrid.h"
#include "local_proto.h"

#define KEY(x) (strcmp(x,key)==0)
#define EQ(x,y) (strcmp(x,y)==0)

static char *help[]=
{
    "color     color",
    "numbers   # [color] [in|out]",
    "width     #",
    "hcolor    color",
    "hwidth    #",
    "masked    [all|data|nodata]",
    "style     solid|[0-9]",
    "pattern   [notick|tick # #]",
    "numbersize  #",
    "numbersbg color",
    ""
};

int getgrid (void)
{
    int r,g,b;
    int spacing;
    int i;
    int color;
    int width;
    int hwidth;
    char temp[30];
    char temp1[30];
    char buf[1024];
    char t1[128], t2[128];
    char *key, *data, *dp;
    int numberson = 0;

    parms.grid_color = BLACK;
    parms.grid_numbers = 0;
    set_grid_num = 0;

    gridtextsize = 0.0;
    color 	= BLACK;
    width	= 1;
    hwidth 	= 0;
    grid.linestyle 	= NULL;

    for (i=0; i<9; i++)
	grid.colors[i]	= BLACK;

    grid.width	= 1;
    grid.hcolor	= WHITE;
    grid.hwidth	= 0;
    strcpy (grid.gridon, "all");
    strcpy (grid.pattern, "notick");
    grid.patternw	= 10;
    grid.patternh	= 10;
    gridtextbg		= WHITE;

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;


	if (KEY("style"))
	{
	   G_strip (data);
	   if (strcmp (data, "solid") == 0)
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

	if (KEY("masked"))
	{
	   if (sscanf (data, "%s %s", t1, t2) != 1) {
	      error (key, data, "illegal gridon input"); 
	      continue;
	   }
	
	if (EQ(t1,"all") || EQ(t1,"data") || EQ(t1, "nodata")) 
	   strcpy (grid.gridon, t1);
	else
	   error (key, data, "illegal masked input"); 

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
	   if (sscanf (data, "%s %s %s", t1, t2, t3) == 3)	   {
	      if (EQ(t1,"tick") ) {
		 strcpy (grid.pattern, t1);
		 grid.patternw	= atoi(t2);
		 grid.patternh	= atoi(t3);
		 continue;
	      }
	    } 
	}
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
	    switch (sscanf (data, "%d %s %s", &spacing, temp, temp1))
	    {
	    case 1: color = BLACK; break;
	    case 2: if (!scan_color (temp, &color,&r,&g,&b))
			spacing = -1;
		    break;
	    case 3: if (!scan_color (temp, &color,&r,&g,&b))
			spacing = -1;
			if (EQ(temp1,"out") || EQ(temp1,"in")) 
			{
			if (EQ(temp1,"out") ) set_grid_num		= 1; 
			else
			set_grid_num		= 0;
		
			}
			else
			spacing	= -1;
			
		    break;
	    }
	    if (spacing < 0)
		error (key,data,"illegal numbers request");
	    else
	    {
		parms.grid_numbers = spacing;
		parms.grid_numbers_color = color;
		numberson	= 1;
	    }
	    continue;
	}

	if (KEY("numbersbg"))
	{
	   if (sscanf(data, "%s %s", t1, t2) == 1 && EQ(t1, "none"))
	      gridtextbg = -1;
	   else
	   {
	      if (!scan_color (data, &color, &r,&g,&b)) {
	         gridtextbg = -1;
	         error (key,data,"illegal numbersbg request");
	      }
	    gridtextbg = color;
	    }
	    continue;
	}

	if (KEY("numbersize"))
	{
	   double x;

	   if (!scan_resolution(data, &x))
	   {
	      gridtextsize = 0.0;
	      error (key, data, "illegal numbersize request");
	    }
	    else 
	       gridtextsize = x;

	    continue;
	}

	error (key,data,"illegal request");
    }

	if (set_grid_num && !numberson)
		set_grid_num 	= 0;

    return 0;
}
