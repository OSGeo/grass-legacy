
#include "gis.h"
#include "labels.h"
#include "misc.h"
#include "text.h"
#include "clegend.h"
#include "legendlabel.h"
#include "parms.h"
#include "barscale.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)

static char *help[]=
{
	"unit		 ft|mi|km|m",
	"length		 #",
	"interval	 #",
    "width       #",
	"color		 color",
	"style		 dash|tick",
    "textcolor   color|none",
    "textsize    #",
	"background  color|none",
	"border		 color|none",
	"font		 font",
    ""
};

record_barscale(east, north)
    char *east;
    char *north;
{
    int r,g,b;
    int width;
    int textcolor;
    float textsize;
    int background;
    int border;
    char t1[128], t2[128];
    char buf[1024];
    char *key, *data;
    FILE *fd, *fd1, *fd2;
    char fontname[128];
	char style[10];
	char unit[10];
	int  length;
	int  interval;
	int  color;

    textcolor = BLACK;
    background = -1;
    border = -1;
    width  = 2;
	interval  = 1;
	length	  = 1;
	color	= BLACK;

    strcpy (fontname, "standard");
	strcpy (unit, "m");
	strcpy (style, "dash");

	if (G_projection() == PROJECTION_XY)
	textsize = 5.0;
	else
    textsize = 400.0;

#ifdef DEBUG
 printf ("record_label(%s at east=%s north=%s)\n", text, east, north);
#endif
    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;



	if (KEY("unit"))
	{
	if (sscanf (data, "%s %s", t1, t2) != 1)
	    {
		error (key, data, "illegal unit request");
		continue;
		}

	if (EQ(t1,"ft") || EQ(t1,"mi") || EQ(t1,"km")
		|| EQ(t1,"m")) 
		strcpy(unit, t1);

	else 
		error (key, data, "illegal unit request");
	continue;
	}


	if (KEY("length")) 
	{
	if (sscanf (data, "%s %s", t1, t2) != 1)
	    {
		error (key, data, "illegal length request");
		continue;
		}

	

	length = atoi(t1);
	
	continue;
	}
	

	if (KEY("interval"))
	{
	if (sscanf (data, "%s %s", t1, t2) != 1)
	    {
		error (key, data, "illegal length request");
		continue;
		}

	interval = atoi(t1);
	continue;
	}

	if (KEY("style"))
	{
	if (sscanf (data, "%s %s", t1, t2) != 1)
	    {
		error (key, data, "illegal style request");
		continue;
		}
	if (EQ(t1,"dash") || EQ(t1,"tick") ) 
		strcpy(style, t1);

	else 
		error (key, data, "illegal style request");


	continue;
	}

	if (KEY("color"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		textcolor = -1;
	    else if (!scan_color (data, &color, &r,&g,&b))
	    {
		color = BLACK;
		error (key,data,"illegal color request");
	    }
	    continue;
	}

	if (KEY("font"))
	{
	    if (sscanf (data, "%s %s", t1, t2) != 1)
	    {
		error (key, data, "illegal font request");
		continue;
	    }
	    if (EQ(t1,"list"))
	    {
		list_fonts();
		continue;
	    }
	    strcpy (fontname, "standard");
	    switch (check_font (t1))
	    {
	    case 0:
		error (key,data,"no such font");
		break;
	    case 1:
		strcpy (fontname, t1);
		break;
	    default:
		error (key, data, "font not readable");
		break;
	    }
	    continue;
	}

	if (KEY("textcolor"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		textcolor = -1;
	    else if (!scan_color (data, &textcolor, &r,&g,&b))
	    {
		textcolor = BLACK;
		error (key,data,"illegal textcolor request");
	    }
	    continue;
	}


	if (KEY("background"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		background = -1;
	    else if (!scan_color (data, &background, &r,&g,&b))
	    {
		background = -1;
		error (key,data,"illegal background request");
	    }
	    continue;
	}

	if (KEY("border"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		border = -1;
	    else if (!scan_color (data, &border, &r,&g,&b))
	    {
		border = -1;
		error (key,data,"illegal border request");
	    }
	    continue;
	}


	if (KEY("width"))
	{
	    width = 0;
	    if (sscanf(data,"%d%1s", &width, t1) != 1 || width <= 0)
	    {
		width = 1;
		error (key,data,"illegal width request");
	    }
		printf (" width is %d\n", width);
	    continue;
	}




	if (KEY("textsize"))
	{
	    double x;

	    if (!scan_resolution(data,&x))
	    {
		textsize = 0.0 ;
		error (key,data,"illegal textsize request");
	    }
	    else
		textsize=x;
	    continue;
	}




	error (key, data, "illegal request");
	}



	if (barscale.other == NULL)
	{
	barscale.other = (char *)G_tempfile();
	if ((fd1 = fopen(barscale.other, "w")) != NULL)
		fclose (fd1);
	}

	fd1 = fopen (barscale.other, "a");
	if (fd1 == NULL)
	{
	error ("misc barscale file", "", "can't open");
	return;
	}

    fprintf (fd1,"font: %s\n", fontname);
    fprintf (fd1,"east: %s\n", east);
    fprintf (fd1,"north: %s\n", north);
	fprintf (fd1, "color: %d\n", color);
    fprintf (fd1,"textcolor: %d\n", textcolor);
    fprintf (fd1,"width: %d\n", width);
    fprintf (fd1,"textsize: %f\n", textsize);

    fprintf (fd1,"background: ");
    if (background >= 0)
	fprintf (fd1, "%d\n", background);
    else
	fprintf (fd1, "none\n");
    fprintf (fd1,"border: ");
    if (border >= 0)
	fprintf (fd1, "%d\n", border);
    else
	fprintf (fd1, "none\n");

	fprintf (fd1, "unit:%s\n", unit);
	fprintf (fd1, "length: %d\n", length);
	fprintf (fd1, "interval: %d\n", interval);
	fprintf (fd1, "style:%s\n", style);




	fprintf (fd1, "end:\n");
    fclose (fd1);
}
