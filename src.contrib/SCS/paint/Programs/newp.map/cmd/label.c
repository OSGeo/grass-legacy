#include "gis.h"
#include "labels.h"
#include "misc.h"
#include "text.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)

static char *help[]=
{
    "font        fontname",
    "color       color|none",
    "width       #",
    "background  color|none",
    "border      color|none",
    "size        #",
    "hcolor      color|none",
    "hwidth      #",
    "ref         upper|lower|center left|right|center",
    "xoffset     #",
    "yoffset     #",
    "opaque      [y|n]",
    ""
};

record_label (east, north, text)
    char *east;
    char *north;
    char *text;
{
    int r,g,b;
    int color;
    int hcolor;
    int background;
    int border;
    int xoffset;
    int yoffset;
    float size;
    int width;
    int hwidth;
    int xref, yref;
    int opaque;
	int rotation;
    char t1[128], t2[128];
    char buf[1024];
    char *key, *data;
    FILE *fd;
    char fontname[128];

    color = BLACK;
    hcolor = -1;
    background = -1;
    border = -1;
    opaque = 1;
    size = 550.0;
    xoffset = 0;
    yoffset = 0;
    width = 1;
    hwidth = 0;
	rotation = 0;
    xref = CENTER;
    yref = CENTER;
    strcpy (fontname, "standard");

#ifdef DEBUG
 printf ("record_label(%s at east=%s north=%s)\n", text, east, north);
#endif
    while (*text == ' ' || *text == '\t')
	text++;
    if (*text == '\\')
	text++;
    if (*text == 0)
    {
	error ("text","","no text given");
	gobble_input();
	return;
    }

    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("font"))
	{
	    if (sscanf (data, "%s %s", t1, t2) != 1)
	    {
		error (key, data, "illegal request");
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

	if (KEY("color"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		color = -1;
	    else if (!scan_color (data, &color, &r,&g,&b))
	    {
		color = BLACK;
		error (key,data,"illegal color request");
	    }
	    continue;
	}

	if (KEY("hcolor"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		hcolor = -1;
	    else if (!scan_color (data, &hcolor, &r,&g,&b))
	    {
		hcolor = -1;
		error (key,data,"illegal color request");
	    }
	    if (hcolor >= 0 || hwidth <= 0) hwidth = 1;
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

	if (KEY("opaque"))
	{
	    opaque = yesno(key,data);
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
	    continue;
	}

	if (KEY("hwidth"))
	{
	    hwidth = 0;
	    if (sscanf(data,"%d%1s", &hwidth, t1) != 1 || hwidth <= 0)
	    {
		hwidth = 0;
		error (key,data,"illegal width request");
	    }
	    continue;
	}

	if (KEY("size"))
	{
	    double x;

	    if (!scan_resolution(data,&x))
	    {
		size = 0.0 ;
		error (key,data,"illegal size request");
	    }
	    else
		size=x;
	    continue;
	}

	if (KEY("xoffset"))
	{
	    *t1 = 0;
	    if (sscanf(data,"%d%1s", &xoffset, t1) != 1 || *t1)
	    {
		xoffset = 0;
		error (key,data,"illegal request");
	    }
	    continue;
	}

	if (KEY("yoffset"))
	{
	    *t1 = 0;
	    if (sscanf(data,"%d%1s", &yoffset, t1) != 1 || *t1)
	    {
		yoffset = 0;
		error (key,data,"illegal request");
	    }
	    continue;
	}

	if (KEY("ref"))
	{
	    if (!scan_ref (data, &xref, &yref))
	    {
		xref = CENTER;
		yref = CENTER;
		error (key,data,"illegal ref request");
	    }
	    continue ;
	}

	if (KEY("rotation"))
	{
	    if (sscanf(data,"%d%1s", &rotation, t1) != 1 || rotation < 0 || rotation > 360)
	    {
		rotation = -1;
		error (key,data,"illegal rotation request");
	    }
	    continue ;
	}

	error (key,data,"illegal request");
    }

    if (labels.other == NULL)
    {
	labels.other = G_tempfile();
	if ((fd = fopen(labels.other,"w")) != NULL)
	    fclose (fd);
    }
    fd = fopen (labels.other,"a");
    if (fd == NULL)
    {
	error ("misc labels file","","can't open");
	return;
    }
	printf (" rotation is %d \n", rotation);
    fprintf (fd,"font: %s\n", fontname);
    fprintf (fd,"east: %s\n", east);
    fprintf (fd,"north: %s\n", north);
    fprintf (fd,"xoffset: %d\n", xoffset);
    fprintf (fd,"yoffset: %d\n", yoffset);
    fprintf (fd,"color: %d\n", color);
    fprintf (fd,"width: %d\n", width);
    fprintf (fd,"hcolor: %d\n", hcolor);
    fprintf (fd,"hwidth: %d\n", hwidth);
    fprintf (fd,"size: %f\n", size);
	fprintf (fd, "rotation: %d\n", rotation);
    fprintf (fd,"opaque: %s\n", opaque?"yes":"no");

    fprintf (fd,"background: ");
    if (background >= 0)
	fprintf (fd, "%d\n", background);
    else
	fprintf (fd, "none\n");
    fprintf (fd,"border: ");

    if (border >= 0)
	fprintf (fd, "%d\n", border);
    else
	fprintf (fd, "none\n");
    
    fprintf (fd, "ref: ");
    switch (yref)
    {
    case UPPER:  fprintf (fd, "upper"); break;
    case LOWER:  fprintf (fd, "lower"); break;
    case CENTER: fprintf (fd, "center"); break;
    }
    switch (xref)
    {
    case LEFT:   fprintf (fd, " left"); break;
    case RIGHT:  fprintf (fd, " right"); break;
    case CENTER: fprintf (fd, "%s", xref==CENTER?"":" center"); break;
    }
    fprintf (fd, "\n");

    fprintf (fd, "text:%s\n",text);

    fclose (fd);
}
