/* Function: record_label
**
** This is a modified version of the p.map function.
**
** Modified by: Paul W. Carlson	May 1992
*/

#include <string.h>
#include "labels.h"
#include "ps_info.h"
#include "local_proto.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)
#define LEFT 0
#define RIGHT 1
#define LOWER 0
#define UPPER 1
#define CENTER 2

extern char *get_color_name();
extern int get_color_number();

static char *help[]=
{
    "font        fontname",
    "color       color",
    "width       #",
    "background  color|none",
    "border      color|none",
    "size        #",
    "fontsize    fontsize",
    "hcolor      color|none",
    "hwidth      #",
    "ref         upper|lower|center left|right|center",
    "xoffset     #",
    "yoffset     #",
    "opaque      [y|n]",
    ""
};

int read_text (char *east, char *north, char *text)
{
    int color;
    int hcolor;
    int background;
    int border;
    int xoffset;
    int yoffset;
    float size;
    int fontsize;
    double width;
    double hwidth;
    int xref, yref;
    int opaque;
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
    size = 0.0;
    fontsize = 0;
    xoffset = 0;
    yoffset = 0;
    width = 1.;
    hwidth = 0.;
    xref = CENTER;
    yref = CENTER;
    strcpy(fontname, "Helvetica");

    while (*text == ' ' || *text == '\t') text++;
    if (*text == '\\') text++;
    if (*text == 0)
    {
	error ("text", "", "no text given");
	gobble_input();
	return 0;
    }

    while (input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;

	if (KEY("font"))
	{
	    get_font(data);
	    strcpy(fontname, data);
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
	    continue;
	}

	if (KEY("hcolor"))
	{
	    if (sscanf(data, "%s %s", t1, t2) == 1 && EQ(t1, "none"))
		hcolor = -1;
	    else
	    {
	    	hcolor = get_color_number(data);
		if (hcolor < 0)
	    	{
		    hcolor = -1;
		    error(key, data, "illegal hcolor request");
	    	}
	    }
	    if (hcolor <= 0 || hwidth <= 0.) hwidth = 0.;
	    continue;
	}

	if (KEY("background"))
	{
	    if (sscanf(data, "%s %s", t1, t2) == 1 && EQ(t1, "none"))
	    {
		opaque = 0;
		background = -1;
            }
	    else
	    {
	    	background = get_color_number(data);
		if (background < 0)
	    	{
		    background = -1;
		    error(key, data, "illegal background request");
	    	}
	    }
	    continue;
	}

	if (KEY("border"))
	{
	    if (sscanf(data, "%s %s", t1, t2) == 1 && EQ(t1, "none"))
		border = -1;
	    else
	    {
	    	border = get_color_number(data);
		if (border < 0)
	    	{
		    border = -1;
		    error(key, data, "illegal border request");
	    	}
	    }
	    continue;
	}

	if (KEY("opaque"))
	{
	    opaque = yesno(key, data);
	    continue;
	}

	if (KEY("width"))
	{
	    width = -1.;
            *t1 = 0;
	    if (sscanf(data, "%lf%1s", &width, t1) < 1 || width < 0.)
	    {
		width = 1.;
		error(key, data, "illegal width request");
	    }
	    if(t1&&t1[0]=='i') width = width/ 72.0;
	    continue;
	}

	if (KEY("hwidth"))
	{
	    hwidth = -1.;
	    *t1 = 0;
	    if (sscanf(data, "%lf%1s", &hwidth, t1) < 1 || hwidth < 0.)
	    {
		hwidth = 0.;
		error(key, data, "illegal width request");
	    }
	    if(t1&&t1[0]=='i') hwidth = hwidth/ 72.0;
	    continue;
	}

	if (KEY("size"))
	{
	    double x;

	    if (!scan_resolution(data, &x))
	    {
		size = 0.0 ;
		error(key, data, "illegal size request");
	    }
	    else size = x;
	    continue;
	}

	if (KEY("fontsize"))
	{
	    if (sscanf(data, "%d", &fontsize) != 1 || fontsize <= 0 )
	    {
		error(key, data, "illegal fontsize request");
	    }
	    else continue;
	}

	if (KEY("xoffset"))
	{
	    *t1 = 0;
	    if (sscanf(data, "%d%1s", &xoffset, t1) != 1 || *t1)
	    {
		xoffset = 0;
		error(key, data, "illegal request");
	    }
	    continue;
	}

	if (KEY("yoffset"))
	{
	    *t1 = 0;
	    if (sscanf(data, "%d%1s", &yoffset, t1) != 1 || *t1)
	    {
		yoffset = 0;
		error(key, data, "illegal request");
	    }
	    continue;
	}

	if (KEY("ref"))
	{
	    if (!scan_ref(data, &xref, &yref))
	    {
		xref = CENTER;
		yref = CENTER;
		error(key, data, "illegal ref request");
	    }
	    continue ;
	}

	error(key, data, "illegal request");
    }

    /* if file doesn't exist create it and close it */
    if (labels.other == NULL)
    {
	labels.other = G_tempfile();
	if ((fd = fopen(labels.other, "w")) != NULL) fclose(fd);
    }

    /* open file in append mode */
    fd = fopen(labels.other, "a");
    if (fd == NULL)
    {
	error("misc labels file", "", "can't open");
	return 1;
    }

    /* write the file */
    fprintf(fd, "font: %s\n", fontname);
    fprintf(fd, "east: %s\n", east);
    fprintf(fd, "north: %s\n", north);
    fprintf(fd, "xoffset: %d\n", xoffset);
    fprintf(fd, "yoffset: %d\n", yoffset);
    fprintf(fd, "width: %f\n", width);
    fprintf(fd, "hwidth: %f\n", hwidth);
    fprintf(fd, "size: %f\n", size);
    fprintf(fd, "fontsize: %d\n", fontsize);
    fprintf(fd, "opaque: %s\n", opaque?"yes":"no");
    fprintf(fd, "color: ");
    if (color >= 0)      fprintf(fd, "%s\n", get_color_name(color));
    else fprintf(fd, "black\n");
    fprintf(fd, "hcolor: ");
    if (hcolor >= 0)     fprintf(fd, "%s\n", get_color_name(hcolor));
    else fprintf(fd, "none\n");
    fprintf(fd, "background: ");
    if (background >= 0) fprintf(fd, "%s\n", get_color_name(background));
    else fprintf(fd, "none\n");
    fprintf(fd, "border: ");
    if (border >= 0)     fprintf(fd, "%s\n", get_color_name(border));
    else fprintf(fd, "none\n");
    fprintf(fd, "ref: ");
    switch (yref)
    {
    	case UPPER:  fprintf(fd, "upper"); break;
    	case LOWER:  fprintf(fd, "lower"); break;
    	case CENTER: fprintf(fd, "center"); break;
    }
    switch (xref)
    {
    	case LEFT:   fprintf(fd, " left"); break;
    	case RIGHT:  fprintf(fd, " right"); break;
    	case CENTER: fprintf(fd, "%s", (xref==CENTER) ? "" : " center"); break;
    }
    fprintf(fd, "\n");
    fprintf(fd, "text:%s\n", text);
    fclose (fd);

    return 0;
}
