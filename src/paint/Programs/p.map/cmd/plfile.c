#include "gis.h"
#include <string.h>
#include "parms.h"
#include "misc.h"
#include "local_proto.h"

#define KEY(x) (strcmp(key,x)==0)

int record_point (double e, double n)
{
    char buf[1024];
    int color;
    int r,g,b;
    char name[100], mapset[100];
    float size ;
    int have_icon;
    char *key, *data;
    int masked;

    static char *help[]=
    {
	"color  color",
	"icon   iconfile",
	"size   #",
	"masked [y|n]",
	""
    };

    size = 1.0;
    have_icon = 0;
    color = BLACK;
    masked = 0;

    while (input(2, buf, help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("masked"))
	{
	    masked = yesno (key,data);
	    continue;
	}
	if (KEY("color"))
	{
	    if (!scan_color (data, &color, &r,&g,&b))
	    {
		color = BLACK;
		error (key,data,"illegal color request");
	    }
	    continue;
	}
	if (KEY("icon"))
	{
	    if (scan_gis("icons","icon",key,data,name,mapset,0))
		have_icon = 1;
	    continue;
	}

	if (KEY("size"))
	{
	    if (sscanf (data,"%f",&size) != 1 || size <= 0.0)
	    {
		size = 1.0;
		error (key, data, "illegal size request");
	    }
	    continue;
	}

	error (key,data,"illegal point request");
    }

    sprintf (buf, "P %d %f %f %d %f", masked, e, n, color, size);
    if (have_icon)
    {
	strcat (buf, " ");
	strcat (buf, name);
	strcat (buf, " ");
	strcat (buf, mapset);
    }

    add_to_plfile (buf);

    return 0;
}

int 
record_line (double e1, double n1, double e2, double n2)
{
    char buf[300];
    int color;
    int r,g,b;
    int width;
    int masked;
    char *key, *data;

    static char *help[]=
    {
	"color  color",
	"width  #",
	"masked [y|n]",
	""
    };

    width = 1;
    color = BLACK;
    masked = 0;

    while (input(2, buf, help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("masked"))
	{
	    masked = yesno (key,data);
	    continue;
	}
	if (KEY("color"))
	{
	    if (!scan_color (data, &color, &r,&g,&b))
	    {
		color = BLACK;
		error (key,data,"illegal color request");
	    }
	    continue;
	}

	if (KEY("width"))
	{
	    if (sscanf (data,"%d",&width) != 1 || width < 1)
	    {
		width = 1;
		error (key, data, "illegal size request");
	    }
	    continue;
	}

	error (key,data,"illegal line request");
    }

    sprintf (buf, "L %d %f %f %f %f %d %d",
	masked, e1, n1, e2, n2, color, width);

    add_to_plfile (buf);

    return 0;
}

int 
add_to_plfile (char *buf)
{
    FILE *fd;

    if (parms.plfile == NULL)
    {	
	parms.plfile = G_tempfile();
	fd = fopen (parms.plfile,"w");
    }
    else
	fd = fopen (parms.plfile,"a");
    if (fd != NULL)
    {
	fprintf (fd, "%s\n", buf);
	fclose (fd);
    }
    else
	error ("point/line file","","can't open");

    return 0;
}
