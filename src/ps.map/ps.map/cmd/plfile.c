#include <string.h>
#include "ps_info.h"
#include "local_proto.h"

#define KEY(x)(strcmp(key,x)==0)

int record_point (double e, double n)
{
    char buf[1024];
    int color;
    char name[100], mapset[100];
    double size;
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

    while(input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;

	if (KEY("masked"))
	{
	    masked = yesno(key, data);
	    if (masked) PS.mask_needed = 1;
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
	if (KEY("icon"))
	{
	    if (scan_gis("ps_icons", "icon", key, data, name, mapset, 0))
		have_icon = 1;
	    continue;
	}

	if (KEY("size"))
	{
	    if (sscanf(data, "%lf", &size) != 1 || size <= 0.0)
	    {
		size = 1.0;
		error(key, data, "illegal size request");
	    }
	    continue;
	}

	error(key, data, "illegal point request");
    }

    sprintf(buf, "P %d %f %f %d %f", masked, e, n, color, size);
    if (have_icon)
    {
	strcat(buf, " ");
	strcat(buf, name);
	strcat(buf, " ");
	strcat(buf, mapset);
    }

    add_to_plfile(buf);

    return 0;
}

int record_eps (double e, double n)
{
    char buf[1024];
    char *eps;
    double scale, rotate,  llx, lly, urx, ury;
    int have_eps;
    char *key, *data;
    int masked;
    FILE *fp;

    static char *help[]=
    {
	"epsfile EPS file",
	"scale   #",
	"rotate   #",	
	"masked [y|n]",
	""
    };

    scale = 1.0;
    rotate = 0.0;
    have_eps = 0;    
    masked = 0;

    while(input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;

	if (KEY("masked"))
	{
	    masked = yesno(key, data);
	    if (masked) PS.mask_needed = 1;
	    continue;
	}

	if (KEY("epsfile"))
	{
	    G_chop(data);
	    eps = G_store(data);
	    /* test if file is accessible */
	    if ((fp = fopen(eps, "r")) == NULL)
	    { 
		fprintf (stderr,"Can't open eps file <%s>\n", eps);
		return (0);
	    }
	    have_eps = 1;
	    fclose (fp);	
	    continue;
	}

	if (KEY("scale"))
	{
	    if (sscanf(data, "%lf", &scale) != 1 || scale <= 0.0)
	    {
		scale = 1.0;
		error(key, data, "illegal scale request");
	    }
	    continue;
	}

	if (KEY("rotate"))
	{
	    if (sscanf(data, "%lf", &rotate) != 1 )
	    {
		rotate = 0.0;
		error(key, data, "illegal rotate request");
	    }
	    continue;
	}

	error(key, data, "illegal eps request");
    }
    if (have_eps) 
    { 
	sprintf(buf, "E %d %f %f %f %f %s", masked, e, n, scale, rotate, eps);
    }
    add_to_plfile(buf);

    return 0;
}

int record_line (double e1, double n1, double e2, double n2)
{
    char buf[300];
    int color;
    double width;
    int masked;
    char ch, *key, *data;

    static char *help[]=
    {
	"color  color",
	"width  #",
	"masked [y|n]",
	""
    };

    width = 1.;
    color = BLACK;
    masked = 0;

    while(input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;

	if (KEY("masked"))
	{
	    masked = yesno(key, data);
	    if (masked) PS.mask_needed = 1;
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

	if (KEY("width"))
	{
	    ch = ' ';
	    if (sscanf(data, "%lf%c", &width , &ch) < 1 || width < 0.)
	    {
		width = 1.;
		error(key, data, "illegal width request");
	    }
	    if(ch=='i') width = width/72.;
	    continue;
	}

	error(key, data, "illegal line request");
    }

    sprintf(buf, "L %d %f %f %f %f %d %.8f",
	masked, e1, n1, e2, n2, color, width);

    add_to_plfile(buf);

    return 0;
}

int record_rectangle (double e1, double n1, double e2, double n2)
{
    char buf[300];
    int color, fcolor, fill;
    double width;
    int masked;
    char ch, *key, *data;

    static char *help[]=
    {
	"color  color",
	"fcolor fill color",	
	"width  #",
	"masked [y|n]",
	""
    };

    width = 1.;
    color = BLACK;
    fcolor = WHITE;
    fill=0;    
    masked = 0;

    while(input(2, buf, help))
    {
	if (!key_data(buf, &key, &data)) continue;

	if (KEY("masked"))
	{
	    masked = yesno(key, data);
	    if (masked) PS.mask_needed = 1;
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

	if (KEY("fcolor"))
	{
	    fcolor = get_color_number(data);
	    if (fcolor < 0)
	    {
		fcolor = WHITE;
		error(key, data, "illegal fill color request");
	    }
	    fill=1;
	    continue;
	}	

	if (KEY("width"))
	{
	    ch = ' ';
	    if (sscanf(data, "%lf%c", &width , &ch) < 1 || width < 0.)
	    {
		width = 1.;
		error(key, data, "illegal width request");
	    }
	    if(ch=='i') width = width/72.;
	    continue;
	}

	error(key, data, "illegal rectangle request");
    }

    sprintf(buf, "R %d %f %f %f %f %d %d %d %.8f",
	masked, e1, n1, e2, n2, color, fcolor, fill, width);

    add_to_plfile(buf);

    return 0;
}

int add_to_plfile (char *buf)
{
    FILE *fd;

    if (PS.plfile == NULL)
    {	
	PS.plfile = G_tempfile();
	fd = fopen(PS.plfile, "w");
    }
    else fd = fopen(PS.plfile, "a");
    if (fd != NULL)
    {
	fprintf(fd, "%s\n", buf);
	fclose(fd);
    }
    else error("point/line file", "", "can't open");

    return 0;
}
