
#include "gis.h"
#include "labels.h"
#include "misc.h"
#include "text.h"
#include "clegend.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)

static char *help[]=
{
	"title		 title",
	"east		 east",
	"north		 north",
    "font        fontname",
    "color       color|none",
    "background  color|none",
    "border      color|none",
    "size        #",
    "xoffset     #",
    "yoffset     #",
	"hspacing	horizontal spacing - 10",
	"vspacing	vertical spacing - 0",
	"ceast		 #",
	"cnorth	 	 #",
    "cwidth      #",
	"cheight	 #",
	"ccolor		 #",
	"cfont		 #",
	"csize		 #",
	"cbackground #",
	"ebackground #",
	"eborder	 #",
	""
};

record_ctable ()
{
    int r,g,b;
    int color;
    int background;
    int border;
    int xoffset;
    int yoffset;
    float size;
    int xref, yref;
	char title[128];
    char t1[128], t2[128];
    char buf[1024];
    char *key, *data;
    FILE *fd;
    char fontname[128];
	char east[50];
	char north[50];
	int	 hspacing;
	int  vspacing;
	int  eborder;

/* attributes for category */
    int cwidth;
	int cheight;
	int ccolor; 
	int cbackground;
	int ebackground;
	float csize;
	char ceast[50];
	char cnorth[50];
	char   cfontname[128];


    color = BLACK;
    background = -1;
    border = -1;
    size = 500.0;
    xoffset = 0;
    yoffset = 0;
    xref = LEFT;
    yref = UPPER;
	eborder = -1;
	ebackground = -1;
    strcpy (fontname, "standard");


/* initialization for attributes for category */
    cwidth = 20;
    cheight= 10;
	csize  = 400.0;
	ccolor = BLACK;
	hspacing = 0;
	vspacing = 0;
	cbackground = -1;
    strcpy (cfontname, "standard");
	strcpy (ceast, "0.0");
	strcpy (cnorth,"0.0");
	strcpy (title, "NULL");


    while (input(2,buf,help))
    {
	if (!key_data (buf, &key, &data))
	    continue;

	if (KEY("title"))
	{
	if (sscanf (data, "%s %s", t1, t2) != 1)
	{
	error (key, data, "illegal request");
	continue;
	}
	strcpy (title, t1);
	printf (" title is %s \n", t1);

	continue;
	}

	if (KEY("east"))
	{
	double e;
		sscanf (data, "%s %s", t1, t2);
	if (sscanf (data, "%s %s", t1, t2) != 1)
	{
	error (key, data, "illegal request");
	continue;
	}
	if (!scan_easting(t1, &e))
		continue;
	else
		strcpy (east, t1);
	continue;
	}

	if (KEY("north"))
	{
	double n;
		sscanf (data, "%s %s", t1, t2);
	if (sscanf (data, "%s %s", t1, t2) != 1)
	{
	error (key, data, "illegal request");
	continue;
	}
	if (!scan_northing(t1, &n))
		continue;
	else
		strcpy (north, t1);
	continue;
	}




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

	if (KEY("hspacing"))
	{
	    *t1 = 0;
	    if (sscanf(data,"%d%1s", &hspacing, t1) != 1 || *t1)
	    {
		hspacing = 0;
		error (key,data,"illegal request");
	    }
	    continue;
	}

	if (KEY("vspacing"))
	{
	    *t1 = 0;
	    if (sscanf(data,"%d%1s", &vspacing, t1) != 1 || *t1)
	    {
		vspacing = 0;
		error (key,data,"illegal request");
	    }
	    continue;

	}


/*
ref is always wrt left upper
	if (KEY("ref"))
	{
	    if (!scan_ref (data, &xref, &yref))
	    {
		xref = LEFT;
		yref = UPPER;
		error (key,data,"illegal ref request");
	    }
	    continue ;
	}
	*/

	if (KEY("cwidth"))
	{
	    cwidth = 0;
	    if (sscanf(data,"%d%1s", &cwidth, t1) != 1 || cwidth <= 0)
	    {
		cwidth = 1;
		error (key,data,"illegal width request");
	    }
	    continue;
	}

	if (KEY("cheight"))
	{
	    cheight= 0;
	    if (sscanf(data,"%d%1s", &cheight, t1) != 1 || cheight<= 0)
	    {
		cheight = 1;
		error (key,data,"illegal height request");
	    }
	    continue;
	}

	if (KEY("ccolor"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		ccolor = -1;
	    else if (!scan_color (data, &ccolor, &r,&g,&b))
	    {
		ccolor = BLACK;
		error (key,data,"illegal ccolor request");
	    }
	    continue;
	}

	if (KEY("cfont"))
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
	    strcpy (cfontname, "standard");
	    switch (check_font (t1))
	    {
	    case 0:
		error (key,data,"no such font");
		break;
		}

	}

	if (KEY("csize"))
	{
	    double x;

	    if (!scan_resolution(data,&x))
	    {
		csize = 0.0 ;
		error (key,data,"illegal csize request");
	    }
	    else
		csize=x;
	    continue;
	}


	if (KEY("ceast"))
	{
	double e;
	if (sscanf (data, "%s %s", t1, t2) != 1)
	{
	error (key, data, "illegal request");
	continue;
	}
	if (!scan_easting(t1, &e))
		continue;
	else
		strcpy (ceast, t1);
	
	continue;
	}

	if (KEY("cnorth"))
	{
	double n;
	if (sscanf (data, "%s %s", t1, t2) != 1)
	{
	error (key, data, "illegal request");
	continue;
	}
	if (!scan_northing(t1, &n))
		continue;
	else
		strcpy (cnorth, t1);
	continue;
	}

	if (KEY("eborder"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		eborder = -1;
	    else if (!scan_color (data, &eborder, &r,&g,&b))
	    {
		eborder = -1;
		error (key,data,"illegal eborder request");
	    }
	    continue;
	}

	if (KEY("ebackground"))
	{
	    if (sscanf(data,"%s %s", t1, t2) == 1 && EQ(t1,"none"))
		ebackground = -1;
	    else if (!scan_color (data, &ebackground, &r,&g,&b))
	    {
		ebackground = -1;
		error (key,data,"illegal ebackground request");
	    }
	    continue;
	}



	error (key, "", "illegal category table request");
	}

    if (prelegend.other == NULL)
    {
	prelegend.other = G_tempfile();
	if ((fd = fopen(prelegend.other,"w")) != NULL)
	    fclose (fd);
    }
    fd = fopen (prelegend.other,"a");
    if (fd == NULL)
    {
	error ("misc prelegend file","","can't open");
	return;
    }
    fprintf (fd,"font: %s\n", fontname);
    fprintf (fd,"east: %s\n", east);
    fprintf (fd,"north: %s\n", north);
    fprintf (fd,"xoffset: %d\n", xoffset);
    fprintf (fd,"yoffset: %d\n", yoffset);
    fprintf (fd,"color: %d\n", color);
    fprintf (fd,"size: %f\n", size);

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

    fprintf (fd,"ebackground:");
    if (ebackground >= 0)
	fprintf (fd, "%d\n", ebackground);
    else
	fprintf (fd, "none\n");

    fprintf (fd,"eborder:");

    if (eborder >= 0)
	fprintf (fd, "%d\n",eborder);
    else
	fprintf (fd, "none\n");





	fprintf (fd, "title:%s\n", title);

    fprintf (fd,"hspacing: %d\n", hspacing);
    fprintf (fd,"vspacing: %d\n", vspacing);

    fprintf (fd,"ccolor: %d\n", ccolor);
    fprintf (fd,"csize: %f\n", csize);
    fprintf (fd,"cwidth: %d\n", cwidth);
    fprintf (fd,"cheight: %d\n", cheight);
    fprintf (fd,"ceast: %s\n", ceast);
    fprintf (fd,"cnorth: %s\n", cnorth);
	fprintf (fd, "end:\n");
    


    fclose (fd);

}
