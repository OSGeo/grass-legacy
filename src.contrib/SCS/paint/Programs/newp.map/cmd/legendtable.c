#include "gis.h"
#include "labels.h"
#include "misc.h"
#include "text.h"
#include "clegend.h"
#include "legendlabel.h"
#include "vector.h"
#include "sites.h"
#include "parms.h"

#define EQ(x,y) (strcmp(x,y)==0)
#define KEY(x) EQ(key,x)

static char *help[]=
{
	"height		 #",
    "width       #",
    "font        fontname",
    "textcolor   color|none",
    "textsize    #",
	"xspace		 #",
	"yspace		 #",
    "background  color|none",
    "border      color|none",
	"sitesname # desc",
	"beginrast	 ",
	"beginvect	 ",
	"beginsites	 ",
    ""
};

record_legendtable(east, north)
    char *east;
    char *north;
{
    int r,g,b;
    int height;
	int vlen;
    int width;
    int textcolor;
    float textsize;
	int xspace;
	int yspace;
    int background;
    int border;
    char t1[128], t2[128];
    char buf[1024];
    char *key, *data;
    FILE *fd, *fd1, *fd2;
    char fontname[128];

    textcolor = BLACK;
    background = -1;
    border = -1;
    width  = 20;
    height = 10;
	vlen   = 20;
	xspace	  = 10;
	yspace	  = 0;
	count	  = 0;
	isramp	  = 0;
    strcpy (fontname, "standard");

	if (G_projection() == PROJECTION_LL)
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
	    continue;
	}


	if (KEY("height"))
	{
	    height= 0;
	    if (sscanf(data,"%d%1s", &height, t1) != 1 || height<= 0)
	    {
		height= 0;
		error (key,data,"illegal height request");
	    }
	    continue;
	}


	if (KEY("vlen"))
	{
	    vlen = 0;
	    if (sscanf(data,"%d%1s", &vlen, t1) != 1 || vlen<= 0)
	    {
		vlen = 1;
		error (key,data,"illegal vlen request");
	    }
	    continue;
	}




	if (KEY("xspace"))
	{
	    xspace = 0;
	    if (sscanf(data,"%d%1s", &xspace, t1) != 1 || xspace<= 0)
	    {
		xspace= 0;
		error (key,data,"illegal xspace request");
	    }
	    continue;
	}

	if (KEY("yspace"))
	{
	    yspace = 0;
	    if (sscanf(data,"%d%1s", &yspace, t1) != 1 || yspace<= 0)
	    {
		yspace= 0;
		error (key,data,"illegal yspace request");
	    }
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


	if (KEY("beginrast")) {
	int i, found ;
	found =0;
		if (parms.cellfd < 0) {
	        error (key, data, "no raster file has been selected");
			continue; }
		svrtmp[count].nolabels = 0;
		getrast();
		if (isramp) {
		for (i=0; i<count; i++) {
			if (strcmp(svrtmp[i].labeltype, "beginrast")==NULL) 
			{
				found = 1;
				continue;
				}
			
		}
		}
		if (!found) {
		strcpy (svrtmp[count].labeltype,"beginrast");
		svrtmp[count].nolabels = nocats;
		count++;
		}
		continue;
		}

	if (KEY("beginvect")) {
		if (vector.count <= 0) {
	        error (key, data, "no vector file has been selected");
			continue; }
		svrtmp[count].nolabels = 0;
		getvect ();
		strcpy (svrtmp[count].labeltype, "beginvect");
		svrtmp[count].nolabels = novects;
		count++;
		continue;
		}

	if (KEY("beginsite")) {
		if (site.count <= 0) {
	        error (key, data, "no site file has been selected");
			continue; }

		svrtmp[count].nolabels = 0;
		getsite();
		strcpy (svrtmp[count].labeltype, "beginsite");
		svrtmp[count].nolabels = nosites;
		count++;
		continue;
		}




	error (key, data, "illegal request");
	}



	if (prelegend.other == NULL)
	{
	prelegend.other = (char *)G_tempfile();
	if ((fd1 = fopen(prelegend.other, "w")) != NULL)
		fclose (fd1);
	}

	fd1 = fopen (prelegend.other, "a");
	if (fd1 == NULL)
	{
	error ("misc prelegend file", "", "can't open");
	return;
	}


    fprintf (fd1,"font: %s\n", fontname);
    fprintf (fd1,"east: %s\n", east);
    fprintf (fd1,"north: %s\n", north);
    fprintf (fd1,"textcolor: %d\n", textcolor);
    fprintf (fd1,"width: %d\n", width);
    fprintf (fd1,"height: %d\n", height);
    fprintf (fd1,"xspace: %d\n", xspace);
    fprintf (fd1,"yspace: %d\n", yspace);
    fprintf (fd1,"textsize: %f\n", textsize);
	fprintf (fd1,"vlen: %d\n", vlen);

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


	{int i, j, nocat, novect, nosite;
	nocat  = 0;
	novect = 0;
	nosite = 0;
	for (i=0; i<count; i++){
	if (strcmp (svrtmp[i].labeltype, "beginrast") == NULL) { 


	   fprintf (fd1, "beginrast:\n");
	   if (isramp) 
		   fprintf (fd1, "ramp:%s %s\n", reqramp,ramporientation );
	
	   else {
	   for (j=nocat; j<svrtmp[i].nolabels+nocat; j++) { 
	   fprintf (fd1, "cat:\n");
	   fprintf (fd1, "%d:%s\n", cattmp[j].catnum, cattmp[j].label);
	   }
	}

		fprintf (fd1, "endrast:\n");
		nocat = svrtmp[i].nolabels + nocat;

	}

	if (strcmp (svrtmp[i].labeltype, "beginvect") == NULL) {
		fprintf (fd1, "beginvect:\n");
		for (j=novect; j<svrtmp[i].nolabels+novect; j++) {
			fprintf (fd1, "vect:\n");
			fprintf (fd1, "%s: %s\n", vtmp[j].labelname,
			vtmp[j].label);
			}

		fprintf (fd1, "endvect:\n");

		novect = svrtmp[i].nolabels + novect;

	}

	if (strcmp (svrtmp[i].labeltype, "beginsite") == NULL) {

	fprintf (fd1, "beginsite:\n");

	for (j=nosite; j<svrtmp[i].nolabels+nosite; j++) {
		fprintf (fd1, "site:\n");
		fprintf (fd1, "%s: %s\n", stmp[j].labelname, stmp[j].label);
		}

		fprintf (fd1, "endsite:\n");

		nosite = svrtmp[i].nolabels + nosite;
		}
	}
	}

	fprintf (fd1, "end:\n");
    fclose (fd1);
}
