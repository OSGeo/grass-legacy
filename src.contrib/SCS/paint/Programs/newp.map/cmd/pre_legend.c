#include "gis.h"
#include "labels.h"
#include "cats.h"
#include "misc.h"
#include "fullwindow.h"
#include "parms.h"
#include "clegend.h"
#include "text.h"
#include "vector.h"
#include "sites.h"

#define FIELD(x) strcmp(x,field)==0

pre_legend(pcats,statf)
struct Categories *pcats;
struct Cell_stats *statf;
{
    FILE *fd1, *fd2;
	char	field[1024];
	char 	value[1024];
	char	buf[1024];
	char east[50], north[50];
	double	dtmp;
	int 	border;
	int 	color;
    int 	x, y, y_apos, x_apos;
	int		border_legend;
	int 	len_name = 0;
	char	long_name[128];
	int 	num_labels = 0;
	int 	ebackground = -1;
	int 	eborder		= -1;
	double  deast, dnorth;
	double  tmpeast, tmpnorth;
	int		allrast, allvect, allsite;
	int 	isramp;


/* variable for the category */
	char	catname[1024];
	int  vlen;
	int  cwidth;
	int  cheight;
	int  textcolor;
	int  xspace;
	int  yspace;
	char fontname[128];
	float textsize;


	vlen	 = 20;
	cwidth	 = 20;
	cheight	 = 10;
	textcolor	 = BLACK;

	deast 	= fullwindow.west;
	dnorth	= fullwindow.north;

	textsize	= 400.0;
	xspace	= 10;
	yspace 	= 0;

	allvect	= 1;
	allrast = 1;
	allsite = 1;
	isramp	= 0;

	fd1 = fopen(prelegend.other, "r");	
	if (fd1 == NULL)
	{
		return;
	}

	if (legend.other == NULL)
	{
	legend.other = G_tempfile();
	if ((fd2 = fopen (legend.other, "w")) != NULL)
		fclose (fd2);
	}

	fd2 = fopen (legend.other, "a");
	if (fd2 == NULL)
	{
		error ("misc legend file", "", "can't open");
		return;
	}





	while (fgets (buf, sizeof buf, fd1)) 
	{ 
	*value	= 0;
	*field	= 0;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;


	if (FIELD ("east")) 
	{
	if (!scan_easting (value, &dtmp))
		continue;

	deast	= dtmp;
	if ( (tmpeast != deast) && (tmpnorth != dnorth)) {
		tmpeast =deast ;
		tmpnorth = dnorth;
		}

	fprintf (fd2, "east:%s\n", value);
	}

	if (FIELD ("north")) 
	{
	if (!scan_northing (value, &dtmp))
		continue;

	dnorth 	= dtmp;
	fprintf (fd2, "north:%s\n", value);
	}


	if (FIELD("yspace"))
	{
	yspace = atoi(value) ;
	fprintf (fd2, "yspace: %d\n", atoi(value));

	}

	if (FIELD("xspace"))
	{
	xspace = atoi(value) ;
	fprintf (fd2, "xspace: %d\n", atoi(value));
	}





	if (FIELD ("textsize"))
	{
		if (scan_resolution (value, &dtmp));
			textsize = dtmp;

		fprintf (fd2, "textsize: %8.2f\n", textsize);
	}


	if (FIELD ("font"))
	{
		fprintf(fd2, "font: %s\n", value);
	}

/*
	if (FIELD ("border")){
	border = which_color(value);
	fprintf (fd2, "border: %d\n", border);
	}
	*/


	if (FIELD ("width"))
	{
		cwidth	= atoi(value);
		fprintf (fd2,"width:%d\n", cwidth);
	}

	if (FIELD ("vlen"))
	{
		vlen = atoi(value);
	}



	if (FIELD ("height"))
	{
		cheight = atoi(value);
		fprintf (fd2, "height:%d\n", cheight);
	}

	if (FIELD ("textcolor"))
	{
		textcolor = atoi(value);
		fprintf (fd2, "textcolor: %d\n", textcolor);
	}





	if (FIELD ("font"))
	{
		strcpy (fontname, value);
		fprintf (fd2, "font: %s\n", fontname);
		continue;
	}

	if (FIELD ("border"))
	{
	eborder = which_color(value);
	if (eborder < 0)
		fprintf (fd2, "border: none\n");
	else
		fprintf (fd2, "border: %d\n", eborder);
	continue;
	}

	if (FIELD ("background"))
	{

	ebackground = which_color(value);
	if (ebackground < 0)
		fprintf (fd2, "background: none\n");
	else
		fprintf (fd2, "background: %d\n", ebackground);
	continue;
	}


	if (FIELD ("beginrast")) 
	{ 
	allrast = 1;
	fprintf (fd2, "beginrast:\n");
	continue;
	}


	if (FIELD("cat")){
	*value 	= 0;
	*field	= 0;
	fgets(buf, sizeof buf, fd1);
	if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;

	fprintf (fd2, "cat:\n");
	fprintf (fd2, "%d:%s\n", atoi(field), value); 
	allrast	= 0;

	continue;
	}
	
	if (FIELD("ramp")) {
		isramp = 1;
		fprintf (fd2, "ramp:%s\n", value);
		continue;
		}
		


	if (FIELD("vect")){
	*value 	= 0;
	*field	= 0;
	fgets(buf, sizeof buf, fd1);
	if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;

	fprintf (fd2, "vect:\n");
	fprintf (fd2, "%s: %s\n", field, value); 
	allvect	= 0;

	continue;
	}

	if (FIELD("site")){
	*value 	= 0;
	*field	= 0;
	fgets(buf, sizeof buf, fd1);
	if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;

	fprintf (fd2, "site:\n");
	fprintf (fd2, "%s: %s\n", field, value); 
	allsite	= 0;

	continue;
	}




	if (FIELD ("endrast")) 
	{
	int last_catnum;
	char *name;
	if (allrast){
	int catnum;
	int numofcats;
	long count;
	int i;

	i = 0;
	last_catnum	= 0;
	numofcats	= 0;
	G_rewind_cell_stats(statf);
	while (G_next_cell_stat(&catnum, &count, statf) )
	{
		name = G_get_cat(catnum, parms.pcats);
		if (isramp)  { 
		if (i==0) {
		fprintf (fd2,"%d:%s\n", catnum, name);
		i = 1;
		numofcats++;
		}
		numofcats++;
		last_catnum = catnum;
		}
		else {
		fprintf (fd2, "cat:\n");
		fprintf (fd2,"%d:%s\n", catnum, name);
		}


	}
	if (isramp) {
	printf (" numofcatst is %d\n", numofcats);
		name = G_get_cat(last_catnum, parms.pcats);
		fprintf (fd2,"%d:%s\n", last_catnum, name);
		fprintf (fd2,"numofcats:%d\n", numofcats);
		}
		

	}

	fprintf (fd2, "endrast:\n");
	continue;
	}

	if (FIELD ("beginvect")) 
	{ int i;

	fprintf (fd2, "vlen: %d\n", vlen);
	fprintf (fd2, "beginvect:\n");
	continue;
	}

	if (FIELD ("endvect")) 
	{
	int i;
	if (allvect) {
	for (i=0; i<vector.count; i++)
	{
	char *name;
	fprintf (fd2, "vect:\n");
	fprintf (fd2,"%s:\n", vector.name[i]);
	}
	}

	fprintf (fd2, "endvect:\n");
	continue;
	}
	

	if (FIELD ("beginsite"))
	{int i;

/*
	for (i=0; i<site.count; i++)
	{
	fprintf (fd2, "beginsite:\n");
	fprintf (fd2, "site:\n");
	fprintf (fd2,"%s:\n", site.name[i]);
	}
	*/
	continue;
	}

	if (FIELD ("endsite")) 
	{
	int i;
	if (allsite) {
	for (i=0; i<site.count; i++)
	{
	fprintf (fd2, "beginsite:\n");
	fprintf (fd2, "site:\n");
	fprintf (fd2,"%s:\n", site.name[i]);
	}
	}


	fprintf (fd2, "endsite:\n");
	continue;
	}

	if (FIELD ("end"))
	{

	fprintf (fd2, "end:\n");
	continue;
	}


	
}

	fclose(fd1);
	fclose(fd2);

}

static
which_color (value)
char *value;
{
int n;
int r,g,b;

if (!scan_color (value, &n,&r,&g,&b))
n = -1;
return n;
}

