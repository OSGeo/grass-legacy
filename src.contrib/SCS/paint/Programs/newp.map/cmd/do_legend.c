#include "labels.h"
#include "gis.h"
#include "cats.h"
#include "misc.h"
#include "parms.h"
#include "clegend.h"
#include "fullwindow.h"
#include "vector.h"
#include "sites.h"
#include "drawsitevect.h"
#include "text.h"
#include "ramp.h"

#define FIELD(x) strcmp(x,field)==0
BOX	rbox;


do_legends ()
{
	double Mheight();
	
    FILE *fd1, *fd2, *fd3;
	char title[1024];
	char	field[1024];
	char 	value[1024];
	char    *ramporientation;
	char	buf[1024];
	double lasty, lastx;
	double	east, north;
	double	deast, dnorth;
	double	dtmp;
	int 	background;
	int 	border;
	int 	color;
	int		hcolor;
	int 	width;
	int 	vlen;
    int 	x, y, y_apos, x_apos;
	int		border_legend;
	float	textsize;
	int		scount;
	int		vcount;
	int 	len_name ;
	int 	num_labels;
	char 	fontname[128];
	char 	longname[128];
	int		sitecols ;
	int		gap;
	int 	numofcats;
	int 	isramp;


/* variable for the category */
	char	catname[1024];
	int cwidth	 ;
	int cheight	 = 10;
	int last_cheight ;
	int xspace = 10;
	int yspace = 0;
	int catnum;	


	numofcats 	= 0;
	isramp		= 0;
	textsize	= 400.0;
	yspace= 0;
	xspace= 10;
	east	= fullwindow.west;
	north	= fullwindow.north;
	cwidth	= 20;
	cheight = 10;
	vlen	= 20;

	scount 	= 0;
	vcount 	= 0;
	len_name = 0;
	num_labels = 0;
	sitecols	= 0;
	gap		= 0;

	fd1 = fopen(legend.other, "r");	
	if (fd1 == NULL)
	{
		return;
	}

	if (labels.other == NULL)
	{
	labels.other = G_tempfile();
	if ((fd2 = fopen (labels.other, "w")) != NULL)
		fclose (fd2);
	}

	fd2 = fopen (labels.other, "a");
	if (fd2 == NULL)
	{
		error ("misc labels file", "", "can't open");
		return;
	}




	while (fgets (buf, sizeof buf, fd1)) 
	{ 
	*value	= 0;
	*field	= 0;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;


	if (FIELD ("east")) 
	{
	if (scan_easting (value, &dtmp))
	   east = dtmp;
	deast	= east;
	continue;
	}

	if (FIELD ("north")) 
	{
	if (scan_northing (value, &dtmp))
 	   north = dtmp;
	dnorth	= north;
	continue;
	}

	if (FIELD("yspace"))
	{
		yspace  = atoi(value) ;
		fprintf(fd2, "yspace: %d\n", yspace);
		continue;
	}

	if (FIELD("xspace"))
	{
		xspace  = atoi(value) ;
		fprintf(fd2, "xspace: %d\n", xspace);
		continue;
	}



	if (FIELD ("textcolor"))
	{
	fprintf(fd2, "color: %s\n", value);
	continue;
	}

	if (FIELD ("background"))
	{
		background = which_color(value);
		continue;
	}

	if (FIELD ("border"))
	{
	int	   x1, x2;
	int	   y1, y2;
		border = which_color(value);

	
	G_plot_where_xy(fullwindow.west, fullwindow.north, &x1, &y1);
	G_plot_where_xy(fullwindow.west, fullwindow.north-textsize, &x1, &y2);




	if (cheight < y2-y1) gap = y2-y1-cheight;


		continue;
	}


	if (FIELD ("width"))
	{
		cwidth	 = atoi(value);
		continue;
	}

	if (FIELD ("height"))
	{
		cheight = atoi(value);
		continue;
	}

	if (FIELD ("textsize"))
	{
		if (scan_resolution (value, &dtmp));
			textsize = dtmp;

		fprintf (fd2, "size: %8.2f\n", textsize);
		continue;
	}


	if (FIELD ("font"))
	{
		fprintf(fd2, "font: %s\n", value);
		G_strip (value);
		select_font (value);
		continue;
	}


	if (FIELD ("beginrast")) {
	if (cats.other == NULL)
	{
	cats.other = G_tempfile();
	if ((fd3 = fopen (cats.other, "w")) != NULL)
		fclose (fd3);
	}

	fd3 = fopen (cats.other, "a");
	if (fd3 == NULL)
	{
		error ("misc cats file", "", "can't open");
		return;
	}
			continue;
		}

    if (FIELD ("cat")) {
	*value	= 0;
	*field	= 0;
	fgets (buf, sizeof buf, fd1) ;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;
		fprintf (fd2, "ref: left upper\n");
		G_plot_where_xy(east, north, &x, &y);
		x_apos = x + cwidth + xspace;
		G_plot_where_en(x_apos, y, &lastx, &lasty);
		fprintf (fd2, "east: %8.2f \n", lastx);
		fprintf (fd2, "north: %8.2f \n", lasty);
		fprintf (fd2, "border: none\n");
		fprintf (fd2, "background: none\n");
		fprintf (fd2, "text:%s\n", value);

		if (len_name < strlen(value)){ len_name =
		strlen(value); strcpy (longname,value) ;
		}


		num_labels++;

		fprintf (fd3, "east: %8.2f \n", east);
		{
		double tmpn, tmpe;
		y_apos = y + gap/2;
		G_plot_where_en(x_apos, y_apos, &tmpe, &tmpn);

		fprintf (fd3, "north: %8.2f \n", tmpn);
		}
		/*
		fprintf (fd3, "north: %8.2f \n", north);
		*/
		fprintf (fd3, "cwidth: %d\n", cwidth);
		fprintf (fd3, "cheight: %d\n", cheight);
		fprintf (fd3, "catnum:%d\n", atoi(field));

		G_plot_where_xy(east, north, &x, &y);
		y_apos = y + cheight + yspace + gap  ;
		G_plot_where_en(x, y_apos, &lastx, &lasty);


		north	= lasty;
		continue;
	}

	if (FIELD ("ramp") ) {
	int x1, y1;
	FILE *fd3;
	int isvalue = 0;	
	int isvertical = 0;
	char tvalue[30], tfield[30];
	char *val;

printf (" in ramp - value is %s\n", value);
printf (" xxxx \n");
    getdata(value, &val,&ramporientation);

printf (" in ramp - value is %s\n", value);
printf (" in ramp - ramporientation is %s\n", ramporientation);

	isvalue = 0; 
	isramp  = 1;
	if (ramp.other == NULL)
	{
	ramp.other = G_tempfile();
	if ((fd3 = fopen (ramp.other, "w")) != NULL)
		fclose (fd3);
	}

	fd3 = fopen (ramp.other, "a");
	if (fd3 == NULL)
	{
		error ("misc labels file", "", "can't open");
		return;
	}
		fprintf (fd3, "orientation:%s\n", ramporientation);

	if (strcmp (ramporientation, "vertical") == NULL)
		isvertical = 1;
	if (strcmp (val, "value") == NULL)

		isvalue = 1;

/* first value */

	fgets (buf, sizeof buf, fd1) ;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;
		fprintf (fd2, "ref: left upper\n");

		fprintf (fd2, "border: white\n");

		G_plot_where_xy(east, north, &x, &y);
		if (isvertical){
		x_apos = x + cwidth + xspace  ;
		G_plot_where_en(x_apos, y, &lastx, &lasty);
		fprintf (fd2, "east: %8.2f \n", lastx);
		fprintf (fd2, "north: %8.2f \n", north);
		}
		else {
		y_apos = y + cwidth + yspace  ;
		G_plot_where_en(x, y_apos, &lastx, &lasty);
		fprintf (fd2, "east: %8.2f \n", east);
		fprintf (fd2, "north: %8.2f \n", lasty);
		}


		if (isvalue)  {
			fprintf (fd2, "text:%s\n", field);
	if (len_name < strlen(field) ){
		len_name = strlen (field);
		strcpy (longname, field);
		}
			}
		else {
			fprintf (fd2, "text:%s\n", value);


	if (len_name < strlen(value) ){
		len_name = strlen (value);
		strcpy (longname, value);
		}
			}

/* 2nd value */
	fgets (buf, sizeof buf, fd1) ;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;

		strcpy (tvalue, value);
		strcpy (tfield, field);

/* num of categories  */
	fgets (buf, sizeof buf, fd1) ;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;
		numofcats = atoi(value);
		if (isvertical){
		fprintf (fd2, "east: %8.2f \n", lastx);
		G_plot_where_xy(east, north+textsize*1.5, &x, &y);
		y_apos = y + 2 * atoi(value) ;
		G_plot_where_en(x, y_apos, &lastx, &lasty);
		fprintf (fd2, "north: %8.2f \n", lasty);
		}

		else {
		fprintf (fd2, "ref: right upper\n"); 
		fprintf (fd2, "north: %8.2f \n", lasty);
		G_plot_where_xy(east, north, &x, &y);
		x_apos = x + 2 * atoi(value) ;

		G_plot_where_en(x_apos, y, &lastx, &lasty);
		fprintf (fd2, "east: %8.2f \n", lastx);
		}

		if (isvalue) {
			fprintf (fd2, "text:%s\n", tfield);
	if (len_name < strlen(tfield) ){
		len_name = strlen (tfield);
		strcpy (longname, tfield);
		}
		}
		else {
			fprintf (fd2, "text:%s\n", tvalue);
	if (len_name < strlen(tvalue) ){
		len_name = strlen (tvalue);
		strcpy (longname, tvalue);
		}
		}


		y_apos = y_apos + yspace ;
		G_plot_where_en(x, y_apos, &lastx, &lasty);


		
		fprintf (fd3, "east: %8.2f \n", east);
		fprintf (fd3, "north: %8.2f \n", north);
		fprintf (fd3, "rwidth: %d\n", cwidth);
		fprintf (fd3, "ramp:\n");


		fclose (fd3);
		north	= lasty;
		

	continue;
	}
		


	if (FIELD ("vlen") )
	{
		vlen	= atoi(value);
		continue;
	}

	if (FIELD ("endrast") ) {
		fclose (fd3);
		continue;
		}


/* to test for vectors */
    if (FIELD ("vect")) {
	int index, vx, vy;
	*value	= 0;
	*field	= 0;
	index	= -1;

	fgets (buf, sizeof buf, fd1) ;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;
	
	index = which_vect(field);

	if (index  >= 0) {
	G_plot_where_xy(east, north-textsize/2, &vx, &vy);

	vy = vy - vector.width[index]/2;

	drwv[vcount].index = index;
	drwv[vcount].x	   = vx;
	drwv[vcount].y	   = vy;
	vcount++;

	fprintf (fd2, "ref: left upper\n");
	G_plot_where_xy(east, north, &x, &y);
	x_apos = x + vlen + xspace;
	G_plot_where_en(x_apos, y, &lastx, &lasty);

	fprintf (fd2, "east: %8.2f \n", lastx);
	fprintf (fd2, "north: %8.2f \n", lasty);
	fprintf (fd2, "border: none\n");
	fprintf (fd2, "background: none\n");
	fprintf (fd2, "text:%s\n", vector.name[index]);
	
	if (len_name < strlen(vector.name[index]) ){
		len_name = strlen (vector.name[index]);
		strcpy (longname, vector.name[index]);
		}

	num_labels++;

	G_plot_where_xy(east, north-textsize, &x, &y);
	y_apos = y + yspace;
	G_plot_where_en(x, y_apos, &lastx, &lasty);

	north = lasty;

	}
	continue;
	}
	
/* to test for site*/
    if (FIELD ("site")) {
	int index, vx, vy;
	int x1, y1;
	int itmp;
	int count;
	*value	= 0;
	*field	= 0;
	index	= -1;

	fgets (buf, sizeof buf, fd1) ;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;
	
	index = which_site(field);

	if (index  >= 0) {
	int ii, n, z;
	G_plot_where_xy(east, north, &vx, &vy);


	drws[scount].index = index;
	drws[scount].x	= vx + site.icon[index].ncols/2;
	drws[scount].y  = vy + site.icon[index].nrows/2;
	scount++;
	if (sitecols < site.icon[index].ncols)
		sitecols  = site.icon[index].ncols; 


	G_plot_where_xy(east, north, &x, &y);
	x_apos = x + site.icon[index].ncols + xspace;
	G_plot_where_en(x_apos, y+site.icon[index].ncols/2-2, &lastx, &lasty);

	fprintf (fd2, "ref: left upper\n");
	fprintf (fd2, "east: %8.2f \n", lastx);
	fprintf (fd2, "north: %8.2f \n", lasty);
	fprintf (fd2, "border: none\n");
	fprintf (fd2, "background: none\n");
	fprintf (fd2, "text:%s\n", site.name[index]);
	if (len_name < strlen(site.name[index]) ) {
		len_name = strlen (site.name[index]);
		strcpy (longname, site.name[index]);
		}


	G_plot_where_xy(east, north-textsize, &x1, &y1);
	itmp	= y1 - y;
	if ( (y1-y) < site.icon[index].nrows/2) 
		y1 = y1 + site.icon[index].nrows/2 + 2;
	y_apos = y1 + yspace;
	G_plot_where_en(x1, y_apos, &lastx, &lasty);

	north = lasty;
	}

	continue;
	}

	if (FIELD ("end")) 
	{
	int x, y, bwidth, bheight;
	int i, j;
	int rheight;

	if ( (background >= 0) || (border >= 0) ) {
	int tmpw, tmph;
	int i, tsiterows;
	int	   x1, x2;
	int	   y1, y2;
	set_text_size(textsize/Mheight());
	set_text_width (1);
	set_text_border(-1);
	set_text_background(-1);
	set_text_hwidth(0);
	set_text_rotation(0);
	set_text_xref(0);
	set_text_yref(1);
	text_bounds(longname, 0, 0, &rbox, 0);

	G_plot_where_xy(fullwindow.west, fullwindow.north, &x1, &y1);
	G_plot_where_xy(fullwindow.west, fullwindow.north-textsize, &x1, &y2);

	tsiterows = 0;
	tmpw	  = 0;
	tmph	  = 0;
	for (i=0; i<scount; i++) {
		if (site.icon[drws[i].index].nrows> 
			(rbox.bottom-rbox.top) ) 
		tsiterows = site.icon[drws[i].index].nrows+ tsiterows;
		else
		tsiterows = rbox.bottom-rbox.top+tsiterows;
	}

	G_plot_where_xy(deast, dnorth, &x, &y);

	if (site.count > 0)
		tmpw	= sitecols;

	if (tmpw < vlen) tmpw = vlen;

	if (tmpw < cwidth ) tmpw  = cwidth; 

	if (cheight < y2-y1) tmph = y2-y1;
	else tmph = cheight;


	rheight = numofcats * 2;

	if (isramp)
	bwidth = tmpw;
	else
	bwidth = (rbox.right-rbox.left)+tmpw+xspace;
bheight = (num_labels*tmph)+(num_labels-1)*yspace+2+tsiterows+rheight; 
	}

	if (background >=0) {
	int j;
	set_color(background);
	for (j=y; j<y+bheight; j++)
		draw_line (x, j, x+bwidth, j);
	}

	if (border >=0) {
	set_color(border);
	draw_line (x-2, y-2, x+bwidth+2, y-2); 
	draw_line (x-2, y-2, x-2, y+bheight+2);
	draw_line (x+bwidth+2, y-2, x+bwidth+2, y+bheight+2);
	draw_line (x-2, y+bheight+2, x+bwidth+2, y+bheight+2);

	}
	for (i=0; i<scount; i++) {
		set_color(site.color[drws[i].index]);
		draw_icon(&site.icon[drws[i].index], 
			drws[i].x, drws[i].y);
	}

	
	if (vcount)
	dvect (vcount,vlen);

	scount = 0;
	num_labels = 0;
	len_name = 0;



	continue;
	}



}

	fclose(fd1);
	fclose(fd2);



}




static which_vect(value)
char *value;
{
int i ;


	for (i=0; i<vector.count; i++){
		if (strcmp (value, vector.name[i]) ==NULL)
			return i;
	}

	return -1;
}

static which_site(value)
char *value;
{
int i ;


	for (i=0; i<site.count; i++){
		if (strcmp (value, site.name[i]) ==NULL)
			return i;
	}

	return -1;
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

static
double
Mheight()
{
	BOX box;

		set_text_border(-1);
		set_text_background(-1);
		set_text_width(1);
		set_text_size(100.0);
		text_bounds ("M",0,0,&box, 0);
	    return (fullwindow.ns_res * (box.bottom-box.top+1) / 100.0) ;
}

