#include <stdio.h>
#include <stdlib.h>

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
BOX 	mbox;


int 
do_legends (void)
{
    double Mheight();
    FILE *fd1, *fd2, *fd3;
	char	field[1024];
	char 	value[1024];
	char    *ramporientation;
	char	buf[1024];
	double  lasty, lastx;
	double	east, north;
	double	deast, dnorth;
	double	dtmp;
	double  catlastx ; 
	int 	background;
	int 	border;
	int 	vlen;
        int 	x, y, y_apos, x_apos;
	float	textsize;
	int  	textwidth;
	int	scount;
	int	vcount;
	int 	len_name ;
	int 	num_labels;
	char 	longname[128];
	int	sitecols ;
	int	gap;
	int 	isramp;
	int 	iscat;
	int 	catfirst ;
	int     ramptwidth;
	int 	ramptheight;


/* variable for the category */
	int cwidth	 ;
	int cheight	 = 10;
	int xspace = 10;
	int yspace = 0;
	isramp	= 0;
	iscat	= 0;
	textsize= 400.0;
	textwidth= 1;
	yspace= 0;
	xspace= 10;
	east	= fullwindow.west;
	north	= fullwindow.north;
	cwidth	= 20;
	cheight = 10;
	vlen	= 20;
	catfirst = 0;
	ramptheight = 0;
	ramptwidth  = 0;

	scount 	= 0;
	vcount 	= 0;
	len_name = 0;
	num_labels = 0;
	sitecols	= 0;
	gap		= 0;
	catlastx= 0.0;

	fd1 = fopen(legend.other, "r");	
	if (fd1 == NULL)
	   return;

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
	      deast= east;
	continue;
	}

	if (FIELD ("north")) 
	{
	   if (scan_northing (value, &dtmp))
 	      north = dtmp;
	      dnorth= north;
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
	int	   x1;
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

	if (FIELD ("textwidth"))
	{
	   textwidth = atoi(value);
           fprintf (fd2, "width: %d\n", textwidth);
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

	   fprintf (fd2, "rotation: 0\n");
	continue;
	}

        if (FIELD ("cat")) {
	   *value	= 0;
	   *field	= 0;
	   fgets (buf, sizeof buf, fd1) ;
           if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) 
	      continue;
	   fprintf (fd2, "ref: left lower\n");
	   G_plot_where_xy(east, north, &x, &y);
	   x_apos = x + cwidth + xspace;
	   y_apos = y + cheight  ;

	   G_plot_where_en(x_apos, y_apos, &lastx, &lasty);
	   fprintf (fd2, "east: %8.2f \n", lastx);
	   fprintf (fd2, "north: %8.2f \n", lasty);
	   fprintf (fd2, "border: none\n");
	   fprintf (fd2, "background: none\n");
	   fprintf (fd2, "text:%s\n", value);
	   fprintf (fd3, "east: %8.2f \n", east);
	   y_apos = y + gap/2;
	   catlastx = lastx;
	   catfirst = 1;
	   {
	   double tmpn, tmpe;
	   G_plot_where_en(x_apos, y_apos, &tmpe, &tmpn);
	   fprintf (fd3, "north: %8.2f \n", tmpn);
	   }

	   G_squeeze(value);

	   if (*value) 
	   {
	   char *t1, *t2;
	   char textval[100];
	   int len_tmp1, len_tmp2;

	      	len_tmp1 = strlen(value) - 1; 
	      	strcpy (textval, value);
		getd(textval, &t1, &t2);

		len_tmp2 = strlen(t1);

		if (len_tmp1 > len_tmp2) {

		   if (strlen(t2) > len_tmp2)
		      len_tmp2 = strlen(t2);
	
		   if (len_name < len_tmp2) {
		      len_name = len_tmp2;
		      strcpy(longname, value);
		   }
		}
		else 
		   if (len_name < strlen(value)) 
		{
		      len_name = strlen(value); 
		      strcpy (longname,value) ;
		}
	     }

/* cat */
	     num_labels++;

	     fprintf (fd3, "cwidth: %d\n", cwidth);
	     fprintf (fd3, "cheight: %d\n", cheight);
	     fprintf (fd3, "catnum:%s\n", field);
	     G_plot_where_xy(east, north, &x, &y);
	     y_apos = y + cheight + yspace + gap  ;
	     G_plot_where_en(x, y_apos, &lastx, &lasty);
	     north	= lasty;
	     iscat	= 1;
	continue;
	}

	if (FIELD ("ramp") ) {
	FILE *fd3;
	int isvalue = 0;	
	int isvertical = 0;
	char tvalue[100], tfield[30];
	char *val;
	int  valen1, valen2;

            getdata(value, &val,&ramporientation);
	    isvalue = 0; 
	    isramp  = 1;
	    set_text_size(textsize/Mheight());
	    set_text_width(1);
	    set_text_border(-1);
	    set_text_background(-1);
	    set_text_hwidth(0);
	    set_text_rotation(0);
	    set_text_xref(0);
	    set_text_yref(0);
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
            if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) 
	       continue;
	       fprintf (fd2, "ref: left upper\n");
	       G_plot_where_xy(east, north, &x, &y);
	       fprintf (fd2, "border: none\n");
	       fprintf (fd2, "background: none\n");
	       if (isvertical){
		  x_apos = x + cwidth + xspace  ;
		  G_plot_where_en(x_apos, y, &lastx, &lasty);
		  fprintf (fd2, "east: %8.2f \n", lastx);
		  fprintf (fd2, "north: %8.2f \n", north);
		  ramptwidth = cwidth + xspace; 
	       }
	       else {
		  y_apos = y + cheight + yspace  ;
		  G_plot_where_en(x, y_apos, &lastx, &lasty);
		  fprintf (fd2, "east: %8.2f \n", east);
		  fprintf (fd2, "north: %8.2f \n", lasty);
		  ramptheight = cheight + yspace;
		  
	       }

	       if (isvalue)  {
		  fprintf (fd2, "text:%s\n", field);
	          if (len_name < strlen(field) ){
		     len_name = strlen (field);
		     strcpy (longname, field);
		  }
		  text_bounds(field, 0, 0, &rbox, 0);
		
	       }
	       else {
		     fprintf (fd2, "text:%s\n", value);
	             if (len_name < strlen(value) ){
		        len_name = strlen (value);
		        strcpy (longname, value);
		     }
		  text_bounds(value, 0, 0, &rbox, 0);
	       }
	
		valen1 = rbox.right - rbox.left;
	

/* 2nd value */
	       fgets (buf, sizeof buf, fd1) ;
               if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;

	       strcpy (tvalue, value);
	       strcpy (tfield, field);

/* num of categories  */
	       fgets (buf, sizeof buf, fd1) ;
               if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;

	       if (isvertical){
		  fprintf (fd2, "east: %8.2f \n", lastx);
		  G_plot_where_xy(east, north, &x, &y);
		  y_apos = y+2 + cheight * (atoi(value)-1);
		  G_plot_where_en(x, y_apos, &lastx, &lasty);
		  fprintf (fd2, "north: %8.2f \n", lasty);
		  fprintf (fd2, "ref: lower left\n");
		  ramptheight =  2 + cheight *(atoi(value)-1);
		}
		else {
		/* horizontal ramp */
		  fprintf (fd2, "ref: right upper\n"); 
		  fprintf (fd2, "north: %8.2f \n", lasty);
		  G_plot_where_xy(east, north, &x, &y);
		  x_apos = x + cwidth * atoi(value) ;
		  G_plot_where_en(x_apos, y, &lastx, &lasty);
		  fprintf (fd2, "east: %8.2f \n", lastx);
		  fprintf (fd2, "ref: upper right\n");
		  ramptwidth = cwidth * atoi(value); 
		}

		if (isvalue) {
		   fprintf (fd2, "text:%s\n", tfield);

	           if (len_name < strlen(tfield) ){
		      len_name = strlen (tfield);
		      strcpy (longname, tfield);
		  text_bounds(tfield, 0, 0, &rbox, 0);
		   }
		}
		else {
		   fprintf (fd2, "text:%s\n", tvalue);
	           if (len_name < strlen(tvalue) ){
		      len_name = strlen (tvalue);
		      strcpy (longname, tvalue);
		  text_bounds(tvalue, 0, 0, &rbox, 0);
		   }
		}

		valen2 = rbox.right - rbox.left;

		if (valen1 < valen2)
		    valen1 = valen2;

		if (isvertical)
		   ramptwidth = ramptwidth + valen1;
		else
		{
		  text_bounds("M", 0, 0, &mbox, 0);
		  ramptheight = ramptheight + mbox.bottom - mbox.top;
		}

		y_apos = y_apos + yspace ;
		if (!isvertical)
		   y_apos = y_apos + yspace ;
		G_plot_where_en(x, y_apos, 
			&lastx, &lasty);
		
		fprintf (fd3, "east: %8.2f \n", east);
		fprintf (fd3, "north: %8.2f \n", north);
		fprintf (fd3, "rwidth: %d\n", cwidth);
		fprintf (fd3, "rheight: %d\n", cheight);
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
	int x ;
	*value	= 0;
	*field	= 0;
	index	= -1;

	fgets (buf, sizeof buf, fd1) ;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;
	
	index = which_vect(field);

/*
	if (isramp) {
	   G_plot_where_xy(fullwindow.west, fullwindow.north, &x, &y1);
	   G_plot_where_xy(fullwindow.west, fullwindow.north-textsize, &x, &y2);
	}
*/
	if (index  >= 0) {
	   G_plot_where_xy(east, north-textsize/2, &vx, &vy);
	   vy = vy - vector.width[index]/2;
	   drwv[vcount].index = index;
	   drwv[vcount].x= vx;
/*
	   drwv[vcount].y= vy + isramp*yspace+y2-y1+2 ;
*/
	   drwv[vcount].y= vy + 2 ;
	   vcount++;
	   fprintf (fd2, "ref: left upper\n");
	   G_plot_where_xy(east, north, &x, &y);
	   x_apos = x + vlen + xspace;
	   G_plot_where_en(x_apos, (drwv[vcount-1].y
		+vector.hwidth[index]/2), &lastx, &lasty);

/*
	   lasty = lasty - isramp*textsize + textsize/2 ; 
*/
	   lasty = lasty + textsize/2 ; 
	   fprintf (fd2, "east: %8.2f \n", lastx);
	   fprintf (fd2, "north: %8.2f \n", lasty);
	   fprintf (fd2, "border: none\n");
	   fprintf (fd2, "background: none\n");
	   fprintf (fd2, "rotation: 0\n");
	   fprintf (fd2, "text:%s\n", value);
fprintf (stdout,"vect - value=%s \n", value);
	   G_squeeze(value);
	   if (*value) 
	   {
	   char *t1, *t2;
	   char textval[30];
	   int len_tmp1, len_tmp2;

		len_tmp1 = strlen(value) - 1; 
		strcpy (textval, value);
		getd(textval, &t1, &t2);

		len_tmp2 = strlen(t1);

		if (len_tmp1 > len_tmp2) {

		   if (strlen(t2) > len_tmp2)
		      len_tmp2 = strlen(t2);
		   if (len_name < len_tmp2) {
		      len_name = len_tmp2;
		      strcpy(longname, value);
		   }
		}
		else 
		if (len_name < strlen(value)){ len_name =
		   strlen(value); strcpy (longname,value) ;
		}
	    }
/* vect */
	    num_labels++;
	    G_plot_where_xy(east, north-textsize, &x, &y);
	    y_apos = y + yspace + vector.width[index] +
		vector.hwidth[index] + 
		vector.hwidth[index] ;
	    G_plot_where_en(x, y_apos, &lastx, &lasty);
	    north = lasty;
	}
	continue;
	}
	
/* to test for site*/
    if (FIELD ("site")) {
	int index, vx, vy;
	int x1, y1;
	int th;
	*value	= 0;
	*field	= 0;
	index	= -1;

	fgets (buf, sizeof buf, fd1) ;
    if (sscanf (buf, "%[^:]:%[^\n]", field, value) < 1) continue;
	
	index = which_site(field);

	if (index  >= 0) {
	G_plot_where_xy(east, north, &vx, &vy);

	drws[scount].index = index;
	drws[scount].x	= vx + site.icon[index].ncols/2;
	drws[scount].y  = vy + site.icon[index].nrows/2;
	scount++;
	if (sitecols < site.icon[index].ncols)
		sitecols  = site.icon[index].ncols; 


	G_plot_where_xy(east, north, &x, &y);
	x_apos = x + site.icon[index].ncols + xspace;
	G_plot_where_en(x_apos, y+site.icon[index].nrows/2-4, &lastx, &lasty);

	G_squeeze(value);
	fprintf (fd2, "ref: left upper\n");

	if (catlastx>0.0)
	fprintf (fd2, "east: %8.2f \n", catlastx);
	else
	fprintf (fd2, "east: %8.2f \n", lastx);
	fprintf (fd2, "north: %8.2f \n", lasty);
	fprintf (fd2, "border: none\n");
	fprintf (fd2, "background: none\n");
	fprintf (fd2, "rotation: 0\n");
	fprintf (fd2, "text:%s\n", value);

	G_squeeze (value);

	if (*value) {
	char *t1, *t2;
	char textval[30];
	int len_tmp1, len_tmp2;

	len_tmp1 = strlen(value) - 1; 
	strcpy (textval, value);
	getd(textval, &t1, &t2);

	len_tmp2 = strlen(t1);

	if (len_tmp1 > len_tmp2) {

	if (strlen(t2) > len_tmp2)
		len_tmp2 = strlen(t2);
	
	if (len_name < len_tmp2) {
		len_name = len_tmp2;
		strcpy(longname, value);
	}
	}
	else 
	if (len_name < strlen(value)){ len_name =
		strlen(value); strcpy (longname,value) ;
	}
	}

	set_text_size(textsize/Mheight());
	set_text_width (1);
	set_text_border(-1);
	set_text_background(-1);
	set_text_hwidth(0);
	set_text_rotation(0);
	set_text_xref(0);
	set_text_yref(1);
	text_bounds ("M",0,0,&mbox, 0);
	if (site.icon[index].nrows < 
		(mbox.bottom-mbox.top))
	   th= mbox.bottom-mbox.top-site.icon[index].nrows;
	else
	   th=site.icon[index].nrows-(mbox.bottom-mbox.top);

	G_plot_where_xy(east, north-textsize, &x1, &y1);
	if ( (y1-y) < site.icon[index].nrows/2) 
		y1 = y1 + site.icon[index].nrows/2 + 2;
	y_apos = y1 + yspace+th/2;
	G_plot_where_en(x1, y_apos, &lastx, &lasty);

	north = lasty;
	}

	continue;
	}

	if (FIELD ("end")) 
	{
	int x, y, bwidth, bheight;
	int i;

	if ( (background >= 0) || (border >= 0) ) {
	int tmpw, tmph;
	int i, tsiterows;
	int	   x1 ;
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
	text_bounds ("M",0,0,&mbox, 0);

/*
	G_plot_where_xy(fullwindow.west, 
		fullwindow.north, &x1, &y1);
	G_plot_where_xy(fullwindow.west, 
		fullwindow.north-textsize, &x1, &y2);
*/

	tsiterows = 0;
	tmpw	  = 0;
	tmph	  = 0;
	bheight   = 0;
	bwidth	  = 0;
	for (i=0; i<scount; i++) {
		if (site.icon[drws[i].index].nrows> 
			(mbox.bottom-mbox.top) ) 
		tsiterows = site.icon[drws[i].index].nrows+ tsiterows;
		else
		tsiterows = mbox.bottom-mbox.top+tsiterows;
	}

	G_plot_where_xy(deast, dnorth, &x, &y);

	if (site.count > 0)
		tmpw	= sitecols;

	if (tmpw < vlen) tmpw = vlen;

	if (tmpw < cwidth ) tmpw  = cwidth; 

	if (iscat) {
	if (cheight < (mbox.bottom-mbox.top)) 
		tmph = (mbox.bottom-mbox.top);
	else 	tmph = cheight;
	}
	else
	tmph	= mbox.bottom-mbox.top;

	bwidth = (rbox.right-rbox.left)+tmpw+xspace;
/*
        bheight = (num_labels*tmph)+(num_labels-1)*yspace+2+tsiterows+yspace*scount +  vcount*(mbox.bottom-mbox.top) +
vcount*yspace + isramp*yspace; 
*/
fprintf (stdout,"xxxx num_labels=%d\n", num_labels);
fprintf (stdout,"xxxx scount=%d \n", scount);
fprintf (stdout,"xxx tmph= %d \n", tmph);
fprintf (stdout,"xxx yspace = %d \n", yspace);
fprintf (stdout,"isramp=%d \n", isramp);
fprintf (stdout,"tsiterows = %d \n", tsiterows);
        bheight = num_labels*(tmph+yspace)+2+tsiterows+yspace*scount + isramp*yspace + ramptheight; 


	}

fprintf (stdout,"xx ramptheight = %d \n", ramptheight);
	if (bwidth < ramptwidth)
	   bwidth = ramptwidth;

	if (background >=0) {
	int j;
	set_color(background);
	for (j=y-5; j<y+bheight+5; j++)
		draw_line (x-5, j, x+bwidth+5, j);
	}

	if (border >=0) {
	set_color(border);
	draw_line (x-2-5, y-2-5, x+bwidth+2+5, y-2-5); 
	draw_line (x-2-5, y-2-5, x-2-5, y+bheight+2+5);
	draw_line (x+bwidth+2+5, y-2-5, x+bwidth+2+5, y+bheight+2+5);
	draw_line (x-2-5, y+bheight+2+5, x+bwidth+2+5, y+bheight+2+5);

	}
	for (i=0; i<scount; i++) {
		set_color(site.color[drws[i].index]);
		draw_icon(&site.icon[drws[i].index], 
			drws[i].x, drws[i].y);
	}

	
fprintf (stdout,"vcount=%d \n", vcount);
	if (vcount)
	dvect (vcount,vlen);

	scount = 0;
	num_labels = 0;
	len_name = 0;
	isramp =0;
	catlastx = 0.0;
	vcount = 0;
	ramptheight = 0;
	ramptwidth = 0;
	xspace = 10;
	yspace = 0;
	iscat = 0;
	catfirst = 0;

	continue;
	}

}

	fclose(fd1);
	fclose(fd2);

}




static 
which_vect (char *value)
{
int i ;


	for (i=0; i<vector.count; i++){
		if (strcmp (value, vector.name[i]) ==NULL)
			return i;
	}

	return -1;
}

static 
which_site (char *value)
{
int i ;


	for (i=0; i<site.count; i++){
		if (strcmp (value, site.name[i]) ==NULL)
			return i;
	}

	return -1;
}




static 
which_color (char *value)
{
int n;
int r,g,b;

if (!scan_color (value, &n,&r,&g,&b))
n = -1;
return n;
}

static double 
Mheight (void)
{
	BOX box;

		set_text_border(-1);
		set_text_background(-1);
		set_text_width(1);
		set_text_size(100.0);
		text_bounds ("M",0,0,&box, 0);
	    return (fullwindow.ns_res * (box.bottom-box.top+1) / 100.0) ;
}

