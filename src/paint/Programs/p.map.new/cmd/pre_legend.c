#include "gis.h"
#include <stdlib.h>
#include <string.h>
#include "labels.h"
#include "cats.h"
#include "misc.h"
#include "fullwindow.h"
#include "parms.h"
#include "clegend.h"
#include "text.h"
#include "vector.h"
#include "sites.h"
#include "local_proto.h"

#define FIELD(x) strcmp(x,field)==0

static int which_color (char *);

int pre_legend (struct Cell_stats *statf)
{
    FILE *fd1, *fd2;
	char	field[1024];
	char 	value[1024];
	char	buf[1024];
	double	dtmp;
	DCELL dmax, dmin;
	int 	ebackground = -1;
	int 	eborder		= -1;
	double  deast, dnorth;
	double  tmpeast = 0.0L, tmpnorth = 0.0L;
	int		allrast, allvect, allsite;
	int 	isramp;
	int 	yspace, xspace;


/* variable for the category */
	int  vlen;
	int  cwidth;
	int  cheight;
	int  textcolor;
	char fontname[128];
	int  textwidth;


	vlen	 = 20;
	cwidth	 = 20;
	cheight	 = 10;
	textcolor	 = BLACK;

	deast 	= fullwindow.west;
	dnorth	= fullwindow.north;

        textwidth = 1;

	allvect	= 1;
	allrast = 1;
	allsite = 1;
	isramp	= 0;

	fd1 = fopen(prelegend.other, "r");	
	if (fd1 == NULL)
	{
	   return 1;
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
	   return 1;
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
           continue;
	}

	if (FIELD ("north")) 
	{
	   if (!scan_northing (value, &dtmp))
		continue;

	   dnorth 	= dtmp;
	   fprintf (fd2, "north:%s\n", value);
           continue;
	}

	if (FIELD("yspace"))
	{
	   yspace = atoi(value) ;
	   fprintf (fd2, "yspace: %d\n", atoi(value));
           continue;
	}

	if (FIELD("xspace"))
	{
	   xspace = atoi(value) ;
	   fprintf (fd2, "xspace: %d\n", atoi(value));
           continue;
	}

	if (FIELD ("textsize"))
	{
	   double x;
	   if (!scan_resolution(value,&x))
 	       x = 0.0; 
	   else
 	       fprintf (fd2, "textsize: %s\n", value);
           continue;
	}

	if (FIELD ("textwidth") )
	{
	   textwidth = atoi(value);	
	   fprintf (fd2, "textwidth: %d\n", textwidth);
           continue;
	}

	if (FIELD ("textfont"))
	{
	   fprintf(fd2, "font: %s\n", value);
           continue;
	}

	if (FIELD ("width"))
	{
	   cwidth	= atoi(value);
	   fprintf (fd2,"width:%d\n", cwidth);
           continue;
	}

	if (FIELD ("vlen"))
	{
	   vlen = atoi(value);
           continue;
	}


	if (FIELD ("height"))
	{
	   cheight = atoi(value);
	   fprintf (fd2, "height:%d\n", cheight);
           continue;
	}

	if (FIELD ("textcolor"))
	{
	   textcolor = atoi(value);
	   fprintf (fd2, "textcolor: %d\n", textcolor);
           continue;
	}

	if (FIELD ("textfont"))
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

	   G_squeeze (value);
	   if (!*value)
	      fprintf (fd2, "%s:  \n", field); 
	   else 
	      if (strcmp (value, "label") == 0) 
	      {
	        char *name1, *name2;
	        if(sscanf(field, "%lf-%lf", &dmin, &dmax) == 2 && dmax != dmin)
		{
		   name1 = G_get_d_raster_cat(&dmin, &parms.pcats);
		   name2 = G_get_d_raster_cat(&dmax, &parms.pcats);
	           fprintf (fd2, "%s:from %s to %s\n", field, name1, name2); 
                }
		else
		{
		   sscanf(field, "%lf", &dmin);
		   name1 = G_get_d_raster_cat(&dmin, &parms.pcats);
		   fprintf (fd2, "%s:%s\n", field, name1);
                }
	      }
	      else
	        fprintf (fd2, "%s:%s\n", field, value); 
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
	   G_squeeze (value);
	   if (!*value)
	      fprintf (fd2, "%s:  \n", field ); 
	   else
	      if (strcmp (value, "title") == 0) {
	        char *mapset;
	        char *name;
	        int stat;
	        struct Categories vcats;
	        mapset = G_find_vector (field, "");
	        stat = G_read_vector_cats(field, mapset, &vcats);
	        if (stat >= 0) { 
	          name = G_get_cats_title(&vcats);
	          fprintf (fd2, "%s:%s\n", field, name ); 
	        }
	      }
	      else
	        fprintf (fd2, "%s:%s   \n", field, value); 

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
	   G_get_stats_for_null_value(&count, statf);
	   if(count)
	   {
		if (!isramp) 
		    fprintf (fd2, "cat:\n");
	        fprintf (fd2,"nv:no data\n");
		numofcats++;
	   }
	   while (G_next_cell_stat(&catnum, &count, statf) )
	   {
		if(parms.map_type == CELL_TYPE)
		   name = G_get_cat(catnum, &parms.pcats);
                else
		   name = G_get_ith_d_raster_cat(&parms.pcats, catnum, &dmin, &dmax);
		if (isramp)  { 
		   if (i==0) {
		    if(parms.map_type == CELL_TYPE)
		         fprintf (fd2,"%d:%s\n", catnum, name);
                    else
		         fprintf (fd2,"%.10f-%.10f:%s\n", dmin, dmax, name);
		     i = 1;
		     numofcats++;
		   }
		   numofcats++;
		   last_catnum = catnum;
		}
		else {
		    fprintf (fd2, "cat:\n");
		    if(parms.map_type == CELL_TYPE)
		         fprintf (fd2,"%d:%s\n", catnum, name);
                    else
		         fprintf (fd2,"%.10f-%.10f:%s\n", dmin, dmax, name);
		}
	    }
	    if (isramp) {
		if(parms.map_type == CELL_TYPE)
		{
		   name = G_get_cat(last_catnum, &parms.pcats);
		   fprintf (fd2,"%d:%s\n", last_catnum, name);
                }
                else
		{
		   name = G_get_ith_d_raster_cat(&parms.pcats, last_catnum, &dmin, &dmax);
		   fprintf (fd2,"%.10f-%.10f:%s\n", dmin, dmax, name);
                }
		fprintf (fd2,"numofcats:%d\n", numofcats);
	    }
		
	    }

	    fprintf (fd2, "endrast:\n");
	    continue;
	}

	if (FIELD ("beginvect")) 
	{ 
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
	        fprintf (fd2, "vect:\n");
	        fprintf (fd2,"%s:%s\n", vector.name[i], vector.name[i]);
	     }
	   }
	   fprintf (fd2, "endvect:\n");
	   continue;
	}
	
	if (FIELD ("beginsite"))
	{
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
	        fprintf (fd2,"%s:%s   \n", site.name[i], site.name[i]);
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

	return 0;
}

static int which_color (char *value)
{
int n;
int r,g,b;

if (!scan_color (value, &n,&r,&g,&b))
n = -1;
return n;
}
