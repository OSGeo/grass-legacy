/* GenToLabelledDigLines.c 
 *
 * function defined:
 *
 * GenToLabelledDigLines(lin_file,txt_file,dig_file,atts_file,cats_filename) 
 *
 * NOTES: 
 *
 * 1) the only values set in the dig file header are the N,S,E,
 * and W boundary coordinates.  These boundary coordinates are 
 * determined by finding the max and min coordiate values in the
 * gen file.
 * 
 * 2) Only lines will be written to the dig file, no area edges. 
 *
 *
 * Dave Johnson 
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */
/*	Alex Shevlakov sixote@yahoo.com 02/2000 hack - use dbf for import to postgres
*	and use id=CatNum simple rule for cats (no need of the text_file which is still
*	set to non_NULL to use the original code)
*/
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "v_in_arc.inter.h"

int 
GenToLabelledDigLines (FILE *lin_file, FILE *txt_file, FILE *atts_file, struct Map_info *VectMap, char *cats_filename)
{
	double *xarray, *yarray, *x, *y;
	int   i, id;
	int   done=0;
	int   almost_done=0;
	double xtmp, ytmp, xtmp1, ytmp1;
	int    first=1;
	double xmin,xmax;
	double ymin,ymax;
	int   itmp;
	struct Categories new_cats;
	int   vertex_count=0;
	int   vertex_max=0;
	int    day, yr;
	char  date[25], mon[4];
	char  inbuf[1024];
	int   n_points=0;
	int   n_dig_lines=0;
	int   type = LINE;
	char  tmpbuf[1024], tmp[1024];
	int   CatStat;
	int   CatNum;
	int   NumCats;
	int   NumCols,
	NumLines,
	IDCol,
	AttCol,
	CatCol;
	char  AttText[512];
	char  txtbuf[512];
	char  *G_whoami();

	struct dig_head head;
	struct line_pnts *Points;

#ifdef DEBUG
	fprintf (stdout,"GenToLabelledDigLines\n");
#endif

	AttText[0]=0;

    Points = Vect_new_line_struct();

	
	G_init_cats(0,"",&new_cats);

	fprintf (stdout,"\nWorking...\n");

	/* read through the lines file to find max/min coordinate values
 * and max number of vertices in a line
 */
	rewind(lin_file);
	first=1;
	done=0;
	do {
		if (!fgets(inbuf,1024,lin_file)) return (-1);
		strcpy(tmpbuf, inbuf);
		if ((strcmp(G_squeeze(inbuf),"END") == 0) && almost_done)
			done = 1;
		else if (strcmp(G_squeeze(inbuf),"END") == 0)
		{
			almost_done = 1;
			if (vertex_count > vertex_max)
				vertex_max = vertex_count;
                }
		else
		{
		   process_inp(tmpbuf);
		   if (sscanf(tmpbuf,"%lf %lf",&xtmp,&ytmp)==2)
		   {
			if (first==1)
			{
				xmax=xmin=xtmp;
				ymax=ymin=ytmp;
				first=0;
			}
			if (xtmp > xmax) xmax=xtmp;
			if (xtmp < xmin) xmin=xtmp;
			if (ytmp > ymax) ymax=ytmp;
			if (ytmp < ymin) ymin=ytmp;
/*DEBUG*/ /*fprintf (stderr, "xmax %f xmin %f ymax %f ymin %f\n",
				   xmax, xmin, ymax, ymin); */

			vertex_count++;
			almost_done = 0;
		   }
		   else if (sscanf(tmpbuf,"%d",&itmp)==1)
	   	   {
			vertex_count = 0;
		   }
                }
	}while (!done);

	rewind(lin_file);

	/* build a dig header from the min and max information */
	G_strncpy (date, G_date(), 24);
	sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
	if (yr < 2000) yr = yr - 1900;
	else yr = yr - 2000;
	sprintf(date,"%s %d %d",mon,day,yr);
	strcpy(head.organization," ");
	strcpy(head.date,date);
	strcpy(head.your_name,G_whoami());
	strcpy(head.map_name,cats_filename);
	strcpy(head.line_3,"Created by import.to.vect(arc-grass)");
	head.orig_scale = 1;
	head.W = xmin;
	head.E = xmax;
	head.S = ymin;
	head.N = ymax;

	strcpy(head.source_date,"");
	head.digit_thresh = 0;
	head.map_thresh = 0;
	head.plani_zone = 0;


	Vect_copy_head_data (&head, &(VectMap->head));


	yarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;
	xarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;

	done = 0;
	do {
		/* read until next line id (or and END marker) is found */
		do {
			if (!fgets(inbuf,1024,lin_file)) return (-1);
			strcpy(tmp, inbuf);
			sscanf(inbuf,"%s",tmpbuf);
			if (strcmp(G_squeeze(tmpbuf),"END")==0)
				done = 1;
			process_inp(tmp);
		}   while (sscanf(tmp,"%d",&id)!=1 && !done);

		if (!done)
		{
			/* find the category number for that line-id */
/*******************************
here we hack -A.Sh
********************/
			CatNum=id;
			CatStat=1;
/*******************/
			/* read line's points until an END marker is found */
			x = xarray;
			y = yarray;
			almost_done = 0;
			n_points = 0;
			do {
				if (!fgets(inbuf,1024,lin_file)) return (-1);
				sscanf(inbuf,"%s",tmpbuf);
				strcpy(tmp, inbuf);
				if (strcmp(G_squeeze(tmpbuf),"END")==0)
					almost_done=1;
                                else
				{
				   process_inp(tmp);
				   if (sscanf(tmp,"%lf %lf",&xtmp,&ytmp)==2)
				   {
					*x++ = xtmp;
					*y++ = ytmp;
#           ifdef DEBUG 
					fprintf (stdout,"(%f %f) ",xtmp,ytmp);
#           endif 
					if (n_points == 0)
					{
						xtmp1 = xtmp;
						ytmp1 = ytmp;
					}
					n_points++;
                                   }
				}
				if (CatStat>-1 && n_points == 2 && !almost_done)
				{
					if (xtmp1 < xtmp)
						xtmp = xtmp1 + (xtmp - xtmp1)/2;
					else xtmp = xtmp + (xtmp1 - xtmp)/2;
					if (ytmp1 < ytmp)
						ytmp = ytmp1 + (ytmp - ytmp1)/2;
					else ytmp = ytmp + (ytmp1 - ytmp)/2;
					fprintf(atts_file,"L %12.2f %12.2f %d\n",xtmp,ytmp,CatNum);
				}
			}      while (!almost_done);

			/* write line to the dig file */
			if (n_points > 0)
			{
				n_dig_lines++;
			/*	dig_Write_line(dig_file,(char)type,xarray,yarray,n_points);*/

               for(i=0;i<n_points;i++)

               if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, n_points))
                    G_fatal_error ("Out of memory");

                Vect_write_line (VectMap, type, Points);

			}

		}
	}while (!done);


	sprintf(tmpbuf,"Arc/Info Lines for %s",cats_filename);
	G_set_cats_title(tmpbuf,&new_cats);
	if (G_write_vector_cats(cats_filename,&new_cats) != 1)
		G_fatal_error("Do_line_labs: writing dig_cats file");

    Vect_destroy_line_struct (Points);

	if (n_dig_lines > 0)
		return(0);             /* normal exit */
	else
		return(-1);            /* error - no lines written to dig file */
}
