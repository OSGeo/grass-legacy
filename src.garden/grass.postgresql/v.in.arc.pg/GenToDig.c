
/* GenToDig.c
 *
 * functions defined:
 *
 * GenToDigArea(ascii,VectMap,neatline,filename)
 * FILE *ascii;     - pointer to gen file to be read
 * struct Map_info VectMap - vector map structure containing vector file ptr
 * [superceded by above:FILE *binary;    - pointer to dig file to be written 
 * int  neatline;   - true if neatline is desired
 * char *filename;   - name of digit file
 *
 * GenToDigLine(ascii,VectMap,neatline,filename)
 * FILE *ascii;     - pointer to gen file to be read
 * struct Map_info VectMap - vector map structure containing vector file ptr
 * [superceded by above:FILE *binary;    - pointer to dig file to be written 
 * int  neatline;   - true if neatline is desired
 * char *filename;   - name of digit file
 *
 * GenToDig(type,ascii,VectMap,neatline,filename)
 * char type;	    - either AREA or LINE
 * FILE *ascii;     - pointer to gen file to be read
 * struct Map_info VectMap - vector map structure containing vector file ptr
 * [superceded by above:FILE *binary;    - pointer to dig file to be written 
 * int  neatline;   - true if neatline is desired
 * char *filename;   - name of digit file
 * 
 * PURPOSE:
 * 
 * NOTES: 
 *
 * 1) the only values set in the dig file header are the N,S,E,
 * and W boundary coordinates.  These boundary coordinates are 
 * determined by finding the max and min coordiate values in the
 * gen file.
 *
 *
 * Dave Johnson
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */

#include "config.h"

#include <string.h>
#include <limits.h>
#include "gis.h"
#include "Vect.h"
#include "v_in_arc.h"

/**********************************************************************/
int 
GenToDigArea (FILE *ascii, struct Map_info *VectMap, int neatline, char *filename)
{
#ifdef DEBUG 
	fprintf (stdout,"GenToDigArea\n");
#endif
	return (GenToDig((char)AREA,ascii,VectMap,neatline,filename));
}

/**********************************************************************/
int 
GenToDigLine (FILE *ascii, struct Map_info *VectMap, int neatline, char *filename)
{
#ifdef DEBUG 
	fprintf (stdout,"GenToDigLine\n");
#endif
	return (GenToDig((char)LINE,ascii,VectMap,neatline,filename));
}

/**********************************************************************/
int 
GenToDig (int type, FILE *ascii, struct Map_info *VectMap, int neatline, char *filename)
{
	char   inbuf[1024];
	char   tmpbuf[1024];
	char   tmp[1024];
	char   *G_whoami();
	char   *G_date();
	int    id;
	int    done=0;
	int    almost_done=0;
	int    itmp;
	int    vertex_count=0;
	int    vertex_max= INT_MIN;
	int    n_points=0;
	int    n_dig_lines=0;
	int    day, yr;
	char   date[25], mon[4];
	double xtmp, ytmp;
	double xmin, 
	xmax;
	double ymin, 
	ymax;
	int    first=1;
	double *xarray,
	*yarray,
	*x, *y;

	struct dig_head head;
	struct line_pnts *Points;

	Points = Vect_new_line_struct();

	/* read through the lines file to find max/min coordinate values
 * and max number of vertices in a line
 */
	do {
		if (!fgets(inbuf,1024,ascii)) return (-1);
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
			if (first)
			{
				xmax=xmin=xtmp;
				ymax=ymin=ytmp;
				first=0;
			}
			if (xtmp > xmax) xmax=xtmp;
			if (xtmp < xmin) xmin=xtmp;
			if (ytmp > ymax) ymax=ytmp;
			if (ytmp < ymin) ymin=ytmp;
			vertex_count++;
 /*DEBUG*//* fprintf (stderr, "xmax %f xmin %f ymax %f ymin %f\n",
				  xmax, xmin, ymax, ymin);*/


			almost_done = 0;
		   }
	      	   else if (sscanf(tmpbuf,"%d",&itmp)==1)
		   {
			vertex_count = 0;
		   }
                }
	/*DEBUG*//*fprintf (stderr, "inbuf:  %s\n", inbuf);*/
	}while (!done);
	rewind(ascii);

 /*DEBUG*//* fprintf (stderr, "xmax %f xmin %f ymax %f ymin %f\n",
				  xmax, xmin, ymax, ymin);*/

	/* build a dig header from the min and max information */
	G_strncpy (date, G_date(), 24);
	sscanf(date,"%*s%s%d%*s%d",mon,&day,&yr);
	if (yr < 2000) yr = yr - 1900;
	else yr = yr - 2000;
	sprintf(date,"%s %d %d",mon,day,yr);
	strcpy(head.organization," ");
	strcpy(head.date,date);
	strcpy(head.your_name,G_whoami());
	strcpy(head.map_name,filename);
	strcpy (head.source_date, "");
	strcpy(head.line_3,"Created by v.in.arc");
	head.orig_scale = 0;
	head.W = xmin;
	head.E = xmax;
	head.S = ymin;
	head.N = ymax;
	head.map_thresh = 0;
	head.digit_thresh = 0;
	head.plani_zone=1;	/* TODO*/

	Vect_copy_head_data (&head, &(VectMap->head));

	/* write the dig header to the binary file */
	/*obsolete: dig_write_head_binary(binary, &head); */

	xarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;
	yarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;

	if (neatline)
	{
		x=xarray; 
		y=yarray;
		n_points = 5;
		*x++ = xmin;
		*y++ = ymin;
		*x++ = xmin;
		*y++ = ymax;
		*x++ = xmax;
		*y++ = ymax;
		*x++ = xmax;
		*y++ = ymin;
		*x++ = xmin;
		*y++ = ymin;
/*DKS: BUG fixed: formerly one too few values passed to dig_Write...*/
/*obsolete		dig_Write_line(binary,(char)type,xarray,yarray,n_points);*/

   if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, n_points))
        G_fatal_error ("Out of memory");

    Vect_write_line (VectMap, type, Points);


	}

	done = 0;
	do {
		/* read until next line id (or and END marker) is found */
		do {
			if (NULL == fgets(inbuf,1024,ascii))
			{
			    done = 1;
			    break;
			}
			sscanf(inbuf,"%s",tmpbuf);
			if (strcmp(G_squeeze(tmpbuf),"END")==0)
				done = 1;
			process_inp(tmpbuf);
		}   while (sscanf(tmpbuf,"%d",&id)!=1 && !done);

		if (!done)
		{
			/* read line's points until an END marker is found */
			x = xarray;
			y = yarray;
			almost_done = 0;
			n_points = 0;
			do {
				if (NULL == fgets(inbuf,1024,ascii))
				    break;
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
					n_points++;
                                    }
				}
			}      while (!almost_done);

			/* write line to binary dig file */
			if (n_points > 0)
			{
				n_dig_lines++;
				/*dig_Write_line(binary,(char)type,xarray,yarray,n_points);*/
			    if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, n_points))
					G_fatal_error ("Out of memory");

				Vect_write_line (VectMap, type, Points);
			}
		}
	}while (!done);


	Vect_destroy_line_struct (Points);
	if (n_dig_lines > 0)
		return(0);             /* normal exit */
	else
		return(-1);            /* error - no lines written to dig file */
}
