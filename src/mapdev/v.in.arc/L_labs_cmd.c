/* L_labs_cmd.c (cmd-line version of GenToLabelledDigLines.c--dks) 
 *
 * function defined:
 *
 * GenToLabelledDigLines(lin_file,txt_file,dig_file,atts_file,cats_filename,
 *        catcol, attcol /new-2/91, dks/) 
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

#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "v_in_arc.cmd.h"

int 
GenToLabelledDigLines (FILE *lin_file, FILE *txt_file, FILE *atts_file, struct Map_info *VectMap, char *cats_filename, int idcol, int catcol, int attcol)
{
	double *xarray, *yarray, *x, *y;
	int   id;
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
	char  tmp[1024], tmpbuf[1024];
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

    Points = Vect_new_line_struct ();

	/* count the columns in the text_file */
	rewind(txt_file);
	/*skip header: 2 fgets here to match GenToLabelledDigLines; */
	/*formerly was buggy here with only one fgets--2-91 dks */
	if (!fgets(txtbuf,512,txt_file)) return (-1);
	if (!fgets(txtbuf,512,txt_file)) return (-1);
	NumCols = CountColumns(txtbuf);
	NumLines = CountLines(txt_file);

	/******EXPUNGED FUNCTION call (by Glen??)

 * determine which column in the txt_file is the line ID
 * number.  this is done by looking for the second column in the
 * file that has a 1 on the 1st line, 2 on the 2nd line, 3 on the
 * 3rd line, and so on...

	rewind(txt_file);
	if ((IDCol=FindIDColumn(txt_file)) < 0)
	   {
	   fprintf (stdout,"\n\nThe LABEL-TEXT file has been scanned.\n");
	   fprintf (stdout,"There is not enough information in the file to assign\n");
	   fprintf (stdout,"to create GRASS attribute and category files\n");
	   return(-1);
	   }
******/

	/*dks: changed reportage of cols from NumCols-1 to NumCols*/
	/* tell the user how many columns were found in the txt_file */
	fprintf (stdout,"\n\nThe LABEL-TEXT file has been scanned. There a are %d\n",NumLines);
	fprintf (stdout,"lines in the file and %d columns in the file\n",NumCols);

	if (NumCols == 2)
	{
		IDCol = 1;  /*I added this, but what do i know?--dks*/
		CatCol = 2;
		AttCol = -1;
		fprintf (stdout,"\nBecause there are only 2 columns, column 2 is assumned to be\n");
		fprintf (stdout,"the category number column\n");
	}
	else if (NumCols < 2)
	{
		G_fatal_error("Too few columns in label-text file");
		exit(-1);
	}
	else
	{
		IDCol = idcol;
		if (IDCol<1 || IDCol>NumCols)
		{
			fprintf(stderr, "\nInvalid number specified for textfile ID column.\n");
			fprintf (stderr, "Re-enter or use interactive version of v.in.arc.\n");
			exit(-1);
		}
		CatCol = catcol;
		if (CatCol<1 || CatCol>NumCols)
		{
			fprintf(stderr, "\nInvalid number specified for textfile Category column.\n");
			fprintf (stderr, "Re-enter or use interactive version of v.in.arc.\n");
			exit(-1);
		}
		if (attcol == 0)
			AttCol = -1;
		else 
		{
			AttCol = attcol;
			if (AttCol<1 || AttCol>NumCols)

			{
				fprintf(stderr, "\nInvalid number specified for textfile Attribute column.\n");
				fprintf (stderr, "Re-enter or use interactive version of v.in.arc.\n");
				exit(-1);
			}
		}
	}

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
	strcpy(head.line_3,"Created by v.in.arc (arc-grass)");
	head.orig_scale = 1;
	head.W = xmin;
	head.E = xmax;
	head.S = ymin;
	head.N = ymax;

	strcpy(head.source_date,"");
	head.digit_thresh = 0;
	head.map_thresh = 0;
	head.plani_zone = 0;

	/* write the dig header to the dig_file */
	/* obsolete: dig_write_head_binary(dig_file, &head); */

	/*DEBUG*/ /*fprintf (stderr, "just before copy_head: W  %f E %f\n", head.W, head.E); */
	Vect_copy_head_data (&head, &(VectMap->head));
	/*DEBUG*/ /*fprintf (stderr, "just after copy_head: W  %f E %f\n", VectMap->head.W, VectMap->head.E); */

	xarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;
	yarray = (double *) dig_falloc(vertex_max, sizeof(double)) ;

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
			CatStat = FindAttAndCat(txt_file,AttCol,CatCol,IDCol,id,AttText,&CatNum);
			if (CatStat < -1)
			{
				G_fatal_error("Reading label-text file");
				exit(-1);
			}

			/* read line's points until an END marker is found */
			x = xarray;
			y = yarray;
			almost_done = 0;
			n_points = 0;
			do {
				if (!fgets(inbuf,1024,lin_file)) return (-1);
				sscanf(inbuf,"%s",tmpbuf);
				strcpy(tmp,inbuf);
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

				if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, n_points))
					G_fatal_error ("Out of memory");

				Vect_write_line (VectMap, type, Points);
			}

			/* set the attribute string in the category structure */
			if (G_set_cat((CELL)CatNum,AttText,&new_cats) != 1)
				G_fatal_error("Do_line_labs: call to G_set_cats");
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
