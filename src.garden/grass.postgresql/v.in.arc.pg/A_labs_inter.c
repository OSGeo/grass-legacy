/* GenToDigAreaLabels.c 
 *
 * function defined:
 *
 * GenToDigAreaLabels(pts_file,txt_file,atts_file,cats_filename)
 *
 *   FILE *pts_file,       - ARC/INFO Generate format label-points file 
 *        *txt_file,       - text file assoc. integer category values
 *                           with lines of descriptive text. 
 *        *atts_file;      - GRASS vector attribute (dig_atts) file
 *   char *cats_filename;  - GRASS vector category (dig_cats) filename
 *
 * PURPOSE: Create a GRASS vector attribute (dig_atts) and a vector
 *          category (dig_cat) file from a label-points file (pts_file)
 *          and a label-text file (txt_file).
 *
 * NOTES: 
 *
 *   1) At the request of Ron Glenn of the USDA-SCS, this routine 
 *      determines which column of the label-text (txt_file) to 
 *      use for label-point-ID numbers by looking for the second
 *      column in the file that has a 1 in the 1st row, 2 in the
 *      second row, 3 in the 3rd row, and so on.  Each label-point
 *      ID number ties a row in the label-text file to a row in the
 *      labels-points file.  This technique is used because one 
 *      cannot count on the first column in the label-text file
 *      being the label-point ID number.
 *  
 *   2) this routine prompts the user to enter the number of the
 *      column in the txt_file that should be used for GRASS
 *      category numbers, and the number of the column that
 *      should be used for attribute text.
 *
 *   3) If the user-specified category column contains any thing
 *      that is not an integer this routine will die gracefully.
 *
 *
 *   4) DKS: 4.0 changes: formerly this routine displayed column numbers    
 *      to the user as NumCols-1 (in effect, treating Rphysical column
 *      #1 as a non-column.  However, since the program counts all columns
 *      in its data processing routines, NumCols itself is displayed.
 *
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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "AtoG.h"
#include "v_in_arc.inter.h"

int GenToDigAreaLabels (FILE *pts_file, FILE *txt_file, FILE *atts_file,
	char *cats_filename)
{
	struct Categories new_cats;
	double  east, north;
	int	id,
	NumCats,
	NumCols,
	NumLines,
	CatStat,
	CatNum,
	IDCol,
	AttCol,
	CatCol,
	done;
	char    txtbuf[512]="";
	char    inbuf[512]="";
	char    tmpbuf[512]="";
	char    AttText[512]="";
	int d_i; /*debug counter*/

    d_i = 0;
#ifdef DEBUG
	fprintf (stdout,"GenToDigAreaLabels\n");
#endif


	NumCats = 1;
	G_init_cats((CELL)NumCats,(char *)NULL,&new_cats);
	
	fprintf (stdout,"\nWorking...\n");
	
	/* LOOP - through the pts_file */
	done = 0;
	while (!done)
	{
		/* read a line */
		if (!fgets(inbuf,1024,pts_file)) return (-1);
		strcpy(tmpbuf, inbuf);
		if (strcmp(G_squeeze(tmpbuf),"END") == 0)
		{
			/* end of file reached */
			done = 1;
		}
                else
		{
		   process_inp(tmpbuf);
		   if (sscanf(tmpbuf,"%d %lf %lf",&id,&east,&north) == 3)
		   {

			CatNum=id;

			/* write point and its category number to the att. file */
			write_att(atts_file,'A',east,north,CatNum);

                   }
		   else
		   {
			/* error */
			G_fatal_error("reading LABEL-POINTS file");
		   }
		}
	}

	sprintf(tmpbuf,"Arc/Info Areas for %s",cats_filename);
	G_set_cats_title(tmpbuf,&new_cats);
	if (G_write_vector_cats(cats_filename,&new_cats) != 1)
		G_fatal_error("Do_area_labs: writing dig_cats file");

	return(0);
}
