/* update by W. Droege 12/99
 * by Werner Droege <Werner.Droege@mailbox.tu-dresden.de>
 * and Alex Shevlakov <sixote@yahoo.com>
 */

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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
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
	char    txtbuf[512];
	char    inbuf[512];
	char    tmpbuf[512];
	char    AttText[512];
	int d_i; /*debug counter*/

    d_i = 0;
#ifdef DEBUG
	fprintf (stdout,"GenToDigAreaLabels\n");
#endif

	AttText[0]=0;

	/* count the columns in the text_file */
	rewind(txt_file);
	if (!fgets(txtbuf,512,txt_file)) return (-1);  /* skip headers record */
	if (!fgets(txtbuf,512,txt_file)) return (-1);  /* skip headers record */
	NumCols = CountColumns(txtbuf);
	NumLines = CountLines(txt_file);

	/** FUNCTION CALL EXPUNGED--dks
	* determine which column in the txt_file is the label-point ID
 * number.  this is done by looking for the second column in the 
 * file that has a 1 on the 1st line, 2 on the 2nd line, 3 on the
 * 3rd line, and so on...
 *
	if ((IDCol=FindIDColumn(txt_file)) < 0)
	{
		fprintf (stdout,"\n\nThe LABEL-TEXT file has been scanned.\n");
		fprintf (stdout,"There is not enough information in the file to create GRASS\n");
		fprintf (stdout,"attribute and category files\n");
		return(-1);
	}
*****/
	/* tell the user how many columns were found in the txt_file and
 * which one was found to be the label-point ID column
 */
	fprintf (stdout,"\n\nThe LABEL-TEXT file has been scanned. There are %d\n",NumLines);
	fprintf (stdout,"lines in the file and %d columns in the file\n", NumCols);


	if (NumCols == 2)
	{
		IDCol = 1;
		CatCol = 2;
		AttCol = -1;
		fprintf (stdout,"\nBecause there are only 2 columns, column 2 is assumned to be\n");
		fprintf (stdout,"the category number column\n");
	}
	else 
	{
		/* ask the user which column to use for GRASS category values,
    * this column must contain integers only.
    */
		fprintf (stdout,"\nColumn headers of the LABEL-TEXT file:\n");
		rewind(txt_file);
		if (!fgets(txtbuf,512,txt_file)) return (-1);
		fprintf(stderr,"%s",txtbuf);
		fprintf (stdout,"\nHere are the first three lines :\n");
		for (done = 0; done <= 2; done++)
		{
			if (!fgets(txtbuf,512,txt_file)) return (-1);
			fprintf(stderr,"%s",txtbuf);
		}
		done=0;

       do {
            fprintf (stdout,"\nEnter the number of the column that should be used\n");
            fprintf (stdout,"for line IDs (probably the column with -ID) : ");
            fgets(txtbuf,512,stdin);
            IDCol = atoi(txtbuf);
            if (IDCol<1 || IDCol>NumCols)
                fprintf (stdout,"That is not a valid column number, please try again\n");
			else
			{
				fprintf (stdout,"<%d>\n",IDCol);
				done=1;
			}
        }   while (!done);


		do {
			fprintf (stdout,"\nEnter the number of the column that should be used \n");
			fprintf (stdout,"for GRASS category values: ");
			fgets(txtbuf,512,stdin);
			if (strlen(txtbuf) == 0) exit(-1);
			CatCol = atoi(txtbuf);
			if (CatCol<1 || CatCol>NumCols)
				fprintf (stdout,"That is not a valid column number, please try again\n");
			else
			{
				fprintf (stdout,"<%d>\n",CatCol);
				done=1;
			}
		}   while (!done);

		/* ask the user which column to use for GRASS attribute text */
		done=0;
		do {
			fprintf (stdout,"\nEnter the number of the column that should be used\n");
			fprintf (stdout,"for GRASS attribute text [<CR> for none]: ");
			fgets(txtbuf,512,stdin);
			if (strlen(txtbuf) > 0)
			{
				AttCol = atoi(txtbuf);
				if (AttCol<1 || AttCol>NumCols)
					fprintf (stdout,"That is not a valid column number, please try again\n");
				else
				{
					fprintf (stdout,"<%d>\n",AttCol);
					done=1;
				}
			}
			else
			{
				AttCol = -1;
				done = 1;
			}
		}   while (!done);
	}

	fprintf (stdout,"\nWorking...\n");

	/* count number of different categories in the category file and use
 * this number to initialize a category structure
 */
	rewind(txt_file);
	if (!fgets(txtbuf,512,txt_file)) return (-1);  /* skip headers record */
	NumCats = CountCats(txt_file,CatCol);
	G_init_cats((CELL)NumCats,(char *)NULL,&new_cats);

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
/*dks: BUG???--why rewind this every time if the points are sequential?*/
			rewind(txt_file);
			if (!fgets(txtbuf,512,txt_file)) return (-1);  /* skip headers record */
			/* find the point's category value in the label-txt file */
		/*DEBUG*//* fprintf (stderr, "line %d id %d\n", d_i++, id); */	
			if ((CatStat=FindAttAndCat(txt_file,AttCol,CatCol,IDCol,id,AttText,&CatNum))<-1)
			{
				G_fatal_error("Reading label-text file");
				exit(-1);
			}

			/* write point and its category number to the att. file */
			write_att(atts_file,'A',east,north,CatNum);

			/* find an attribute string in the label-text file */
			/* set the attribute string in the category structure */
			if (G_set_cat((CELL)CatNum,AttText,&new_cats) != 1)
				G_fatal_error("Do_area_labs: call to G_set_cats");
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
