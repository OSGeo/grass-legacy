/* @(#)main.c	1.0   04/90 */
/*  Written by  Dave Johnson
**  DBA Systems, Inc.
**
**  modified by R.L.Glenn
**  USDA, Soil COnservation Service, CGIS Staff
*/

/* main.c
 *
 * function defined: main routine for arc.to.grass program
 *
 * PURPOSE: Import ARC/INFO Generate files into GRASS.
 *          Results in the creation of a GRASS vector 
 *          file consisting of dig, dig_atts, and
 *          dig_cats components.  This routine gets
 *          file names from the user and opens files
 *          necessary for importation. 
 *
 * NOTES: 
 *
 * 1) ARC/INFO Generate files must represent either
 *    a polygon or a line coverage.  No mixing of lines
 *    and polygons in one import operation.
 *
 * 2) Program v.support must be run on the resulting GRASS
 *    vector file and then some cleaning-up may have to be
 *    done using program v.digit.
 *
 *
 * Dave Johnson
 * DBA Systems, Inc.
 * 10560 Arrowhead Drive
 * Fairfax, Virginia 22030
 *
 */

#include  <stdio.h>
#include  "gis.h"
#include  "Vect.h"

main(argc, argv)
int argc;
char **argv;
{
	FILE  *fopen();
	char  tmpbuf[80];
	int   neatline=0;
	int   done,
	errflag,
	try_again=0;
	char  *mapset ;
	char errmsg[200];
	char  cov_type[80];          /* coverage type */
	char  lines_filename[80] ;   /* ungenerate format lines file */
	FILE  *lines_file;
	char  pts_filename[80] ;     /* ungenerate format point-labels file */

	FILE  *pts_file;
	char  txt_filename[80] ;     /* label-text file */

	FILE  *txt_file;
	char  dig_filename[80] ;     /* GRASS vector (dig) file */

	char  dig_filepath[80] ;
	FILE  *dig_file;
	char  atts_filename[80] ;     /* GRASS vector (dig_atts) file */

	char  atts_filepath[80] ;
	FILE  *atts_file;
	char  *tmp_name, *G_tempfile();
	FILE  *tmp_file;

	struct Map_info  VectMap;

	G_gisinit("ARC/INFO Generate Format Import");

	if (argc != 2)
	{                 /* get name for grass vector file from user */
		mapset = G_ask_new( " VECTOR (DIGIT) FILENAME ",dig_filename,"dig",
		    "binary vector") ;
		if ( ! mapset) exit(0) ;
	}
	else 
		strcpy(dig_filename,argv[1]);

	do {

		try_again=0;

		/* GET COVERAGE TYPE */

		printf("\n COVERAGE TYPE\n");
		printf("Enter \"polygon(area)\" or \"line\"\n");
		printf("Hit RETURN to cancel request\n");
		printf("> ");
		gets(tmpbuf);

		cov_type[0] = '\0';
		if (strcmp(tmpbuf,"polygon") == 0) strcat(cov_type,"polygon");
		else if (strcmp(tmpbuf,"poly") == 0) strcat(cov_type,"polygon");
		else if (strcmp(tmpbuf,"area") == 0) strcat(cov_type,"polygon");
		else if (strcmp(tmpbuf,"line") == 0) strcat(cov_type,"line");

		/* EXIT IF USER HIT RETURN */
		if (strcmp(tmpbuf,"") == 0)
			exit(0);

			/* GET FILE NAMES FOR COVERAGE TYPE "POLYGON" */

		else if (strcmp(cov_type,"polygon")==0)
		{
			do {
				printf("\n NEATLINE\n");
				printf("Do you want a neatline ?\n");
				printf("Enter \"yes\" or \"no\"\n");
				printf("> ");
				gets(tmpbuf);
			}      while (strcmp(tmpbuf,"yes")!=0 && strcmp(tmpbuf,"no")!=0);
			if (strcmp(tmpbuf,"yes")==0)
				neatline = 1;

			/* LINES FILENAME */
			done = 0;
			do {
				printf("\n LINES FILENAME\n");
				printf("Enter the name of the file created with the LINES\n");
				printf("option of the ARC/INFO Ungenerate command\n");
				printf("Hit RETURN to cancel request\n");
				printf("> ");
				gets(tmpbuf);
				if (strcmp(tmpbuf,"") == 0)
					exit(0);
				else 
				{
					if (G__file_name(lines_filename,"arc",tmpbuf,G_mapset())
					    == NULL)
						G_warning ("Could not find ARC line file %s\n", lines_filename);
					if ((lines_file=fopen(lines_filename,"r")) == NULL)
						G_warning("File could NOT be opened for reading ");
					else
						done = 1;
				}
			}     while (!done);

			/* LABEL-POINTS FILENAME */
			done = 0;
			do {
				printf("\n LABEL-POINTS FILENAME\n");
				printf("Enter the name of a file created with the POINTS\n");
				printf("option of the ARC/INFO Ungenerate command\n");
				printf("Hit RETURN if there is no such file\n");
				printf("> ");
				gets(tmpbuf);
				if (strcmp(tmpbuf,"") == 0)
				{
					pts_file = NULL;
					txt_file = NULL;
					done = 1;
				}
				else 
				{
					if (G__file_name(pts_filename,"arc",tmpbuf,G_mapset())
					    == NULL)
						G_warning ("Could not find ARC label-points file %s\n",
						    pts_filename);
					if ((pts_file=fopen(pts_filename,"r")) == NULL)
						G_warning("File could NOT be opened for reading ");
					else
						done = 1;
				}
			}      while (!done);

			/* LABEL-TEXT FILENAME */
			if (pts_file != NULL)
			{
				done = 0;
				do {
					printf("\n LABEL-TEXT FILENAME\n");
					printf("Enter the name of a file that associates\n");
					printf("label-point ID numbers with text label strings\n");
					printf("Hit RETURN if there is no such file\n");
					printf("> ");
					gets(tmpbuf);
					if (strcmp(tmpbuf,"") == 0)
					{
						txt_file = NULL;
						done = 1;
					}
					else 
					{
						if (G__file_name(txt_filename,"arc",tmpbuf,G_mapset())
						    == NULL)
							G_warning ("Could not find ARC text file %s\n", 
							    txt_filename);
						/* remove the txt_file INFO header, save the column headings
                 * rewrite the txt_file */
						tmp_name = G_tempfile();
						if ((errflag = DO_txt_file(txt_filename, tmp_name)) == 0)
						{
							txt_file=fopen(tmp_name,"r");
							done = 1;
						}
						else
						{
							if (errflag == -1)
								G_warning("Error in ARC-INFO text-label file header.");
							else
								G_warning("Error in ARC-INFO textlabel file item name list.");
						}
					}
				}         while (!done);
			}
		}

		/* ELSE - GET FILE NAMES FOR COVERAGE TYPE "LINE" */

		else if (strcmp(cov_type,"line")==0)
		{
			/* LINES FILE */
			done = 0;
			do {
				printf("\n LINES FILENAME\n");
				printf("Enter the name of a file created with the LINES\n");
				printf("option of the ARC/INFO Ungenerate command\n");
				printf("Hit RETURN to cancel request\n");
				printf("> ");
				gets(tmpbuf);
				if (strcmp(tmpbuf,"") == 0)
					exit(0);
				else 
				{
					if (G__file_name(lines_filename,"arc",tmpbuf,G_mapset())
					    == NULL)
						G_warning ("Could not find ARC line file %s\n", lines_filename);
					if ((lines_file=fopen(lines_filename,"r")) == NULL)
						G_warning("File could NOT be opened for reading ");
					else
						done = 1;
				}
			}      while (!done);

			/* LABEL-TEXT FILENAME */
			done = 0;
			do {
				printf("\n LABEL-TEXT FILENAME\n");
				printf("Enter the name of a file that associates\n");
				printf("line ID numbers with text label strings\n");
				printf("Hit RETURN if there is no such file\n");
				printf("> ");
				gets(tmpbuf);
				if (strcmp(tmpbuf,"") == 0)
				{
					pts_file = NULL;
					txt_file = NULL;
					done = 1;
				}
				else 
				{
					if (G__file_name(txt_filename,"arc",tmpbuf,G_mapset())
					    == NULL)
						G_warning ("Could not find ARC text file %s\n",txt_filename);
					/* remove the txt_file INFO header, save the column headings
                 * rewrite the txt_file */
					tmp_name = G_tempfile();
					if ((errflag = DO_txt_file(txt_filename, tmp_name)) == 0)
					{
						txt_file=fopen(tmp_name,"r");
						done = 1;
					}
					else
					{
						if (errflag == -1)
							G_warning("Error in INFO attribute <txt> file item count");
						else
							G_warning("Error in INFO attribute <txt> file item names");
					}
				}
			}      while (!done);
		}
		else try_again = 1;
	}while (try_again);

	/*obsolete
G__make_mapset_element("dig") ;
G__file_name(dig_filepath,"dig",dig_filename,G_mapset()) ; 
if ((dig_file=fopen(dig_filepath,"w"))==NULL)
   {
   printf("Not able to open dig file <%s>\n",dig_filepath);
   exit(-1);
   }
**/
	if (0 > Vect_open_new (&VectMap, dig_filename))
	{
		sprintf(errmsg, "Not able to open vector file <%s>\n", dig_filename) ;
		G_fatal_error (errmsg);
	}

	/* open a new GRASS dig_atts file */
	G__make_mapset_element("dig_att") ;
	G__file_name(atts_filepath,"dig_att",dig_filename,G_mapset()) ;
	if ((atts_file=fopen(atts_filepath,"w"))==NULL)
	{
		printf("Not able to open dig_atts file <%s>\n",atts_filepath);
		exit(-1);
	}


	if ((errflag=BuildDig(cov_type,neatline,lines_file,pts_file,txt_file,
	    atts_file,&VectMap, dig_filename))<0)
	{
		switch (errflag)
		{
		case -1:
			G_fatal_error("Reading LINES file"); 
			exit(-1);
		case -2:
			G_fatal_error("Reading LABEL-TEXT file"); 
			exit(-1);
		case -3:
			G_fatal_error("Reading LABEL-TEXT or LABEL-POINTS file"); 
			exit(-1);
		case -4:
			G_fatal_error("Reading LINES file"); 
			exit(-1);
		case -5:
			G_fatal_error("Invalid coverage type"); 
			exit(-1);
		default: 
			break;
		}
	}

	Vect_close (&VectMap);
	fprintf (stderr, "\n\nv.in.arc finished.\n");
	fprintf(stderr, "\n\nBefore the new vector file can be used in the 'v.digit' program:\n\n   Run the program v.support to build the  needed support files.\n",
	    dig_filename) ;

	exit(0) ;
}
