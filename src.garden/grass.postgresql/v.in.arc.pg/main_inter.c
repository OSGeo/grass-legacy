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
 /*
 *	Alex Shevlakov sixote@yahoo.com 02/2000 Postgres dump from DBF 
 *	and no need to have cats consequently (so cats == id's)
 */

#include  <stdio.h>
#include  <string.h>
#include  "gis.h"
#include  "Vect.h"
#include "v_in_arc.inter.h"
#include "shapefil.h"

int main (int argc, char **argv)
{
	char  tmpbuf[80]="";
	int   neatline=0;
	int   done,
	errflag,
	try_again=0;
	char  *mapset ;
	char 	errmsg[200]="";
	char  cov_type[80]="";          /* coverage type */
	char  lines_filename[80]="" ;   /* ungenerate format lines file */
	FILE  *lines_file;
	char  pts_filename[80]="" ;     /* ungenerate format point-labels file */

	FILE  *pts_file;
	char  txt_filename[80]="" ;     /* label-text file */

	FILE  *txt_file;
	char  dig_filename[80]="";     /* GRASS vector (dig) file */

	char  dig_filepath[80]="" ;
	FILE  *dig_file;
	char  atts_filename[80]="" ;     /* GRASS vector (dig_atts) file */

	char  atts_filepath[80]="" ;
	FILE  *atts_file;
	char  *tmp_name;
	FILE  *tmp_file;

	struct Map_info  VectMap;
	
	int no_rattle;
   	char *infile;


    	struct {
	struct Option *input, *dumpmode;
    	} parm;


	G_gisinit("ARC/INFO -  import from UNGENERATE");

	if (argc != 2)
	{                 /* get name for grass vector file from user */
		mapset = G_ask_new( "GRASS vector file:",dig_filename,"dig",
		    "binary vector") ;
		if ( ! mapset) exit(0) ;
	}
	else 
		strcpy(dig_filename,argv[1]);

	do {

		try_again=0;

		/* GET COVERAGE TYPE */

		fprintf (stdout,"\n Coverage type:\n");
		fprintf (stdout,"Enter \"polygon(area)\" or \"line\"\n");
		fprintf (stdout,"Hit RETURN to cancel.\n");
		fprintf (stdout,"> ");
		fgets(tmpbuf,80,stdin);
/*************************************************************************/
   /*****!!!!!! hier eine Zeile eingefuegt,ebenso bei allen anderen fgets *****/

              tmpbuf[strlen(tmpbuf)-1]='\0';
/********************************************************************/
                
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
				fprintf (stdout,"\n Neatline:\n");
				fprintf (stdout,"Add neatline?\n");
				fprintf (stdout,"Enter \"yes\" or \"no\"\n");
				fprintf (stdout,"> ");
				fgets(tmpbuf,80,stdin);
                                tmpbuf[strlen(tmpbuf)-1]='\0';
			}      while (strcmp(tmpbuf,"yes")!=0 && strcmp(tmpbuf,"no")!=0);
			if (strcmp(tmpbuf,"yes")==0)
				neatline = 1;

			/* LINES FILENAME */
			done = 0;
			do {
				fprintf (stdout,"\n Lines file ARC/INFO:\n");
				fprintf (stdout,"Enter name of LINES file\n");
				fprintf (stdout,"from ARC/INFO Ungenerate\n");
				fprintf (stdout,"Hit RETURN to cancel.\n");
				fprintf (stdout,"> ");
				fgets(tmpbuf,80,stdin);
                                tmpbuf[strlen(tmpbuf)-1]='\0';
				if (strcmp(tmpbuf,"") == 0)
					exit(0);
				else 
				{

					strncpy(lines_filename,tmpbuf,strlen(tmpbuf));
					if ((lines_file=fopen(lines_filename,"r")) == NULL)
						G_warning("Can't open lines from ARC/INFO");
					else
						done = 1;
				}
			} while (!done);

			/* LABEL-POINTS FILENAME */
			done = 0;
			do {
				fprintf (stdout,"\n Labels file ARCINFO:\n");
				fprintf (stdout,"Enter file POINTS\n");
				fprintf (stdout,"from ARC/INFO Ungenerate \n");
				fprintf (stdout,"Hit RETURN, if none.\n");
				fprintf (stdout,"> ");
				fgets(tmpbuf,80,stdin);
                                tmpbuf[strlen(tmpbuf)-1]='\0';
				if (strcmp(tmpbuf,"") == 0)
				{
					pts_file = NULL;
					txt_file = NULL;
					done = 1;
				}
				else 
				{

					strncpy(pts_filename,tmpbuf,strlen(tmpbuf));
					if ((pts_file=fopen(pts_filename,"r")) == NULL)
						G_warning("Can't open labels from ARC/INFO");
					else
						done = 1;
				}
			} while (!done);

		}

		/* ELSE - GET FILE NAMES FOR COVERAGE TYPE "LINE" */

		else if (strcmp(cov_type,"line")==0)
		{
			/* LINES FILE */
			done = 0;
			do {
				fprintf (stdout,"\n Lines file:\n");
				fprintf (stdout,"Enter name of LINES file\n");
				fprintf (stdout,"from ARC/INFO Ungenerate\n");
				fprintf (stdout,"Hit RETURN to cancel.\n");
				fprintf (stdout,"> ");
				fgets(tmpbuf,80,stdin);
                                tmpbuf[strlen(tmpbuf)-1]='\0';
				if (strcmp(tmpbuf,"") == 0)
					exit(0);
				else 
				{

					strncpy(lines_filename,tmpbuf,strlen(tmpbuf));
					if ((lines_file=fopen(lines_filename,"r")) == NULL)
						G_warning("Can't open lines from ARC/INFO");
					else
						done = 1;
				}
			} while (!done);

		
		}
		else try_again = 1;
	}while (try_again);


	if (0 > Vect_open_new (&VectMap, dig_filename))
	{
		sprintf(errmsg, "Can't open <%s>\n", dig_filename) ;
		G_fatal_error (errmsg);
	}

	/* open a new GRASS dig_atts file */
	G__make_mapset_element("dig_att") ;
	G__file_name(atts_filepath,"dig_att",dig_filename,G_mapset()) ;
	if ((atts_file=fopen(atts_filepath,"w"))==NULL)
	{
		fprintf (stdout,"Can't open dig_atts <%s>\n",atts_filepath);
		exit(-1);
	}

/************************
here we hack -A.Sh.*/
			txt_file=lines_file;
			
/***********************/
	if ((errflag=BuildDig(cov_type,neatline,lines_file,pts_file,txt_file,
	    atts_file,&VectMap, dig_filename))<0)
	{
		switch (errflag)
		{
		case -1:
			G_fatal_error("Error reading LINES"); 
			exit(-1);
		case -2:
			G_fatal_error("Error reading"); 
			exit(-1);
		case -3:
			G_fatal_error("Error reading LABEL-POINTS"); 
			exit(-1);
		case -4:
			G_fatal_error("Error reading LINES"); 
			exit(-1);
		case -5:
			G_fatal_error("Bad coverage type"); 
			exit(-1);
		default: 
			break;
		}
	}
	
	
	/* define the different options */
	
    	parm.input = G_define_option() ;
    	parm.input->key        = "input";
    	parm.input->type       = TYPE_STRING;
    	parm.input->required   = NO;
    	parm.input->description= "Name of .dbf file to be imported (or hit ENTER for none)";

	parm.dumpmode = G_define_option() ;
    	parm.dumpmode->key        = "dumpmode";
   	parm.dumpmode->type       = TYPE_STRING;
    	parm.dumpmode->required   = NO;
    	parm.dumpmode->description= "Admin/normal user dump mode (Default = Postgres super-user)";
    

    /* get options and test their validity */

    if (G_parser(argc, argv))
	exit(-1);
	
	infile = parm.input->answer;
    	no_rattle = (int) parm.dumpmode->answer;
    

    	if (infile) 
		PgDumpFromDBF(infile, no_rattle);
	
	Vect_close (&VectMap);
	fprintf (stderr, "\n\nv.in.arc.pg finished.\n");
	fprintf(stderr, "\n\nBefore using <%s> in 'v.digit' :\nrun v.support to build topology.\n",
	    dig_filename) ;

	exit(0) ;
}
