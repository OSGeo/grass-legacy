
/* main_cmd.c
 *
 *
 *  main for grass 4.0 cmd version of v.in.arc
 *  adapted by David Stigberg 2/91
 *
 * function defined: main routine for arc.to.grass (v.in.arc) program
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
 * 1) ARC/INFO Generate files must respresent either
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

#include <stdlib.h>
#include  <stdio.h>
#include  "gis.h"
#include  "AtoG.h"
#include  "Vect.h"
#include "v_in_arc.cmd.h"

#define POLY_TYPE    1
#define LINE_TYPE    0

static char *lines_fname = NULL;   /* ungenerate format lines file */
static char *pts_fname = NULL;    /* ungenerate format point-labels file */
static char *txt_fname = NULL;     /* label-text file */
static char *dig_fname = NULL;    /* GRASS vector (dig) file */

int 
main (int argc, char **argv)
{
	FILE  *fopen();
	int   neatline=0;
	int idcol, catcol, attcol;
	int   errflag;
	char  *progname, *mapset ;
	char errmsg[200];
	int  cov_type;          /* coverage type */
	int need_colspecs = 0;

	FILE  *lines_file;
	FILE  *pts_file;
	FILE  *txt_file;
	FILE  *dig_file;
	FILE  *atts_file;

    struct Map_info VectMap;
	char full_txtname[200];
	char  *tmp_name, *G_tempfile();
	FILE  *tmp_file;


	char  atts_filepath[80] ;

	/*
   struct Flag *p_flag, *n_flag, *l_flag;
   */
	struct GModule *module;
	struct Flag *n_flag;
	struct Option *covertype, *linesname, *ptsname ;
	struct Option *txtname, *digname ;
	struct Option *idcolopt, *catcolopt, *attcolopt;


	/* Define the different options */


	G_gisinit(argv[0]);
	progname = G_program_name();

	module = G_define_module();
	module->description =
		"Converts data in ARC/INFO format to GRASS's vector format, "
		"and stores output in the user's current GRASS mapset.";

	/*
	p_flag = G_define_flag();
	p_flag->key     = 'p';
	p_flag->description = "POLYGON coverage type)";

	l_flag = G_define_flag();
	l_flag->key     = 'l';
	l_flag->description = "LINE coverage type";
	*/

	n_flag = G_define_flag();
	n_flag->key     = 'n';
	n_flag->description = "Neatline";

	covertype = G_define_option() ;
	covertype->key        = "type";
	covertype->type       = TYPE_STRING;
	covertype->options		= "line,polygon";
	covertype->required   = YES;
	covertype->description= "coverage type";

	linesname = G_define_option() ;
	linesname->key        = "lines_in";
	linesname->type       = TYPE_STRING;
	linesname->required   = YES;
	linesname->description= "ARC/INFO ungenerate lines file";

	ptsname = G_define_option() ;
	ptsname->key        = "points_in";
	ptsname->type       = TYPE_STRING;
	ptsname->required   = NO;
	ptsname->description= "ARC/INFO ungenerate label-points file";

	txtname = G_define_option() ;
	txtname->key        = "text_in";
	txtname->type       = TYPE_STRING;
	txtname->required   = NO;
	txtname->description= "ARC/INFO label-text file" ;

	digname = G_define_option() ;
	digname->key        = "vector_out";
	digname->type       = TYPE_STRING;
	digname->required   = YES;
	digname->description= "resultant Vector output file" ;

	idcolopt = G_define_option() ;
	idcolopt->key        = "idcol";
	idcolopt->type       = TYPE_INTEGER;
	idcolopt->required   = NO;
	idcolopt->description= "ID Number column in label-text file" ;

	catcolopt = G_define_option() ;
	catcolopt->key        = "catcol";
	catcolopt->type       = TYPE_INTEGER;
	catcolopt->required   = NO;
	catcolopt->description= "GRASS category column in label-text file" ;

	attcolopt = G_define_option() ;
	attcolopt->key        = "attcol";
	attcolopt->type       = TYPE_INTEGER;
	attcolopt->required   = NO;
	attcolopt->description= "GRASS attribute column in label-text file";


	if (G_parser (argc, argv))
	{
		exit (-1);
	}

	/*set coverage: polygon vs line*/

	if (strcmp (covertype->answer, "polygon") == 0)
		cov_type = POLY_TYPE;
	else
	{
		if (strcmp (covertype->answer, "line") == 0)
			cov_type = LINE_TYPE;
		else
		{
                       sprintf(errmsg,
                       "%100s: Must specify POLYGON or LINE coverage type.\n",
                           progname);
                       G_fatal_error (errmsg);
			exit (-1);
		}
	}

    pts_file = NULL;
	atts_file = NULL;
	txt_file = NULL;


	neatline = n_flag->answer;

	fprintf(stderr, "%s: Coverage type = %s\n", progname, cov_type ? "polygon":"line");



	lines_fname = linesname->answer;
	pts_fname = ptsname->answer;
	txt_fname = txtname->answer;
	dig_fname = digname->answer;


	/*verify that required filenames are there*/
	if (!*lines_fname  || !*dig_fname)
	{
		fprintf (stderr, "\n\n%s: Command line error: missing lines input name or vector output name.\n\n", argv[0]);
		G_usage();
		exit (-1);
	}

	/*************SETUP: ARC/INFO side**************/

	/*open input files*/

	if ((mapset=G_find_file2 ("arc", lines_fname, "")) == NULL)
	{
		fprintf(stderr, "Cannot find ARC/INFO lines file <%s>\n", lines_fname);
		exit (-1);
	}
	if ((lines_file=G_fopen_old ("arc", lines_fname, mapset)) == NULL)
	{
		fprintf(stderr, "Cannot open ARC/INFO lines file <%s>\n", lines_fname);
		exit (-1);
	}

	if (cov_type == POLY_TYPE)
	{
		if (pts_fname != NULL && *pts_fname)
			if ((pts_file = G_fopen_old("arc", pts_fname, mapset)) == NULL)
			{
				fprintf(stderr, "Cannot open ARC/INFO points file <%s>\n", lines_fname);
				exit (-1);
			}
	}

	if (txt_fname != NULL && *txt_fname)
	{

		need_colspecs = 1; /*set flag saying idcol and catcol need to be set*/ 

		if (G__file_name(full_txtname,"arc",txt_fname, mapset) == NULL){
			sprintf (errmsg,"Could not find ARC text file %s\n",full_txtname);
			G_warning (errmsg);
		}
		/* remove the txt_file INFO header, save the column headings
                 * rewrite the txt_file */
		tmp_name = G_tempfile();
		if ((errflag = DO_txt_file(full_txtname, tmp_name)) == 0)
		{
			txt_file=fopen(tmp_name,"r");
		}
		else
		{
			if (errflag == -1)
				G_fatal_error ("ARC-INFO text-label file header");
			else
				G_fatal_error ("Item name list in ARC-INFO text-label file header.");
		}
	 }


	/************SETUP: ID, Cat, and Att Column specs*****************/

	if (idcolopt->answer == NULL)
		idcol = 0;
	else
		idcol = atoi (idcolopt->answer);

	if (catcolopt->answer == NULL)
		catcol = 0;
	else
		catcol = atoi (catcolopt->answer);

	if (attcolopt->answer == NULL)
		attcol = 0;
	else
		attcol = atoi (attcolopt->answer);

	if (need_colspecs)
	   if (idcol == 0 | catcol == 0)
		   G_fatal_error ("Must specify ID and Category Column (idcol and catcol) for ARC-INFO text-label file");

	/*************SETUP: vector side**************/

	/*open binary dig file*/
	/*
	if ((dig_file = G_fopen_vector_new (dig_fname)) == NULL)
	{
		fprintf (stderr, "Cannot open binary vector file <%s>\n", dig_fname);
		exit (-1);
	}
	*/

    if (0 > Vect_open_new (&VectMap, dig_fname))
	{
		sprintf(errmsg, "Not able to open vector file <%s>\n", dig_fname) ;
		G_fatal_error (errmsg);
	}

/*isn't this wrong? shouldn't it be text file?? since line files don't need
  points_file
*/
	if (txt_file != NULL)
	{
		/* open a new GRASS dig_atts file */
		if ((atts_file = G_fopen_new ("dig_att", dig_fname)) == NULL)
		{
			fprintf (stderr, "Cannot open dig att file <%s>\n", dig_fname);
			exit (-1);
		}
	}

	/* set up finished: on to the task at hand*/

	if ((errflag=BuildDig(cov_type,neatline,lines_file,pts_file,txt_file,
	    atts_file, &VectMap,dig_fname, idcol, catcol, attcol))<0)
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
	fprintf (stderr, "\nv.in.arc finished.\n");
	fprintf(stderr, "\n\nBefore vector file <%s> can be used in the 'v.digit' program:\nRun the program v.support to build the needed support files.\n",
	    dig_fname) ;

	exit(0) ;
}
