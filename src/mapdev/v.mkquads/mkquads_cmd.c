/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       v.mkquads
 * AUTHOR(S):    Michael H.
 *               DKS ?
 * PURPOSE:      make quads for USGS maps. 
 * COPYRIGHT:    (C) 2000 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *   	    	 License (>=v2). Read the file COPYING that comes with GRASS
 *   	    	 for details.
 *
 *****************************************************************************/

/*
*  Creates as many quads as it can in the current window.
*  Find the lower left quad point in the window. 
*  From that calculate number of quads that will fit in window.
*  Create the vector file.
*
*  Written by GRASS Team, Fall of 88,  Michael H.
*
*  Updated for Grass 4.0, 1/91 DKS
*/


/*
*  FORMERLY (pre-GRASS4.0):
*  There is a hidden option in this program. `-r`  it creates a reg file that
*  can be used by `digit`.  This is to be used by users who understand
*  the reg file, because the reg file WILL have to be edited for their use.
*  The reg file is a by-product of building the vector file.
*
*  GRASS4.0:
*    -r option now is for region (was window), NOT reg
*    -x option is now for reg(istration) file. ALSO, this option is no longer
*        hidden, and should be documented for the user.

*/

#include <string.h>
#include <stdlib.h>
#include <stdio.h>
#include "gis.h"
#include "Vect.h"
#include "quad_structs.h"
#include "local_proto.h"


/**  data directories   **/
#define		B_DIG		"dig"
#define		SITE_DIR	"site_lists"
#define		WIND_DIR	"windows"
#define		CHARNULL	'\0'

static char  *PROG ;


int main (int argc, char *argv[])
{
	/*  store filename and path  */
	char  *file_name;
	char errmsg[200];

	FILE  *fp_reg = NULL, *fp_sites = NULL ;
	FILE  *fp_digit = NULL;

	struct  quads_description  quads_info ;
	struct Cell_head window ;	/*  window in utm's  */
	struct Cell_head window_ll ;	/*  window in lat, long  */
	struct Cell_head work_window ;	/*  used for writing multiple windows */
	struct dig_head   d_head;

   /*new with Vlib*/
    struct Map_info Map;

	struct command_flags flags ;

	struct Option *map, *type;
	struct Flag *e_flag, *r_flag, *s_flag, *v_flag, *x_flag;
	int hsize=0, vsize=0;
	struct GModule *module;

	PROG = argv[0] ;
	G_gisinit(argv[0]) ;
	/* commented by Andreas Lange, 11/2000 */
	/* system("clear") ; */
	setbuf(stdout, NULL) ;

	module = G_define_module();
	module->description = 
	  "Create a vector map layer and/or sites list and/or " 
	  "geographic region definintion file for a USGS 7.5"
	  "-minute quadrangle. ";

	map = G_define_option();
	map->key		= "map";
	map->type		= TYPE_STRING;
	map->required		= YES;
	map->multiple		= NO;
	map->description	= "map name";

	type = G_define_option();
	type->key		= "type";
	type->type		= TYPE_STRING;
	type->required		= NO;
	type->multiple		= NO;
	type->description	= "quad size";
	type->answer		= "full";
	type->options		= "full,third,quarter,1x1,1x2";

	e_flag = G_define_flag();
	e_flag->key		= 'e';
	e_flag->description	= "encompass current region with quads";

	s_flag = G_define_flag();
	s_flag->key		= 's';
	s_flag->description	= "create sites file";

	r_flag = G_define_flag();
	r_flag->key		= 'r';
	r_flag->description	= "create region file(s): quad.1 quad.2 ...";

	v_flag = G_define_flag();
	v_flag->key		= 'v';
	v_flag->description	= "create vector file (default)";

	x_flag = G_define_flag();
	x_flag->key		= 'x';
	x_flag->description	= "create reg file";
	
	if (G_parser (argc, argv))
	    exit(-1);

	file_name = map->answer;
	if (strcmp(type->answer, "full") == 0){
		hsize = 450;
		vsize = 450;
	}
	if (strcmp(type->answer, "quarter") == 0){
		hsize = 225;
		vsize = 225;
	}
	if (strcmp(type->answer, "third") == 0){
		hsize = 450;
		vsize = 150;
	}
	if (strcmp(type->answer, "1x1") == 0){
		hsize = 3600;
		vsize = 3600;
	}
	if (strcmp(type->answer, "1x2") == 0){
		hsize = 7200;
		vsize = 3600;
	}

	if (G_legal_filename (file_name) < 0)
	{
	    fprintf (stderr, "<%s> illegal name\n", file_name);
	    exit(-1);;
	}

/*	if (G_projection() != METERS)
	{
		fprintf( stderr, " This program can only create a UTM vector file and this is not a UTM location.\n") ;
		exit(-1) ;
	}
*/
		
    /*  get the current window  */
	G_get_window(&window) ;
	G_get_window(&window_ll) ;


#ifdef OLD_VERSION
	/***replaced with new parsing stuff***/
	*buffer = CHARNULL ;
	check_args( argc, argv, buffer, &flags) ;

	/*set up flag flags!!*/
	flags.vectors = 0 ;
	flags.sites = 0 ;
	flags.windows = 0 ;
	flags.encompass = 0 ;
	flags.reg = 0 ;
	flags.usage = 0 ;
	flags.files = 0 ;
#endif

	flags.encompass = e_flag->answer;
	flags.sites = s_flag->answer;
	flags.windows = r_flag->answer;
	flags.reg = x_flag->answer;
	if (flags.windows || flags.sites || flags.reg)
	    flags.vectors = v_flag->answer;
	else /*default*/
	    flags.vectors = 1;

/*DEBUG*/
/*
     fprintf (stderr, "flags: e=%d s=%d v=%d w=%d r=%d\n", 
     flags.encompass, flags.sites, flags.vectors, flags.windows, flags.reg);
*/

#ifdef OLD_VERSION
	if (flags.usage)
	{
		fprintf( stderr, " Usage:  %s  file.name  -[e,v,s]\n", PROG) ;
		fprintf( stderr, "    -e   encompass the GIS region\n") ;
		fprintf( stderr, "    -s   creates a site file\n") ;
		fprintf( stderr, "    -v   creates a vector file (default)\n") ;
		exit(-1) ;
	}
#endif /*OLD VERSION*/

/*  setup vector file */
	if (flags.vectors)

	/*	if ( (fp_digit = G_fopen_vector_new(file_name))  ==  NULL) */
		if (0 > Vect_open_new (&Map, file_name))
		{
		   sprintf(errmsg," %s: Can't open vector file <%s>\n", PROG, file_name) ;
		   G_fatal_error (errmsg);
		}


/*  setup sites file */
	if (flags.sites)
		if ( (fp_sites = G_fopen_sites_new(file_name))  ==  NULL)
		{
			fprintf(stderr, " %s: Can't open sites file for write:%s\n", PROG, file_name) ;
			exit(-1) ;
		}

/*  setup reg file */

	if (flags.reg)
		if ( (fp_reg = G_fopen_new("reg", file_name))  ==  NULL)
		{
			fprintf(stderr, " %s: Can't open reg file for write.\n", PROG) ;
			exit(-1) ;
		}

/*  setup the conversions */
	setup_ll_to_utm (&quads_info) ;
	convert_window_to_ll (&window_ll) ;
	init_quad_struct( &quads_info, &window_ll, hsize, vsize) ;
	find_quad_point (&quads_info, &window_ll, &flags, hsize, vsize) ;
	calculate_quads ( &quads_info, &window_ll, &flags, hsize, vsize) ;

    /*  initialize and write the digit vector header  */
	if (flags.vectors)
	{
		fprintf (stdout,"\n Creating vector file of quads...") ;
		init_header ( fp_digit, &window, &d_head) ;
		/*new with Vlib*/
		Vect_copy_head_data (&d_head, &Map.head);
		write_quads ( fp_digit, &quads_info, &Map ) ;
		/*fclose (fp_digit) ;*/
		Vect_close (&Map);
	}

	if (flags.sites)
	{
		fprintf (stdout,"\n\n Creating sites file ...") ;
		sites_quads ( fp_sites, &quads_info ) ;
		fclose (fp_sites) ;
	}

	if (flags.reg)
	{
		fprintf (stdout,"\n\n Creating reg file of `reg.quads`...") ;
		reg_quads ( fp_reg, &quads_info ) ;
		fclose (fp_reg) ;
	}

    /*  this is in the future  */
	if (flags.windows)
	{
		fprintf (stdout,"\n\n Creating region files with names `quad.#`...") ;
    	/*  make sure directory is there  */
		G__make_mapset_element( WIND_DIR) ;
		G_get_window(&work_window) ;

		window_quads ( &quads_info, &work_window) ;
	}


	fprintf (stdout,"\n\n") ;
	report_quads ( stdout, &quads_info, &flags ) ;
	fprintf (stdout,"\n") ;

	exit(0);
}		/*  main()  */


int check_args (int argc, char *argv[], char *filename,
	struct command_flags *flags)
{

	int  stat ;

	flags->vectors = 0 ;
	flags->sites = 0 ;
	flags->windows = 0 ;
	flags->encompass = 0 ;
	flags->reg = 0 ;
	flags->usage = 0 ;
	flags->files = 0 ;

	if (argc == 1)
	{
		flags->vectors = 1 ;
		return(0) ;
	}

	while ( --argc)
		if ( *argv[argc] == '-' )
		{
			stat = dashed_args(  argv[argc], flags) ;
			if (stat)
			{
				flags->usage = 1 ;
				break ;
			}
			
		}
		else
		if (flags->files)
		{
			fprintf( stderr, " Too many filenames on the command line ...\n") ;
			flags->usage = 1 ;
			break ;
		}
		else
		if ( ! (strcmp( argv[argc], "usage") ) )
		{
			flags->usage = 1 ;
			break ;
		}
		else
		{
			flags->files = 1 ;
			strcpy ( filename, argv[argc]) ;
		}

	    /*  set default  */
		if ( flags->sites == 0  &&  flags->windows == 0)
			flags->vectors = 1 ;
	return (0) ;
}

int dashed_args (char *args, struct command_flags *flags)

{

	char  *ptr ;

	ptr = args ;
	/*  skip over the '-'  */
	++ptr ;
	while ( *ptr )
		switch ( *ptr++)
		{
		case 'v':
			flags->vectors = 1 ;
			break ;
		case 's':
			flags->sites = 1 ;
			break ;
		case 'w':
			flags->windows = 1 ;
			break ;
		case 'e':
			flags->encompass = 1 ;
			break ;
		case 'r':
			flags->reg = 1 ;
			break ;
		default:
			return(-1) ;
			break ;
		}

	return(0) ;
}

