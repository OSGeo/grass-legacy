static char rcsid[]="$Header: /gis3/4.0/src/mapdev/v.mkquads/RCS/mkquads_cm.c,v 1.2 1991/10/04 14:48:04 grass Exp $";

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

#include	<stdio.h>
#include	"gis.h"
#include	"Vect.h"
#include	"quad_structs.h"


/**  data directories   **/
#define		B_DIG		"dig"
#define		SITE_DIR	"site_lists"
#define		WIND_DIR	"windows"
#define		CHARNULL	'\0'

static char  *PROG ;


main (argc, argv)
	int  argc ;
	char  *argv[] ;
{

/*--> added by R Glenn, SCS, 10/3/91 
      for projection support */
	char  *proj_file, proj_name[10];
	/*  store filename and path  */
	char  *file_name;
	char  buffer[128] ;
	char errmsg[200];

	FILE  *fp_reg, *fp_sites ;
	FILE  *fp_digit,  *fopen() ;

	struct  quads_description  quads_info ;
	struct Cell_head window ;	/*  window in utm's  */
	struct Cell_head window_ll ;	/*  window in lat, long  */
	struct Cell_head work_window ;	/*  used for writing multiple windows */
	struct dig_head   d_head;

   /*new with Vlib*/
    struct Map_info Map;

	struct command_flags flags ;

	struct Option *map;
	struct Flag *e_flag, *r_flag, *s_flag, *v_flag, *x_flag;

	PROG = argv[0] ;
	G_gisinit(argv[0]) ;
#ifdef SYSV
	system("tput clear");
#else
	system("clear") ;
#endif
	setbuf(stdout, NULL) ;

	map = G_define_option();
	map->key		= "map";
	map->type		= TYPE_STRING;
	map->required		= YES;
	map->multiple		= NO;
	map->description	= "map name";

	e_flag = G_define_flag();
	e_flag->key		= 'e';
	e_flag->description	= "encompass current window with quads";

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

/*--> modified for projections, R Glenn, SCS, 10/3/91
  	if (G_projection() != METERS) */
        G__file_name (buffer, "", "PROJ_INFO", G_mapset());
	proj_file = buffer;
        if (access(buffer,0) == 0)
	  {
	  if ( G_lookup_key_value_from_file(proj_file, "proj", proj_name, 4) != 1)
	    {
	    fprintf(stderr," <%s> is not readable\n",buffer);
	    exit(-1);
	    }
	  }
        else
	  G_fatal_error("PROJ_INFO file not found, run m.setproj for this mapset");

  	if (strncmp(proj_name,"utm",3) != 0)
	   G_fatal_error("This program can only create a UTM vector file and this is not a UTM mapset.") ;
		
	if (G_legal_filename (file_name) < 0)
	{
	    fprintf (stderr, "<%s> illegal name\n", file_name);
	    exit(-1);;
	}

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
		fprintf( stderr, "    -e   encompass the GIS window\n") ;
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
	init_quad_struct( &quads_info, &window_ll) ;
	find_quad_point (&quads_info, &window_ll, &flags) ;
	calculate_quads ( &quads_info, &window_ll, &flags) ;

    /*  initialize and write the digit vector header  */
	if (flags.vectors)
	{
		printf("\n Creating vector file of quads...") ;
		init_header ( fp_digit, &window, &d_head) ;
		/*new with Vlib*/
		Vect_copy_head_data (&d_head, &Map.head);
		write_quads ( fp_digit, &quads_info, &Map ) ;
		/*fclose (fp_digit) ;*/
		Vect_close (&Map);
	}

	if (flags.sites)
	{
		printf("\n\n Creating sites file ...") ;
		sites_quads ( fp_sites, &quads_info ) ;
		fclose (fp_sites) ;
	}

	if (flags.reg)
	{
		printf("\n\n Creating reg file of `reg.quads`...") ;
		reg_quads ( fp_reg, &quads_info ) ;
		fclose (fp_reg) ;
	}

    /*  this is in the future  */
	if (flags.windows)
	{
		printf("\n\n Creating region files with names `quad.#`...") ;
    	/*  make sure directory is there  */
		G__make_mapset_element( WIND_DIR) ;
		G_get_window(&work_window) ;

		window_quads ( &quads_info, &work_window) ;
	}


	printf("\n\n") ;
	report_quads ( stdout, &quads_info, &flags ) ;
	printf("\n") ;

}		/*  main()  */


check_args( argc, argv, filename, flags) 
	int  argc ;
	char  *argv[] ;
	char  *filename ;
	struct  command_flags  *flags ;
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

dashed_args(  args, flags) 
	char  *args ;
	struct  command_flags  *flags ;

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
