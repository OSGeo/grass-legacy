/*
*
*   GRASS4.0: converted to new parser - 1/91 -- dks
*/
#include    <stdio.h>
#include    "gis.h"
/*
#include    "dig_structs.h"
*/
#include    "digit.h"
#include    "dig_head.h"

/*  will install this after fixing  digit code to be able to handle
*   negative areas (islands)
#define ISLANDS 1
*/

#define MAIN
#define METERS_PER_INCH 0.0254

#define  USAGE  "mapset  file_name snap=[yes, no] ram=[yes, no]"

/*
#define DEBUG
*/
/* still have to add the code to rebuild att file */
/* to guarantee that it is in our fixed field format */

/*  command line args and flags  */
/*  defaults are RAM off and snapping off  */
static  int   RAM_OK = 0 ;
static	int   snap_ok = 0 ;
static	double   snap_val = 0.0 ;
static	int   thresh_flag = 0 ;
static	char  *mapset = NULL ;
static	char  *name = NULL ;
static  int do_islands = 1; /*ISLE*/


double dig_unit_conversion ();
static	int   snapped = 0 ;
static int ident_only = 0;

main(argc, argv)
    int argc;
    char **argv;
{
    int   status ;
    int   ret ;
    double     atof ();
    double    val;
    char buf[1024];
    int tot_atts;

    FILE    *fp_plus ;

    struct Map_info Map ;
    struct Plus_head Plus ;
    char *memptr;        /* for load_file () */
    char *file_name;


   struct Flag  *s_flag, *p_flag;
#ifdef RAM
    struct Flag *ram_flag;
#endif /*RAM*/
   struct Option *map, *s_val;
   struct Flag *ident;


    setbuf(stdout, 0) ;

/* Show advertising */
    G_gisinit(argv[0]) ;

/*****************************COMMAND PARSER******************************/
    map = G_define_option();
    map->key			= "map";
    map->type			= TYPE_STRING;
    map->required		= YES;
    map->multiple		= NO;
    map->gisprompt		= "old,dig,vector";
    map->description		= "vector file name";

    
    s_val = G_define_option();
    s_val->key			= "threshold";
    s_val->type			= TYPE_DOUBLE;
    s_val->required		= NO;
    s_val->multiple		= NO;
    s_val->description		= "snap threshold value";

    ident = G_define_flag ();
    ident->key 			= 'i';
    ident->description 		= "Only run identical line tests";

    if (G_parser (argc, argv))
	exit (-1);

    file_name = map->answer;

    if (snap_ok == 1)
    {
	if (s_val->answer != NULL)
	    snap_val = atof(s_val->answer);
	thresh_flag = 0;
    }
    else
    {
	snap_val = 0.0;
	thresh_flag = 0;
    }

    ident_only = ident->answer == NULL ? 0 : 1;

    if ( !file_name || !*file_name )
    {
        fprintf (stderr, "%s: Command line error: missing map name.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }
/*****************************COMMAND PARSER******************************/
/*  init Map structure and show header info  */
    init_plus_struct(&Plus) ;
    init_map_struct(&Map) ;


/*  open dig and dig plus file  */
    if ( open_dig_files( file_name, &fp_plus, &Map, &Plus ) )
   	 exit(-1) ;

    printf("\nSelected information from vector header\n") ;
    printf(" Organization:  %s\n", Map.head.organization) ;
    printf(" Map Name:      %s\n", Map.head.map_name) ;
    printf(" Source Date:   %s\n", Map.head.source_date) ;
    printf(" Orig. Scale:   %ld\n", Map.head.orig_scale) ;



    {
/*DEBUG*/ debugf ("MAIN:scale %d  unit %lf\n", Map.head.orig_scale, dig_unit_conversion ());
	Map.snap_thresh = THRESH_FUDGE * Map.head.orig_scale * dig_unit_conversion();
/*DEBUG*/ debugf ("MAIN:  snap_thresh %lf\n", Map.snap_thresh);
	Map.head.map_thresh = Map.snap_thresh;
    }
    
    if (snap_ok)
    {
	if (snap_val == 0.0)
	    Map.snap_thresh = Map.head.map_thresh; 
	else
	    Map.snap_thresh = snap_val; 
    }
    else
	Map.snap_thresh = 0.0;

    if (thresh_flag)
    {
	printf ("Enter snapping threshold [default=%7.2lf]: ", Map.snap_thresh);
	fflush (stdout);
	gets (buf); 

	dig_rmcr (buf);  G_squeeze (buf);
	if (strlen (buf) && (val = atof (buf)) > 0.0)
	{
	    Map.snap_thresh = val;
	    /*
	    head.map_thresh = Map.snap_thresh;
	    */
	}
    }


    if (Map.snap_thresh == 0.0)
	printf (" No snapping will be done \n");
    else
	printf (" Snapping threshold %7.2lf\n\n", Map.snap_thresh) ;

    printf ( "\n");
    printf ( "    Reading Vector file.\n");

/****************************************/
    /* attempt to load dig file into memory for next step */
    /* if this is not desired just comment out the load and release parts*/
    /* if malloc fails, the program will continue on working with the file */


    /* check to see if it is efficient to load entire file into memory.
    ** This needs to be tuned.  Our concerns are: 
    **        Are there any area lines?
    **        Area there many more line lines than area lines?
    **        Is the file too big?
    **        If the file is small, lets just do it
    */
#ifdef MEMORY_IO	/* not yet upgraded for 4.0 */
#ifndef RAM_OFF
    if(RAM_OK && Map.n_alines && 
      (Map.n_alines / (Map.n_llines+.5) > 1.5 || Map.n_lines < 2000))
    if (dig_Mem_load_file (Map.digit, &memptr) >= 0)
        printf ("    Using RAM file\n");
#endif
#endif
/****************************************/


/****************************************/
/*                      		*/
/*Here it is:           		*/
/*            VVVVVVVVVV		*/
/****************************************/
    init_extents (); /* TEST.. was commened out let v.build do this*/
    snapped = read_digit( &Map, &Plus);
    if (snapped < 0)
	fprintf (stderr, "Could not build support (dig_plus) file.\n"), exit (-1);

#ifdef FROM_BUILD
    printf ("    Building areas\n");
    build_all_areas (&Map);

    if (do_islands) /*ISLE*/
    {
	printf ("    Building islands\n");
	matchup_isles (&Map); /*ISLE*/
    }

    printf ("    Attaching labels\n");
    clean_atts_file (Map.att_file);
    tot_atts = read_atts (&Map, Map.att_file);
#endif
    /* TODO: try cleaning before and after for now */
    if (!ident_only)
    {
	printf ("    Cleaning lines less than thresh\n");
	clean_lines (&Map, Map.snap_thresh);
    }

    printf ( "Identical lines check");
    if (!ident_only)
	printf ("  and   Line intersections");
    printf ( "\n");

    intersect (&Map, ident_only);

    if (!ident_only)
    {
	clean_lines (&Map, Map.snap_thresh);
    }

    compress (&Map, 1);

    printf ("    Building areas\n");
    build_all_areas (&Map);

    if (do_islands) /*ISLE*/
    {
        printf ("    Building islands\n");
        matchup_isles (&Map); /*ISLE*/
    }

    printf ("    Attaching labels\n");
    clean_atts_file (Map.att_file);
    tot_atts = read_atts (&Map, Map.att_file);



/****************************************/
    /* release memory file */
    /* this only has effect if "Using RAM file" */

#ifdef MEMORY_IO	/* not yet upgraded for 4.0 */
    dig_Mem_release_file ();
#endif
/****************************************/

    printf (" Number of lines:   %d\n", Map.n_lines) ;
    printf (" Number of nodes:   %d\n", Map.n_nodes) ;
    printf (" Number of areas:   %d\n", Map.n_areas) ;
    printf (" Number of isles:   %d\n", Map.n_isles) ;
    printf (" Number of atts :   %d\n", Map.n_atts) ;

    printf (" Number of unattached atts :   %d\n", tot_atts-Map.n_atts) ;
    printf (" Snapped lines  :   %d\n", snapped) ;

/*==========================================*/
    if (!ident_only)
      printf ("WARNING: Beta version. You must run v.support before using this data\n");
/*==========================================*/

/*  write out all the accumulated info to the plus file  */
    Plus.all_areas = 1;
    if (do_islands) /*ISLE*/
	Plus.all_isles = 1;
    else
	Plus.all_isles = 0; 


/*==========================================*/
/* TODO:  For now make this file unuseable without first running v.support */

    if (!ident_only)	/* for ident_only, is ok */
    {
	Plus.all_areas = 0;
	Plus.all_isles = 0;
    }

/*==========================================*/
    dig_map_to_head (&Map, &Plus);

/*  clean up files  */

    fflush(Map.dig_fp) ;
    fflush(Map.att_fp) ;

    if (0 > dig_write_plus_file (fp_plus, &Map, &Plus))
    fprintf (stderr, "Error writing out support (dig_plus) file.\n"), exit (-1);

    /*  Note, not using the library to update head, and close files */


    /* Re-write out the header w/ the possibly changed window */
    /* 4.0  dpg */
    /*Vect__read_head_binary  (&Map, &(Map.head));*/
    update_head_from_ext (&(Map.head));
    Vect__write_head_binary (&Map, &(Map.head));

    fclose(Map.dig_fp) ;
    fclose(Map.att_fp) ;


    fclose(fp_plus) ;

    exit (0);
}

#ifdef DEBUG
debugf (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
    fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#endif

#ifdef DEBUG2
debugf2 (format, a, b, c, d, e, f, g, h, i, j, k, l)
    char *format;
    int a, b, c, d, e, f, g, h, i, j, k, l;
{
    fprintf (stderr, format, a, b, c, d, e, f, g, h, i, j, k, l);
}
#else
debugf2 ()
{
}
#endif
