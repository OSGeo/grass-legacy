#define MAIN
#include    <stdio.h>
#include    <unistd.h>
#include    <sys/stat.h>
#include    "gis.h"
#include    "Vect.h"

#include    "stc123.h"
#include    "defines.h"
#include    "externs.h"
#include    "globals.h"
#include    "globals2.h"



/* 
**  Last modified by David Stigberg 11/14/1994
**
**
**  Usage:
**   v.out.sdts [-am] input=name [path=name] output=name
**
**  Flags:
**    -a   Transfer AREA type Lines only; omit LINE types and their attributes
**    -m   Access user-defined metadata file created with v.sdts.meta
**
**  Parameters:
**     input   vector input file
**     path   path for SDTS output dataset
**     output   prefix for SDTS output files (4 characters)
**
*/

/*
#define DEBUG
*/

int legal_SDTS_file_prefix();
char * get_err_mess();
double dig_unit_conversion ();

main(argc, argv)
    int argc;
    char **argv;
{
    char *dig_name, *sdts_prefix;
	char full_dig_name [100];
	char sdts_path [300];
    char *mapset;
    char errmsg[200];
    struct Option *old, *path, *new;
	struct Flag *m_flag, *aline_flag;
	int meta_flag;
	struct stat info;

    char *spheroid_list();
    int zone;
	int ll_input;



/* Show advertising */
    G_gisinit(argv[0]) ;

    aline_flag = G_define_flag();
    aline_flag->key = 'a';
    aline_flag->description = "Transfer AREA type Lines only; omit LINE types and their attributes";

    m_flag = G_define_flag();
    m_flag->key = 'm';
    m_flag->description = "Access user-defined metadata file created with v.sdts.meta";

    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
    old->gisprompt		= "old,dig,vector";
    old->description		= "vector input file";
    
    path = G_define_option();
    path->key			= "path";
    path->type			= TYPE_STRING;
    path->required		= NO;
    path->multiple		= NO;
	/*
    path->gisprompt		= "new,sdts,sdts";
	*/
    path->description		= "path for SDTS output dataset";

    new = G_define_option();
    new->key			= "output";
    new->type			= TYPE_STRING;
    new->required		= YES;
    new->multiple		= NO;
	/*
    new->gisprompt		= "new,sdts,sdts";
	*/
	new->checker		= legal_SDTS_file_prefix;
    new->description		= "prefix for SDTS output files (4 upper-case chars and/or digits)";


    if (G_parser (argc, argv))
	exit (-1);

    dig_name = old->answer;

/*
	if (legal_SDTS_file_prefix (new->answer) != 1)
	{
		sprintf (errmsg, "<%s> is not a legal SDTS file prefix; must be 4 upper-case characters and/or digits.\n", new->answer);
	    G_fatal_error (errmsg);
	}
*/
    sdts_prefix = new->answer;

    if (!*dig_name  || !*sdts_prefix )
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
	    G_usage();
        exit (-1);
    }

	if (path->answer)
	{

	   if ((stat (path->answer, &info) == 0)  && (S_ISDIR (info.st_mode) != 0)
		   && (access (path->answer, W_OK) == 0) )
	   {
			sprintf (sdts_path, "%s/%s", path->answer, sdts_prefix);
	   }
	   else
	   {
		 sprintf (errmsg, "Invalid path specification: '%s'\n", path->answer);
		 G_fatal_error (errmsg);
	   }
	}
	else
	{
	   if (access (".", W_OK) == 0)
		  sprintf (sdts_path, "./%s", sdts_prefix);
       else
		  G_fatal_error ("Cannot create SDTS output directory under current directory.\n");
    }

    mkdir (sdts_path, 0777);

	strcpy (full_dig_name, dig_name);

    printf("\n\n   Export SDTS:\n\n") ;

	
    if ((mapset = G_find_vector (dig_name, G_mapset())) == NULL)
    {
		sprintf (errmsg, "Could not find Vector file <%s>\n", dig_name);
		G_fatal_error (errmsg);
    }

	/*if map is found, make sure it's really in your own mapset*/
	if (strcmp (mapset, G_mapset()) != 0)
	{
		sprintf (errmsg, "Currently, only files in your mapset can be exported.\n <%s> cannot be exported.\n", full_dig_name);
		G_fatal_error (errmsg);
	}


#ifndef NO_CONVERSION

/*This code is left from an earlier version, when the TVP required data to be
  in lat-lon.  the code here and elsewhere is left in case utm-lat-lon 
  conversion is desired/required at some future date.

  NO_CONVERSION is defined in defines.h

*/

/**
**   check to see if lat/lon conversion is necessary, and if so, initialize
**   conversion routines
*/
    ll_input = lat_flag->answer;

    if (!ll_input)
    {
        U_to_ll = 1;
        zone = setup_ll_conv (zone_opt->answer, spheroid->answer, spheroid->key
, south_flag->answer);
        Zone = zone;
    }
    else
       U_to_ll = 0;

#endif /* NO_CONVERSION */

    meta_flag = m_flag->answer;
    Aline_only = aline_flag->answer;

	/*set dummy variables, place-holders for lat-lon conversion*/
	   ll_input = 0;
	   zone = 0;

    export (dig_name, mapset, sdts_prefix, sdts_path, 
		   ll_input,  zone, meta_flag); 
	 

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


struct Map_info Map;

export(dig_name, mapset, sdts_prefix, sdts_path, ll_input, zone, meta_flag )
    char *dig_name, *mapset, *sdts_prefix, *sdts_path;
	int ll_input, zone, meta_flag;
{
	char out_name1[250];
	char out_name2[250];
	int level;
	char sdts_base1[250];
	char sdts_base2[250];
	char sdts_base3[250];
	int failed_modules = 0;

	if ( ! mapset)
		G_fatal_error ("No mapset specified.\n");

/*FILL MAP STRUCT WITH VECTOR DATA TO BE EXPORTED*/
	level = Vect_open_old (&Map, dig_name, mapset);
	if (level <= 0)
	    G_fatal_error ("Can't open vector file");

	if (level < 2 || !Map.all_areas || !Map.all_isles)
	{
	    fprintf (stderr, "\n\nYou must first run v.support on this data.\n");
	    exit (1);
	}


/*GET PROJECTION AND COORDINATE INFORMATION*/

	 get_metadata (&Map, meta_flag, dig_name, mapset);

	 init_SDTS_proj_info (&Map);


/* Build now polygons from LINE-type lines, make sure every line has
   left/right polygons attached to it */	

	build_all_polygons (&Map);


/* OBSOLETE: Build Universe polygon.  

	build_area_one (&Map); 
*/

	shuffle_dots (&Map);


/*BUILD SDTS OUTPUT FILE NAMES*/

	strcpy (sdts_base1, sdts_prefix);
	strcpy (sdts_base2, sdts_prefix);
	strcpy (sdts_base3, sdts_prefix);

/*NODES: BUILD PLANAR_NODE OUTPUT FILE TITLE AND FULL PATH */
	/*open file for nodes: [sdts_prefix]no01.DDF*/
	strcat (sdts_base1, "NO01.DDF");

	sprintf (out_name1, "%s/%s", sdts_path, sdts_base1);

	printf ("Writing Planar Node Module\n");
    if (!write_SDTS_nodes (&Map, out_name1, sdts_base1, ll_input, zone))
	{
		 fprintf (stderr, "Error: %s\n", get_err_mess());
		 fprintf (stderr, "\nFailed to process Planar Nodes. Continuing.\n");
		 clear_err_mess ();
		 failed_modules++;
		 
	} 

/*LINES: BUILD COMPLETE CHAIN OUTPUT FILE*/

	strcpy (sdts_base1 + 4, "LE01.DDF");
	sprintf (out_name1, "%s/%s", sdts_path, sdts_base1);

	strcpy (sdts_base2 + 4, "NE01.DDF");
	sprintf (out_name2, "%s/%s", sdts_path, sdts_base2);

	printf ("Writing Complete Chain and Entity Points Modules\n"); 

    if (!write_SDTS_le_and_ne (&Map, out_name1, sdts_base1, out_name2, sdts_base2, ll_input, zone))
	{
		 fprintf (stderr, "Error: %s\n", get_err_mess());
		 fprintf (stderr, "\nFailed to process Lines and Entity Points. Continuing.\n");
		 clear_err_mess ();
		 failed_modules++;
	} 


/*AREA: BUILD AREA and Area-Point modules*/

	strcpy (sdts_base1 + 4, "PC01.DDF");
	sprintf (out_name1, "%s/%s", sdts_path, sdts_base1);

	strcpy (sdts_base2 + 4, "NA01.DDF");
	sprintf (out_name2, "%s/%s", sdts_path, sdts_base2);

	printf ("Writing GT-Polygon and Area Point Modules\n"); 
    if (!write_SDTS_areas (&Map, out_name1, sdts_base1, out_name2, sdts_base2, ll_input, zone))
	{
		 fprintf (stderr, "Error: %s\n", get_err_mess());
		 fprintf (stderr, "\nFailed to process Areas and Area Points. Continuing.\n");
		 clear_err_mess ();
		 failed_modules++;
	} 


/*AREA: BUILD Attribute Module*/

	strcpy (sdts_base1 + 4, "AP01.DDF");
	sprintf (out_name1, "%s/%s", sdts_path, sdts_base1);

    if (!write_SDTS_atts (&Map, out_name1, sdts_base1, ll_input, zone, dig_name, mapset))
	{
		 fprintf (stderr, "Error: %s\n", get_err_mess());
		 fprintf (stderr, "\nFailed to process AP01 attribute module. Continuing.\n");
		 clear_err_mess ();
		 failed_modules++;
	}; 

    if (!write_SDTS_quality_mods (sdts_prefix, sdts_path, dig_name))
	{
		 fprintf (stderr, "\nFailed to process one or more Data Quality modules. Continuing.\n");
		 clear_err_mess ();
		 failed_modules++;
	}; 

    if (!write_SDTS_dict_mods (&Map, sdts_prefix, sdts_path, dig_name))
	{
		 fprintf (stderr, "\nFailed to process dictionary modules. Continuing.\n");
		 clear_err_mess ();
		 failed_modules++;
	}; 

    if (!write_SDTS_global_mods (&Map, sdts_prefix, sdts_path, dig_name, zone, ll_input))
	{
		 fprintf (stderr, "\nFailed to process global modules. Continuing.\n");
		 failed_modules++;
	}; 

    if (!write_SDTS_stat (&Map, sdts_prefix, sdts_path))
	{
		 fprintf (stderr, "\nFailed to process stats module. Continuing.\n");
		 failed_modules++;
	}; 



/**done module writing*/

	Vect_close (&Map);

	if (failed_modules)
	{
	  fprintf (stderr, "WARNING:\n");
	  fprintf (stderr, "At least %d SDTS output file(s) is missing,\n", failed_modules);
	  fprintf (stderr, "or contain(s) incomplete or corrupt data.\n");
	  fprintf (stderr, "\n");
	  fprintf (stderr, "Export process is complete but flawed.\n");
	}
	else
	  fprintf (stderr, "SDTS files completed.\n");

	return (1) ;
}


