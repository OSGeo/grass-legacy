
/*
*  This program is a top-level menu that runs a multitude of MAPDEV programs
*  trying to create the files necessary to for the 'digit' program.
*
*  Programs used by this program:
*      (/usr/gis/etc)  a.b.dlg, dlg.to.dig, a.b.vect, build.vect
*      
*      dlg.to.dig also forces all unlabeled Area lines to Line lines.
*
*  Data files used by this program:
*      ascii dlg,  binary dlg,  ascii vector,  binary vector,  dig_att
*  
*  Note: all the programs that this calls must exit with a zero value on 
*  successful completion and a true value on error.
*/


#include	<stdio.h>
#include	<signal.h>
#include	"gis.h"

/**  data directories   **/
#define		B_DIG		"dig"
#define		A_DIG		"dig_ascii"
#define		B_DLG		"bdlg"
#define		A_DLG		"dlg"
#define		ATT		"dig_att"
#define		PLUS		"dig_plus"

static char  *current_mapset ;
static char  *gbase ;

static char  *PROG ;

main (argc, argv)
	int  argc ;
	char  *argv[] ;
{

	int   phase ;  /*  current level  */
	int   level ;  /* how many levels to convert to digit vector format  */
	int   force_areas ;

	int   snap ;
	int   ram ;

	char  name[128] ;

	/*  store filename and path  */
	char  file1[128] ;
	char  file2[128] ;
	char  file3[128] ;
	char *p;
	char  *getenv();

	char  command[256] ;

	G_gisinit(argv[0]) ;
	system("clear") ;
	setbuf(stdout, NULL) ;

	phase = 0 ;
	PROG = argv[0] ;
	gbase = G_gisbase() ;
	current_mapset = G_mapset() ;

	level = ask_which_level() ;

	/* decide if we override the default for using the RAM file */
	ram = 0 ;	 /* default */
#ifdef CERL_FOO
	/* cancel this. */
	ram = 1;	/* if cerl turn it on */
#endif
	p = getenv ("GTUNE_VSUP_RAM");	/* if TUNE var is set */
	if (p != NULL)
	    if (!strcmp ("ON", p))	/* on */
		ram = 1;
	    else
		ram = 0;		/* else off */




/**************************  phase:   a.b.dlg  **************************/

	if (level == 1 )
	{
		ask_for_name( file1, " DLG FILENAME ", name, A_DLG, "ascii dlg") ;
		force_areas = ask_force() ;
		snap = ask_snap()  ;
		show_phase( &phase, "Convert ascii dlg file to binary dlg file.\n") ;
		run_a_b_dlg( command, name, file1, file2) ;
	}


/**************************  phase:  dlg.to.digit  **************************/
/*  file2 - binary dlg,   file1 - binary digit,   file3 - att file    */

	if (level < 3 )
	{

		/*  starting with binary dlg file  */
		if (level == 2 )
		{
			ask_for_name( file2, " DLG FILENAME ", name, B_DLG, "binary dlg") ;
			force_areas = ask_force() ;
			snap = ask_snap()  ;
		}

		show_phase( &phase, "Convert binary dlg file to binary digit file.\n") ;
		run_dlg_to_digit( command, name, file2, file1, file3, force_areas) ;
	}



/**************************  phase:  a.b.vect  **************************/
/*  file2 - ascii dig,   file1 - binary vector               */

	if (level == 3 )
	{
		ask_for_name( file2, " VECTOR (DIGIT) FILENAME ", name, A_DIG, "ascii vector") ;
		snap = ask_snap()  ;
		show_phase( &phase, "Convert ascii vector file to binary vector file.\n") ;
		run_a_b_vect( command, name, file2, file1) ;
	}



/**********************  final phase:  build.vect  ***********************/
/*    file1 - binary digit    */


	if (level == 4)
	{
		ask_for_name( file2, " VECTOR (DIGIT) FILENAME ", name, B_DIG, "binary vector") ;
		snap = ask_snap()  ;
	}

	show_phase( &phase, "Create vector support files.\n") ;
	run_build_vect( command, name, snap, ram) ;



}		/*  main()  */


ask_which_level()
{

	int		num ;
	char	buf[80] ;

	fprintf (stdout,"\n     Imports to GRASS Vector Format and Creates Needed Support Files\n\n") ;
	fprintf (stdout,"     1  -   Ascii DLG file  to GRASS Vector Format\n") ;
	fprintf (stdout,"     2  -   Binary DLG file to GRASS Vector Format\n") ;
	fprintf (stdout,"     3  -   Ascii VECTOR file  to GRASS Vector Format\n") ;
	fprintf (stdout,"     4  -   Binary VECTOR file to GRASS Vector Format\n") ;
	fprintf (stdout,"\n\n Enter a number <1-4>\n anything else to quit: ") ;

	if (fgets (buf,80,stdin) == NULL)
		clearerr (stdin), exit (1) ;

	num = atoi(buf) ;
	if( num < 1  ||  num > 4)
		fprintf(stderr, "\n\n ...Leaving %s\n\n", PROG),   exit(1) ;

	return(num) ;

}


show_phase( phase, describe)
	int	*phase ;
	char	*describe ;
{
	++*phase ;
	fprintf(stderr, "\n\n   ----- PHASE %d ----->  %s\n\n", *phase, describe) ;

}

ask_for_name( file_name, header, name, dir, file_desc)
	char	*file_name, *header,  *name,  *dir, *file_desc ;
{

	char	*mapset ;

	mapset = G_ask_old( header, name, dir, file_desc, 0) ;

	if ( ! mapset)
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;



	/* ask user if map is in same units as mapset */
	{
	    int proj;
	    char buf[1024];
	if (NULL == G_find_file ("dig", name, G_mapset ()))
	{
	    proj = G_projection ();
	    if (proj == 0)
	    {
		if (!G_yes ( "Mapset units are undefined. Continue? ", 1))
		    exit (-1);
	    }
	    else
	    {
		fprintf (stdout, "\n\nCurrent mapset is %s.\n", G_database_projection_name ());
		sprintf (buf, "  Is this map in %s %s? ", 
		  G_database_projection_name (), 
		  G_database_unit_name (1));
		if (!G_yes (buf, 1))
		{
		    fprintf (stdout, "Sorry, GRASS does not currently support mixing map units\n");
		    exit (-1);
		}
		else
		    fprintf (stdout, "Thank You\n");
		fprintf (stdout, "\n");
	    }
	}
	}
	return (0);


}	/**  ask_for_name()  **/


ask_force()
{
fprintf (stdout,"\n During the DLG-3 file conversion to Vector:\n") ;
fprintf (stdout," Determine if this map is composed of Area or Line information.\n") ;
return( G_yes( " Do you want to give precedence to Areas (opposed to Lines) ", 1)) ;

}

ask_snap()
{
fprintf (stdout,"\n\n During the building of the Vector format:\n") ;
return( G_yes(" Do you want to snap nodes to other nodes within a threshold ", 0) ) ;
}



/** ----- functions to run the commands ------ **/


run_a_b_dlg( command, name, ascii_dlg, binary_dlg)
	char	*command, *name, *ascii_dlg, *binary_dlg ;
{

	G__file_name( binary_dlg, B_DLG, name, current_mapset) ;
	G__make_mapset_element(B_DLG) ;

/*  execute the a.b.dlg program  */
	sprintf( command, "%s/etc/a.b.dlg %s %s ", gbase, ascii_dlg, binary_dlg) ;

	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert ascii dlg file: '%s' to binary dlg file: '%s'\n", PROG,  ascii_dlg, binary_dlg) ;
		exit(-1) ;
	}

}  



run_dlg_to_digit( command, name, binary_dlg, binary_dig, att, force_areas)
	char	*command, *name, *binary_dlg, *binary_dig, *att ;
	int	force_areas ;
{

	G__file_name( binary_dig, B_DIG, name, current_mapset) ;
	G__make_mapset_element(B_DIG) ;

	G__file_name( att, ATT, name, current_mapset) ;
	G__make_mapset_element(ATT) ;

/*  execute the program  */
	sprintf( command, "%s/etc/dlg.to.digit %s %s %s ", gbase, binary_dlg, binary_dig, att) ;

	/*  assumes lines making up unlabeled areas are Area edges
	*  unless specified, then their Lines
	*/

	if( !force_areas)
		strcat( command, "line") ;


	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert binary dlg file: '%s' to binary digit file: '%s'\n", PROG, binary_dlg, binary_dig) ;
		exit(-1) ;
	}

} 


run_a_b_vect( command, name, ascii_vector, binary_vector)
	char	*command, *name, *ascii_vector, *binary_vector ;
{

	G__file_name( binary_vector, B_DIG, name, current_mapset) ;
	G__make_mapset_element(B_DIG) ;


/*  execute the program  */
	sprintf( command, "%s/etc/a.b.vect %s %s", gbase, ascii_vector,
		binary_vector) ;


	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert ascii vector file: '%s' to binary vector file: '%s'\n", PROG, ascii_vector, binary_vector) ;
		exit(-1) ;
	}

} 



run_build_vect( command, name, snap, ram)
	char	*command, *name ;
	int	snap,  ram ;
{

	G__make_mapset_element(ATT) ;
	G__make_mapset_element(PLUS) ;

/*
*  Usage:  build.vect  mapset  file_name snap=["yes", "no"] ram=["yes", "no"]
*    snap:    "no" to leave nodes as they are,
*             "yes" to snap nodes
*    ram:     "no" to read/write strictly from file
*             "yes" read everything into memory
*    thresh:  "no" use the default thresh value
*             "yes" user wants to set own thresh value for snapping
*/


	sprintf( command, "%s/etc/build.vect  %s  %s  %s  %s thresh=no", gbase,
		current_mapset, name, snap ? "snap=yes": "snap=no",
		ram ? "ram=yes" : "ram=no") ;


	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not build vector file: '%s'\n"
			, PROG, name) ;
		exit(-1) ;
	}


return(0) ;


} 
