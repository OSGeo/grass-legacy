/*  @(#)import_gef.c	1.0  11/20/89  
* Created by: R.L.Glenn, SCS, CGIS Division
*/

/*
*  This program is a user interface to the SCSGEF import routine
*  
*  Note:  SCSGEF comes in two(2) flavors
*          old - 72 char. header file, 24 char. lines data file, and
*                28 char. text data file.
*          new - 72 char. header file, 40 char. lines and text files.
*
*         The user is assumed to have created the combination of these
*         three files into one resultant file. 
*/

#include	<stdio.h>
#include	<stdlib.h>
#include	<signal.h>
#include	"gis.h"

/**  data directories   **/
#define		B_DIG		"dig"
#define		A_DIG		"dig_ascii"
#define		B_DLG		"bdlg"
#define		A_DLG		"dlg"
#define		ATT		"dig_att"
#define		PLUS		"dig_plus"
#define         SUBJ            "SUBJ"
#define         GEF             "gef"

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

	char  name[256] ;

	/*  store filename and path  */
	char  file1[128] ;
	char  file2[128] ;
	char  file3[128] ;

	char  command[256] ;
	int ier, format;
	char location[60], *LOCATION;
	char cat_name[60], *catset, cats_file[60];  
        char gef_name[60], dig_name[60], *mapset;

	G_gisinit("IMPORT_TO_DIGIT") ;
	system("clear") ;
	setbuf(stdout, NULL) ;

	phase = 1 ;
	PROG = argv[0] ;
	gbase = G_gisbase() ;
	current_mapset = G_mapset() ;


/**************************  phase:   imp_gef  **************************/

                if (!ask_if_join()) exit(0);

                format = ask_for_flavor();

		ask_for_name( file1, " SCSGEF FILENAME ",
                                           name, GEF, "ascii gef") ;
                                        /* Save gef file name */
                strcpy(gef_name,name);

                mapset = G_ask_any (" VECTOR (DIGIT) FILENAME ",
                                           name, A_DIG, "ascii vector",1);
	        if ( ! mapset)
		        exit(0) ;
                strcpy(dig_name,name);
                G__file_name (file2, A_DIG, name, mapset);

					/* Make Master Category dir
							if not existing */
 	        G__make_mapset_element("SUBJ") ;
 	        G__make_mapset_element("dig_cats") ;

	        catset = G_ask_any( " SUBJECT MATTER",
                                           name, "SUBJ", "subject", 0) ;
	        if ( ! catset)
		        exit(0) ;
	        G__file_name( cat_name, SUBJ, name, catset) ;

			 /* make sure command is empty */
                command[0] = '\0';
		strcpy(name,gef_name);

		run_imp_gef( command, name, file1, file2, cat_name, format);

		show_phase( &phase, "Convert ascii dig file to binary dig file.\n") ;
		run_a_b_vect( command, dig_name, file2, file1) ;

/*-------------------------------------------------------------------*/

   	        show_phase( &phase, "Create digit support files.\n") ;
	        run_build_vect( command, dig_name, snap, ram) ;

}		/*  main()  */




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

	if ( ! mapset )
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

        if (strcmp(header," ATTRIBUTE FILENAME ") ) return(0);
	/* ask user if map is in same units as mapset */
	{
	    int proj;
	    char buf[1024];
	if (NULL == G_find_file ("dig", name, current_mapset ))
	{
	    proj = G_projection ();
	    if (proj == 0)
	    {
		if (!G_yes ( "Mapset units are undefined. Continue? ", 1))
		    exit (-1);
	    }
	    else
	    {
		fprintf (stdout, "\n\nCurrent mapset is %s.\n", G_projection_name (proj));
		sprintf (buf, "  Is this map in %s %s? ", 
		  G_projection_name (proj), 
		  G_unit_name (G_projection_units(proj)));
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

ask_if_join()
{
fprintf (stdout,"\n\n") ;
return( G_yes(" Have you joined all 3 GEF files into one file ? ",1) );
}

ask_for_flavor()
{
fprintf (stdout,"\n\n") ;
return( G_yes(" Is the SCSGEF data in the new (40 char.) format ? ",1) );
}


/** ----- functions to run the commands ------ **/


/*------------------------------------------------11/21/89--RLG,SCS--*/

run_imp_gef( command, name, ascii_gef, ascii_dig, catag_file, fmt)
	char	*command, *name, *ascii_gef, *ascii_dig, *catag_file ;
        int   fmt;
{

	G__make_mapset_element(A_DIG) ;

/*  execute the SCS imp_gef program  */
        if (fmt)       /* new format */
	      sprintf( command, "%s/etc/imp_gef %s %s %s ", 
                        gbase, ascii_gef, catag_file, ascii_dig) ;
        else           /* old format */
	      sprintf( command, "%s/etc/imp_gef -o %s %s %s ", 
                        gbase, ascii_gef, catag_file, ascii_dig) ;

/*      fprintf(stderr,"%s\n",command); */
        if (system( command) ) 
	{
		fprintf(stderr, "ERROR(%s):  Could not convert ascii GEF file: '%s' to ascii dig file: '%s'\n", PROG,  ascii_gef, ascii_dig) ;
		exit(-1) ;
	}
}

/*------------------------------------------------------------RLG----*/


run_a_b_vect( command, name, ascii_vector, binary_vector)
	char	*command, *name, *ascii_vector, *binary_vector ;
{

	G__file_name( binary_vector, B_DIG, name, current_mapset) ;
	G__make_mapset_element(B_DIG) ;


/*  execute the program  */
	sprintf( command, "%s/etc/a.b.vect %s %s ", gbase, ascii_vector,
		binary_vector) ;

/*      fprintf(stderr,"%s\n",command);*/
        if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert ascii digit file: '%s' to binary digit file: '%s'\n", PROG, ascii_vector, binary_vector) ;
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

/*      fprintf(stderr,"%s\n",command);   */
  	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not build digit file: '%s'\n"
			, PROG, name) ;
		exit(-1) ;
	}


return(0) ;


} 
