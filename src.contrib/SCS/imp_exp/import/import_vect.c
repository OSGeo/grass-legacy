/* @(#)import_vect.c	1.5   03/91 GRASS 4.0 */
/* @(#)import_vect.c	1.4   04/90 */
/*
*  This program is a top-level menu that runs a multitude of MAPDEV programs
*  trying to create the files necessary to for the 'digit' program.
*
*  Programs used by this program:
*      (/usr/gis/etc)  a.b.dlg, dlg.to.dig, a.b.vect, v.build
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
#define		CAT		"dig_cats"
#define		PLUS		"dig_plus"
#define         SUBJ            "SUBJ"
#define         GEF             "gef"
#define         ARC             "arc"
#define         DXF             "dxf"
#define         TIG             "tiger"
#define V_PROG1  "v.in.dlg.scs"   /* non-interactive version */
#define V_PROG2  "v.in.scsgef"   /* non-interactive version */
#define V_PROG3  "v.in.arc"   /* non-interactive version */
#define V_PROG4  "v.in.dxf"   /* non-interactive version */
#define V_PROG5  "v.in.tiger"   /* non-interactive version */

static char  *current_mapset ;
static char  *gbase ;

static char  *PROG ;

main (argc, argv)
	int  argc ;
	char  *argv[] ;
{

	int   phase ;  /*  current level  */
	int   level ;  /* how many levels to convert to vector format  */
	int   force_areas ;

	int   snap ;
	int   ram ;

	char  name[256] ;

	/*  store filename and path  */
	char  file1[128] ;
	char  file2[128] ;
	char  file3[128] ;

	char  command[256] ;
	int ier, got_cats, info, format, got_univ;
	char *p, attr_file[60], *attset, dlg_name[60]; 
	char location[60], *LOCATION, *getenv();              
	char tmp_name[60], cat_name[60], *catset, cats_file[60];  
        char gef_name[60], dig_cat_name[60], dig_name[60], *mapset;
	char arc_name[60], dxf_name[60], tig_name[60];

	G_gisinit("IMPORT_TO_DIGIT") ;

        if (argc > 1 && !strcmp (argv[1], "help"))
           {
           fprintf(stderr,"\n\n");
           fprintf(stderr,"v.import is an interactive program which runs other vector\n");
           fprintf(stderr,"   import programs.  It allows you to import DLG, GEF, ARC,\n");
           fprintf (stderr,"  DXF, or ASCII vector files into GRASS vector format.  It \n");
	   fprintf(stderr,"   also runs v.support.\n");
	   fprintf (stderr, "\nThere are NO arguments required.\n\n");
           exit (1);
           }

	system("clear") ;
	setbuf(stdout, NULL) ;

	phase = 0 ;
	PROG = argv[0] ;
	gbase = G_gisbase() ;
	current_mapset = G_mapset() ;

	level = ask_which_level() ;

        /* decide if we override the default for using the RAM file */
		ram = 0 ;        /* default */
#ifdef CERL_FOO
        /* cancel this. */
	ram = 1;        /* if cerl turn it on */
#endif
	p = getenv ("GTUNE_VSUP_RAM");  /* if TUNE var is set */
	if (p != NULL)
	   if (!strcmp ("ON", p))      /* on */
		ram = 1;
           else
		ram = 0;                /* else off */
													 
/**************************  phase:   a.b.dlg  **************************/

	if (level == 1 )
	{
		ask_for_name( file1, " DLG FILENAME ", name, A_DLG, "ascii dlg") ;

		force_areas = ask_force() ;
		snap = ask_snap()  ;

		got_cats = ask_for_cats();
		if ( !got_cats )
		   { 
                                        /* Save dlg file name */
                   sprintf(dlg_name,"%s",name);
                   sprintf(tmp_name,"%s",name);
					/* Make Master Category dir
							if not existing */
 	           G__make_mapset_element("SUBJ") ;
 	           G__make_mapset_element("dig_cats") ;

	           catset = G_ask_any( " SUBJECT MATTER", name, "SUBJ", "subject", 0) ;
	           G__file_name( cat_name, SUBJ, name, catset) ;

		   ask_for_name( attr_file, " ATTRIBUTE FILENAME", name, A_DLG, "ascii") ;
					/* ARC/INFO att. tables must
					    be handled a special way  */
                   info = ask_if_info();
                   if (info) got_univ = 1;
		   else got_univ = ask_if_univ();

                   show_phase( &phase, "Modify ascii dlg file without category codes.\n") ;
		   run_dlg_cat(command, cat_name, attr_file, dlg_name, info);

			/* put the modified attribute file name in
			   with -n (no GRASS codes) flag */
		   sprintf(cats_file,"%s2",attr_file);
			 /* clean up a little */
                   command[0] = '\0';
		   sprintf(name,"%s",dlg_name);

		   show_phase( &phase, "Convert ascii dlg file to binary dlg file.\n") ;

		   run_a_b_SCS(command,name,file1,file2,cats_file,got_univ);

                                        /* copy the SUBJ category file 
					to this dig-map-file category file */
	           G__file_name( dig_cat_name, CAT, tmp_name, catset) ;
                   sprintf(command,"cp %s %s",cat_name,dig_cat_name);
                   system (command); 
                   }

		if ( got_cats )  
		     {
		     show_phase( &phase, "Convert ascii dlg file to binary dlg file.\n") ;
		     run_a_b_dlg( command, name, file1, file2) ;
		     }
	} /* end level 1 */


/**************************  phase:  dlg.to.digit  **************************/
/*  file2 - binary dlg,   file1 - binary vector,   file3 - att file    */

	if (level < 3 )
	{       
		/*  starting with binary dlg file  */
		if (level == 2 )
		{
			ask_for_name( file2, " DLG FILENAME ", name, B_DLG, "binary dlg") ;
			force_areas = ask_force() ;
			snap = ask_snap()  ;
		}

		show_phase( &phase, "Convert binary dlg file to binary vector file.\n") ;
		run_dlg_to_digit( command, name, file2, file1, file3, force_areas) ;
	} 



/**************************  phase:  a.b.vect  **************************/
/*  file2 - ascii dig,   file1 - binary vector               */

	if (level == 3 )
	{
		ask_for_name( file2, " VECTOR FILENAME ", name, A_DIG, "ascii vector") ;
		snap = ask_snap()  ;
	        G__make_mapset_element(ATT) ;
	        if (NULL == G_find_file (ATT, name, current_mapset ))
		   {    /* get file_name.att file from dig_ascii */
	           sprintf( command, "cp dig_ascii/%s.att dig_att/%s",
				 name, name);

	           if (system( command) )
	              {
		      fprintf(stderr, "ERROR(%s):  Could not copy ascii att file: '%s' to dig_att file: '%s'\n", PROG,  name, name) ;
		      exit(-1) ;
	              }
                   }

		show_phase( &phase, "Convert ascii vector file to binary vector file.\n") ;
		run_a_b_vect( command, name, file2, file1) ;
	} /* end level <= 3 */



/**********************  final phase:  import.binary  ***********************/
/*    file1 - binary vector    */


	if (level == 4)
	{
		ask_for_name( file2, " VECTOR FILENAME ", name, B_DIG, "binary vector") ;
		snap = ask_snap()  ;
	} /* end level 4 */


/**********************  phase:  import.gef  ****************************/
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

	if (level == 5)
	{

                if (!ask_if_join()) exit(0);

                format = ask_for_flavor();

		snap = ask_snap()  ;

		ask_for_name( file1, " SCSGEF FILENAME ",
                                           name, GEF, "ascii gef") ;
                                        /* Save gef file name */
                strcpy(gef_name,name);

                mapset = G_ask_any (" VECTOR FILENAME ",
                                           name, A_DIG, "ascii vector",1);
	        if ( ! mapset)
		        exit(0) ;
	                  /* ask user if map is in same units as mapset **
	        {
	           int proj;
	           char buf[1024];
	           if (NULL == G_find_file (B_DIG, name, current_mapset ))
	              {
	              proj = G_projection ();
	              if (proj == 0)
	                {
		        if (!G_yes ( "Mapset units are undefined. Continue? ", 1))
		        exit (-1);
	                }
	              else
	                {
		        printf ( "\n\nCurrent mapset is %s.\n",
					      G_database_projection_name ());
		        sprintf (buf, "  Is this map in %s %s? ", 
		              G_database_projection_name (), 
		              G_database_unit_name ());
		        if (!G_yes (buf, 1))
		          {
		          printf ( "Sorry, GRASS does not currently support mixing map units\n");
		          exit (-1);
		          }
		        else
		          printf ( "Thank You\n\n");
	                }
	              }
		} */

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
		strcpy(name,dig_name);
	} /* end level 5 */

/**********************  phase:  import.arc  ****************************/
/*
*  This program is a user interface to the ARC/INFO import routine
*/

	if (level == 6)
	{
                          /* get name for output grass vector file from user 
                mapset = G_ask_any (" VECTOR OUTPUT FILENAME ",
                                           name, B_DIG, "binary vector",1);
                strcpy(arc_name,name);
	        if ( ! mapset)
		        exit(0) ;
		snap = ask_snap()  ;*/

                show_phase( &phase, "Process ascii arc files.\n") ;
		strcpy(arc_name,name);

			 /* make sure command is empty */
                command[0] = '\0';

		run_imp_arc( command);
		goto at_end;

	}  /* end level 6 */

/**********************  phase:  import.dxf  ****************************/
/*
*  This program is a user interface to the DXF import routine
*/
	if (level == 7)
	{
                          /* get name for dxf file from user */
		ask_for_name( file1, " DXF FILENAME", name, DXF, "dxf ascii") ;

                          /* get name for output grass vector file from user */
                mapset = G_ask_any (" VECTOR OUTPUT FILENAME ",
                                           file2, B_DIG, "binary vector",1);
                show_phase( &phase, "Process ascii DXF files.\n") ;
		run_imp_dxf( command, file1, file2);

		ask_for_name( file2, " ASCII FILENAME ", name, A_DIG, "ascii vector") ;
	        if ( ! mapset)
		        exit(0) ;
		snap = ask_snap()  ;  
		show_phase( &phase, "Convert ascii vector file to binary vector file.\n") ;
		run_a_b_vect( command, name, file2, file1) ;

	}  /* end level 7 */

/**********************  phase:  import.tiger  ****************************/
/*
*  This program is a user interface to the tiger import routine
*/
	if (level == 8)
	{
                          /* get name for output grass vector file from user */
                mapset = G_ask_any (" VECTOR FILENAME ",
                                           name, B_DIG, "binary vector",1);
	        if ( ! mapset)
		        exit(0) ;
		snap = ask_snap()  ;  

                strcpy(tig_name,name);
                show_phase( &phase, "Process ascii TIGER files.\n") ;
			 /* make sure command is empty */
                command[0] = '\0';
                strcpy(command,name);

		run_imp_tig( command);
		strcpy(name,tig_name);

	}  /* end level 8 */


/**********************  phase:  build.vect  ****************************/

	show_phase( &phase, "Create vector support files.\n") ;
	run_build_vect( command, name, snap, ram) ;
at_end: ;
}		/*  main()  */

ask_which_level()
{

	int		num ;
	char	buf[80] ;

	printf("\n     Imports to GRASS Vector Format and Creates Needed Support Files\n\n") ;
	printf("     1  -   Ascii DLG file  to GRASS Vector Format\n") ;
	printf("     2  -   Binary DLG file to GRASS Vector Format\n") ;
	printf("     3  -   Ascii VECTOR file to GRASS Vector Format\n") ;
	printf("     4  -   Binary VECTOR file to GRASS Vector Format\n") ;
	printf("     5  -   Ascii SCS-GEF file to GRASS Vector Format\n") ;
	printf("     6  -   Ascii ARC/INFO file to GRASS Vector Format\n") ;
	printf("     7  -   Ascii DXF file to GRASS Vector Format\n") ;
	printf("     8  -   Ascii TIGER file to GRASS Vector Format\n") ;
	printf("\n\n Enter a number <1-8>\n anything else to quit: ") ;

	if (gets (buf) == NULL)
		clearerr (stdin), exit (1) ;

	num = atoi(buf) ;
	if( num < 1  ||  num > 8)
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

	if ( ! mapset )
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

        if (strcmp(header," ATTRIBUTE FILENAME") == 0 ||
            strcmp(header," DXF FILENAME") == 0 ) return(0);

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
		printf ( "\n\nCurrent mapset is %s.\n",
					      G_database_projection_name ());
		sprintf (buf, "  Is this map in %s %s? ", 
		      G_database_projection_name (), 
		      G_database_unit_name ());
		if (!G_yes (buf, 1))
		{
		    printf ( "Sorry, GRASS does not currently support mixing map units\n");
		    exit (-1);
		}
		else
		    printf ( "Thank You\n");
		printf ( "\n");
	    }
	}
	}

	return (0);


}	/**  ask_for_name()  **/


ask_force()
{
printf("\n During the DLG-3 file conversion to Vector:\n") ;
printf(" Determine if this map is composed of Area or Line information.\n") ;
return( G_yes( " Do you want to give precedence to Areas (opposed to Lines) ", 1)) ;

}

ask_snap()
{
printf("\n\n During the building of the Vector format:\n") ;
return( G_yes(" Do you want to snap nodes to other nodes within a threshold ", 0) ) ;
}

ask_for_cats()
{
printf("\n\n") ;
return( G_yes(" Does the DLG-3 file contain GRASS category codes ",1) );
}

ask_if_info()
{
printf("\n\n") ;
return( G_yes(" Is this DLG-3 file from an ARC/INFO GIS system ",0) );
}

ask_if_univ()
{
printf("\n\n") ;
return( G_yes(" Does this DLG-3 file contain a Universe Polygon ",0) );
}

ask_if_join()
{
printf("\n\n") ;
return( G_yes(" Have you joined all 3 GEF files into one file ? ",1) );
}

ask_for_flavor()
{
printf("\n\n") ;
return( G_yes(" Is the SCSGEF data in the new (40 char.) format ? ",1) );
}

/** ----- functions to run the commands ------ **/


run_a_b_dlg( command, name, ascii_dlg, binary_dlg)
	char	*command, *name, *ascii_dlg, *binary_dlg ;
{

	G__file_name( binary_dlg, B_DLG, name, current_mapset) ;
	G__make_mapset_element(B_DLG) ;

/*  execute the a.b.dlg program  */
	sprintf( command, "%s/etc/v.a.b.dlg %s %s ", gbase, ascii_dlg, binary_dlg) ;

	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert ascii dlg file: '%s' to binary dlg file: '%s'\n", PROG,  ascii_dlg, binary_dlg) ;
		exit(-1) ;
	}

}  

run_a_b_SCS( command, name, ascii_dlg, binary_dlg, catag_file,univ)
	char	*command, *name, *ascii_dlg, *binary_dlg, *catag_file ;
	int univ;
{

	G__file_name( binary_dlg, B_DLG, name, current_mapset) ;
	G__make_mapset_element(B_DLG) ;

/*  execute the special SCS a.b.dlg program  */
	if (univ)
	   sprintf( command, "%s/bin/contrib/cmd/%s -uf dlg=%s bdlg=%s att=%s", 
		gbase, V_PROG1, ascii_dlg, binary_dlg, catag_file) ;
	else
	   sprintf( command, "%s/bin/contrib/cmd/%s -f dlg=%s bdlg=%s att=%s", 
		gbase, V_PROG1, ascii_dlg, binary_dlg, catag_file) ;

/*      fprintf(stderr,"%s\n",command);  */
        if (system( command) )
	{
		fprintf(stderr, "ERROR(v.import - dlg):  Could not convert ascii dlg file: '%s' to binary dlg file: '%s'\n", ascii_dlg, binary_dlg) ;
		exit(-1) ;
	}  
}

run_dlg_cat( command, catname, attname, asc_dlg, doinfo)
	char	*command, *catname, *attname, *asc_dlg;
	int doinfo;
{

/*  execute the SCS modify attribute file program  */
        if (doinfo)
	    sprintf ( command, "%s/etc/dlgcat -a dlg=%s att=%s subj=%s",
					  gbase, asc_dlg, attname, catname) ;
        else
	    sprintf ( command, "%s/etc/dlgcat dlg=%s att=%s subj=%s",
					  gbase, asc_dlg, attname, catname) ;

/*      fprintf(stderr,"%s\n",command); */
        if (system( command) )
	{ 
		fprintf(stderr, "ERROR(%s):  Could not modify ascii attribute file: '%s' to category file: '%s2'\n", PROG,  attname, attname) ;
		exit(-1) ;
	}

}  


run_imp_gef( command, name, ascii_gef, ascii_dig, catag_file, fmt)
	char	*command, *name, *ascii_gef, *ascii_dig, *catag_file ;
        int   fmt;
{

	G__make_mapset_element(A_DIG) ;

/*  execute the SCS imp_gef program  */
        if (fmt)       /* new format */
	      sprintf( command, "%s/bin/%s gef=%s cat=%s output=%s ", 
                        gbase, V_PROG2, ascii_gef, catag_file, ascii_dig) ;
        else           /* old format */
	      sprintf( command, "%s/bin/%s -o gef=%s cat=%s output=%s ", 
                        gbase, V_PROG2, ascii_gef, catag_file, ascii_dig) ;

/*      fprintf(stderr,"%s\n",command); */
        if (system( command) ) 
	{
		fprintf(stderr, "ERROR(v.import - gef):  Could not convert ascii GEF file: '%s' to ascii dig file: '%s'\n", ascii_gef, ascii_dig) ;
		exit(-1) ;
	}
}

run_imp_arc( command)
	char	*command;
{

/*  execute the imp_arc program  */
	sprintf( command, "%s/bin/%s", gbase, V_PROG3) ;

/*      fprintf(stderr,"%s\n",command);*/
        if (system( command) ) 
	{
		fprintf(stderr, "ERROR(v.import - arc):  Could not convert ARC/INFO files to vector dig file\n") ;
		exit(-1) ;
	}
}

run_imp_dxf( command, in_name, out_name)
	char	*command, *in_name, *out_name;
{

/*  execute the imp_dxf program  */
	sprintf( command, "%s/bin/%s -a dxf=%s prefix=%s", 
			 gbase, V_PROG4, in_name, out_name) ;

/*      fprintf(stderr,"%s\n",command);*/
        if (system( command) ) 
	{
		fprintf(stderr, "ERROR(v.import - dxf):  Could not convert DXF files to vector dig file\n") ;
		exit(-1) ;
	}
/*   show user what is in dig_ascii directory now */
	fprintf(stderr, "\n The following ascii files for <%s> may now be\n",
				       out_name);
	fprintf(stderr, "    imported, as required :\n\n");

	sprintf( command, "cd $LOCATION/dig_ascii; ls -xp %s*; cd $LOCATION",
				       out_name);
        if (system( command) ) 
	{
		fprintf(stderr, "ERROR(v.import - dxf):  Could not show DXF files converted to ascii dig files\n") ;
		exit(-1) ;
	}
}

run_imp_tig( command)
	char	*command;
{
	char out_name[60];

	sprintf(out_name,"%s",command);

/*  execute the imp_tig program  */
	sprintf( command, "%s/bin/%s out=%s", 
			 gbase, V_PROG5, out_name) ;

/*      fprintf(stderr,"%s\n",command);*/
        if (system( command) ) 
	{
		fprintf(stderr, "ERROR(v.import - tiger):  Could not convert TIGER files to vector dig file\n") ;
		exit(-1) ;
	}
}

run_dlg_to_digit( command, name, binary_dlg, binary_dig, att, force_areas)
	char	*command, *name, *binary_dlg, *binary_dig, *att ;
	int	force_areas ;
{

/*	G__file_name( binary_dig, B_DIG, name, current_mapset) ;*/
	G__make_mapset_element(B_DIG) ;

	G__file_name( att, ATT, name, current_mapset) ;
	G__make_mapset_element(ATT) ;

/*  execute the program  */
	sprintf( command, "%s/etc/v.dlg.to.digit %s %s %s ", gbase, binary_dlg, name, att) ;

	/*  assumes lines making up unlabeled areas are Area edges
	*  unless specified, then their Lines
	*/

	if( !force_areas)
		strcat( command, "line") ;

        fprintf(stderr,"%s\n",command); 
	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert binary dlg file: '%s' to binary vector file: '%s'\n", PROG, binary_dlg, binary_dig) ;
		exit(-1) ;
	}

} 


run_a_b_vect( command, name, ascii_vector, binary_vector)
	char	*command, *name, *ascii_vector, *binary_vector ;
{

	G__file_name( binary_vector, B_DIG, name, current_mapset) ;
	G__make_mapset_element(B_DIG) ;


/*  execute the program  */
	sprintf( command, "v.in.ascii in=%s out=%s ", name, name) ;

/*      fprintf(stderr,"%s\n",command);  */
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

       /*call to v.build: flags mean:
       *     -s    = snap nodes, use default thresh setting (NO PROMPT)
       *     ""    = don't snap nodes
       */
       sprintf( command, "%s/etc/v.build  map=%s  %s", 
		   G_gisbase(), name, snap ? "-s": "" ) ;
					 

	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not build vector file: '%s'\n"
			, PROG, name) ;
		exit(-1) ;
	}


return(0) ;


} 
