/* updated Oct 1999, Markus Neteler GRASS 5*/
/* @(#)export_vect.c	1.1   03/91 GRASS 4.0 */
/*
*  This program is a top-level menu that runs MAPDEV programs
*  trying to create the exportable files from the 'digit' database.
*
*  Programs used by this program:
*      (/usr/gis/etc)  b.a.vect, Vexport_dlg 
*      
*  Data files used by this program:
*      binary vector,  dig_att
*  
*  Note: all the programs that this calls must exit with a zero value on 
*  successful completion and a true value on error.
*/


#include <stdio.h>
#include <signal.h>
#include <string.h>
#include <stdlib.h>
#include "gis.h"
int ask_which_level(void);
int show_phase(int *, char *);
int ask_for_name(char *, char *, char *, char *, char *);
int run_dig_to_dlg(char *, char *, char *, char *);
int run_dig_to_gef(char *, char *, char *, char *);
int run_dig_to_arc(char *);
int run_dig_to_dxf(char *);
int run_b_a_vect(char *, char *, char *, char *);

/**  data directories   **/
#define		B_DIG		"dig"
#define		A_DIG		"dig_ascii"
#define		A_DLG		"dlg"
#define		A_GEF		"gef"
#define		A_ARC		"arc"
#define		A_DXF		"dxf"
#define		ATT		"dig_att"
#define V_PROG1  "v.out.dlg.scs"   /* non-interactive version */
#define V_PROG2  "v.out.scsgef"   /* non-interactive version */
#define V_PROG3  "v.out.arc"   /* non-interactive version */
#define V_PROG4  "v.out.ascii"      /* non-interactive version */
#define V_PROG5  "v.out.dxf"      /* non-interactive version */

static char  *current_mapset ;
static char  *gbase ;
static char  *location ;
static char  cov_type[15] ;
static char  answer[100];

static char  *PROG ;

int 
main (int argc, char *argv[])
{
	struct GModule *module;

	int   done;
	int   phase ;  /*  current level  */
	int   level ;  /* how many levels to convert to vector format  */

	char  name[256] ;
	char  name2[256] ;

	/*  store filename and path  */
	char  file1[128] ;
	char  file2[128] ;
	char  file3[128] ;

	char	*mapset ;
	char  command[256] ;

	G_gisinit("EXPORT_FROM_DIGIT") ;

	module = G_define_module();
	module->description =
		"Converts binary vector files into formatted ASCII files "
		"for transfer to other computer systems.";

        if (argc > 1 && !strcmp (argv[1], "help"))
           {
           fprintf(stderr,"\n\n");
           fprintf(stderr,"v.export is an interactive program which runs other vector\n");
           fprintf(stderr,"   export programs.  It allows you to export DLG, GEF, ARC,\n");
           fprintf (stderr,"  DXF, or ASCII vector files from GRASS vector format. \n");
	   fprintf (stderr, "\nThere are NO arguments required.\n\n");
           exit (1);
           }

	system("clear") ;
	setbuf(stdout, NULL) ;

	phase = 0 ;
	PROG = argv[0] ;
	gbase = G_gisbase() ;
        location = G_location() ;
	current_mapset = G_mapset() ;

	level = ask_which_level() ;


/**************************  phase:  digit.to.dlg  **************************/
/*  file1 - binary digit,   file2 - ascii dlg  */

	if (level == 1 )
	{

                ask_for_name( name, " VECTOR FILENAME ", file1, B_DIG, "binary vector") ;

	        mapset = G_ask_any( " FILENAME TO STORE ASCII DLG FILE", name,
				A_DLG, "ascii dlg", 1) ;
	        if ( ! mapset)
		        exit(0) ;

		show_phase( &phase, "  ") ;
  		run_dig_to_dlg( command, name, file1, file2) ;

                sprintf(file2,"%s",name);
	}



/**************************  phase:  b.a.vect  **************************/
/*  file2 - ascii dig,   file1 - binary vector               */

	if (level == 2 )
	{
		ask_for_name( file1, " VECTOR FILENAME ", name, B_DIG, "binary vector") ;
		show_phase( &phase, "Convert binary vector file to ascii vector file.\n") ;
		run_b_a_vect( command, name, file2, file1) ;
	}

/**************************  phase:  digit.to.gef  **************************/
/*  file1 - binary digit,   file2 - ascii gef  */

	if (level == 3 )
	{

                ask_for_name( name, " VECTOR FILENAME ", file1, B_DIG, "binary vector") ;

	        mapset = G_ask_any( " FILENAME TO STORE ASCII SCS_GEF FILE", 
			    name, A_GEF, "ascii gef",1) ;
	        if ( ! mapset)
		        exit(0) ;

		show_phase( &phase, "  ") ;
  		run_dig_to_gef( command, name, file1, file2) ;

	}

/**************************  phase:  digit.to.arc  **************************/
/*  file1 - binary digit,   file2 - ascii arc  */

	if (level == 4 )
	{
/*************
                do 
		{
                fprintf (stdout,"\n COVERAGE TYPE\n");
                fprintf (stdout,"Enter \"polygon\" or \"line\"\n");
                fprintf (stdout,"Hit RETURN to cancel request\n");
                fprintf (stdout,"> ");
                if (fgets (cov_type,80,stdin) == NULL)
                        clearerr (stdin), exit (1) ;

                           ** EXIT IF USER HIT RETURN **
                if (strcmp(cov_type,"") == 0)
                  exit(0);
                }
                while (strcmp(cov_type,"polygon")!=0 && 
		      strcmp(cov_type,"line")!=0);

		if (strcmp(cov_type,"polygon") == 0)
		      strcpy(cov_type,"area");

                ask_for_name( name, " VECTOR FILENAME ", 
				    file1, B_DIG, "binary vector") ;

		done = 0;
                do {
	        mapset = G_ask_any( " FILENAME TO STORE ASCII ARC/INFO FILE",
			       name, A_ARC, "ascii arc", 1) ;
	        if ( ! mapset)
		        exit(0) ;
                sprintf(name2,"%s.lab",name);
	        if (G_find_file( A_ARC, name2, mapset) != NULL) ;
                   {
		   sprintf(answer,
			"\n** %s exists. ok to overwrite? ", name);
		   if (!G_yes (answer,0))
			  done = 0;
                   else done = 1;
                   }
                }
		while (!done); 
*/

		show_phase( &phase, "  ") ;
/*		run_dig_to_arc( command, name, file1, file2) ;*/
		command[0] = '\0';
  		run_dig_to_arc( command) ;

	}


/**************************  phase:  digit.to.dxf  **************************/
/*  file1 - binary digit,   file2 - ascii dxf  */

	if (level == 5 )
	{
		show_phase( &phase, "  ") ;
		command[0] = '\0';
  		run_dig_to_dxf( command) ;

	}

/**********************  future phases:  ?????  ***********************/
/*    file1 - binary digit    */

	exit(0);
}		/*  main()  */


int 
ask_which_level (void)
{

	int		num ;
	char	buf[80] ;

	fprintf (stdout,"\n     Exports from GRASS Vector Format \n\n") ;
	fprintf (stdout,"     1  -   Ascii DLG file from GRASS Vector Format\n") ;
	fprintf (stdout,"     2  -   Ascii VECTOR file from GRASS Vector Format\n") ;
	fprintf (stdout,"     3  -   Ascii SCS_GEF file from GRASS Vector Format\n") ;
	fprintf (stdout,"     4  -   Ascii ARC/INFO file from GRASS Vector Format\n") ;
	fprintf (stdout,"     5  -   Ascii DXF file from GRASS Vector Format\n") ;
	fprintf (stdout,"\n\n Enter a number <1-5>\n anything else to quit: ") ;

        if (fgets (buf,80,stdin) == NULL)
                clearerr (stdin), exit (1) ;
                                        
	num = atoi(buf) ;
	if( num < 1  ||  num > 5)
		fprintf(stderr, "\n\n ...Leaving %s\n\n", PROG),   exit(1) ;

	return(num) ;

}


int 
show_phase (int *phase, char *describe)
{

	++*phase ;
	fprintf(stderr, "\n\n   ----- PHASE %d ----->  %s\n\n", *phase, describe) ;
	return 0;
}

int 
ask_for_name (char *file_name, char *header, char *name, char *dir, char *file_desc)
{

	char	*mapset ;

	mapset = G_ask_old( header, name, dir, file_desc) ;

	if ( ! mapset )
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

	return (0);

}	/**  ask_for_name()  **/


/** ----- functions to run the commands ------ **/


int 
run_dig_to_dlg (char *command, char *name, char *binary_dig, char *ascii_dlg)
{
        G__make_mapset_element(A_DLG);
        G__file_name( ascii_dlg, A_DLG, name, current_mapset); 

/*  execute the program  */
	sprintf (command, "%s/bin/%s input=%s output=%s", 
                     G_gisbase (), V_PROG1, binary_dig, name);

/*fprintf(stderr,"%s\n",command); */
    	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert binary dig file: '%s' to ascii dlg file: '%s'\n", V_PROG1, binary_dig, ascii_dlg) ;
		exit(-1) ;
	}
	return 0;
}

int 
run_dig_to_gef (char *command, char *name, char *binary_dig, char *ascii_gef)
{
        G__make_mapset_element(A_GEF);
        G__file_name( ascii_gef, A_GEF, name, current_mapset); 

/*  execute the program  */
	sprintf (command, "%s/bin/%s input=%s output=%s", 
                               G_gisbase (), V_PROG2, binary_dig, name);

/*fprintf(stderr,"%s\n",command);  */
    	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert binary dig file: '%s' to ascii gef file: '%s'\n", V_PROG2, binary_dig, name) ;
		exit(-1) ;
	}
	return 0;
}

/*run_dig_to_arc( command, name, binary_dig, ascii_arc)
	char	*command, *name, *ascii_arc, *binary_dig;*/
int 
run_dig_to_arc (char *command)
{

/*      G__make_mapset_element(A_ARC);
        G__file_name( ascii_arc, A_ARC, name, current_mapset); */

/*  execute the program  */
	sprintf (command, "%s/bin/%s", G_gisbase (), V_PROG3);

/*fprintf(stderr,"%s\n",command);*/
    	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert binary dig file to ascii arc file\n", V_PROG3) ;
		exit(-1) ;
	}
	return 0;
}

/*run_dig_to_dxf( command, name, binary_dig, ascii_arc)
	char	*command, *name, *ascii_arc, *binary_dig;*/
int 
run_dig_to_dxf (char *command)
{

/*      G__make_mapset_element(A_DXF);
        G__file_name( ascii_arc, A_DXF, name, current_mapset); */

/*  execute the program  */
	sprintf (command, "%s/bin/%s", G_gisbase (), V_PROG5);

/*fprintf(stderr,"%s\n",command);*/
    	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert binary dig file to ascii dxf file\n", V_PROG5) ;
		exit(-1) ;
	}
	return 0;
}

int 
run_b_a_vect (char *command, char *name, char *ascii_vector, char *binary_vector)
{

	G__make_mapset_element(A_DIG) ;


/*  execute the program  */
	sprintf( command, "%s/bin/%s input=%s output=%s  ",
		     gbase, V_PROG4, name, name) ;

/*fprintf(stderr,"%s\n",command);*/
  	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert binary vector file: '%s' to ascii vector file: '%s'\n", V_PROG4, binary_vector, ascii_vector) ;
		exit(-1) ;
	}

/* Move the ascii attribute file, too */
	sprintf( command, "cp %s/%s/dig_att/%s %s/%s/dig_ascii/%s.att ",
                 G_location_path(), current_mapset, name, 
                 G_location_path(), current_mapset, name) ;

/*fprintf(stderr,"%s\n",command);  */
    	if (system( command) )
	{
		fprintf(stderr, "ERROR(cp):  Could not create attribute file: '%s.2' for vector file: '%s'\n", ascii_vector, binary_vector) ;
		exit(-1) ;
	}

	return 0;
} 
