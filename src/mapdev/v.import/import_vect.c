
/*
*  This program is a top-level menu that runs a multitude of MAPDEV programs
*  trying to create the files necessary to for the 'digit' program.
*
*  Programs used by this program:
*      (/usr/gis/etc)  v.in.dlg  a.b.vect, build.vect
*      
*      dlg.to.dig also forces all unlabeled Area lines to Line lines.
*
*  Data files used by this program:
*      ascii dlg,  ascii vector,  binary vector,  dig_att
*  
*  Note: all the programs that this calls must exit with a zero value on 
*  successful completion and a true value on error.
*/


#include <string.h>
#include <stdlib.h>
#include	<stdio.h>
#include	<signal.h>
#include	"gis.h"

/**  data directories   **/
#define		B_DIG		"dig"
#define		A_DIG		"dig_ascii"
#define		A_DLG		"dlg"
#define		ATT			"dig_att"
#define		PLUS		"dig_plus"

static char  *current_mapset ;
static char  *gbase ;

static char  *PROG ;
int ask_which_level(void);
int show_phase(int *, char *);
int ask_for_name(char *, char *, char *, char *, char *);
int ask_force(void);
int ask_mult_att_base(char *, int *);
int ask_snap(void);
int run_dlg_to_digit(char *, char *, char *, char *, char *, int, char *, int);
int run_a_b_vect(char *, char *, char *, char *);
int run_build_vect(char *, char *, int, int);

int main (int argc, char *argv[])
{

	int   phase ;  /*  current level  */
	int   level ;  /* how many levels to convert to digit vector format  */
	int   force_areas ;

	int   snap = 0;
	int   ram ;
        int   base = 0;

	char  name[128] ;

	/*  store filename and path  */
	char  file1[128] ;
	char  file2[128] ;
	char  file3[128] ;
	char  matt_file[128] ;
	char *p;

	char  command[256] ;

	G_gisinit(argv[0]) ;


        if (argc > 1 && !strcmp (argv[1], "help"))
        {
fprintf(stderr,"\n\n");
fprintf(stderr,"v.import is an interactive program which runs other vector\n");
fprintf(stderr,"   import programs.  It allows you to import DLG or ASCII vector\n");
fprintf (stderr,"  files into GRASS vector format.  It also runs v.support.\n");
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





/**************************  phase:  dlg.to.digit  **************************/
/*  file2 - binary digit,   file1 - ascii dlg,   file3 - att file    */
	if (level == 1 )
	{
		ask_for_name( file1, " DLG FILENAME ", name, A_DLG, "ascii dlg") ;
		force_areas = ask_force() ;
                if (ask_mult_att_base(matt_file,&base) == 0) matt_file[0]='\0';
		snap = ask_snap()  ;
		show_phase( &phase, "Convert binary dlg file to binary digit file.\n") ;
		run_dlg_to_digit( command, name, file1, file2, file3, force_areas, matt_file, base) ;
	}



/**************************  phase:  a.b.vect  **************************/
/*  file2 - ascii dig,   file1 - binary vector               */

	if (level == 2 )
	{
		ask_for_name( file2, " VECTOR (DIGIT) FILENAME ", name, A_DIG, "ascii vector") ;
		snap = ask_snap()  ;
		show_phase( &phase, "Convert ascii vector file to binary vector file.\n") ;
		run_a_b_vect( command, name, file2, file1) ;
	}



/**********************  final phase:  build.vect  ***********************/
/*    file1 - binary digit    */


	if (level == 3)
	{
		ask_for_name( file2, " VECTOR (DIGIT) FILENAME ", name, B_DIG, "binary vector") ;
		snap = ask_snap()  ;
	}

	/*if level == 1 or 2 or 3 */

	show_phase( &phase, "Create vector support files.\n") ;
	run_build_vect( command, name, snap, ram) ;


	return 0;
}		/*  main()  */


int ask_which_level (void)
{

	int		num ;
	char	buf[80] ;

	fprintf (stdout,"\n     Imports to GRASS Vector Format and Creates Needed Support Files\n\n") ;
	fprintf (stdout,"     1  -   Ascii DLG file  to GRASS Vector Format\n") ;
	fprintf (stdout,"     2  -   Ascii VECTOR file  to GRASS Vector Format\n") ;
	fprintf (stdout,"     3  -   Binary VECTOR file to GRASS Vector Format\n") ;
	fprintf (stdout,"\n\n Enter a number <1-3>\n anything else to quit: ") ;

	if (fgets (buf,80,stdin) == NULL)
		clearerr (stdin), exit (1) ;

	num = atoi(buf) ;
	if( num < 1  ||  num > 3)
		fprintf(stderr, "\n\n ...Leaving %s\n\n", PROG),   exit(1) ;

	return(num) ;

}


int show_phase (int *phase, char *describe)
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


int 
ask_force (void)
{
fprintf (stdout,"\n During the DLG-3 file conversion to Vector:\n") ;
fprintf (stdout," Determine if this map is composed of Area or Line information.\n") ;
return( G_yes( " Do you want to give precedence to Areas (opposed to Lines) ", 1)) ;

	return 0;
}


int 
ask_mult_att_base (char *matt_file, int *base)
{
 char answer[128];

fprintf (stdout,"\n During the DLG-3 file conversion to Vector:\n") ;
if( G_yes( " Do you want to create multiple attribute file? ", 1)) {
  fprintf(stderr,"Enter multiple attributes file name>");
  fgets(answer,128,stdin);
  sprintf(matt_file,"%s",answer);
  fprintf(stderr,"Enter base for multiple attributes (0)>");
  fgets(answer,128,stdin);
  if (strlen(answer) == 0) *base = 0;
  else *base = atoi(answer); 
  return 1;
}
else return 0;
}



int ask_snap (void)
{
fprintf (stdout,"\n\n During the building of the Vector format:\n") ;
return( G_yes(" Do you want to snap nodes to other nodes within a threshold ", 0) ) ;
}



/** ----- functions to run the commands ------ **/



int run_dlg_to_digit (char *command, char *name, char *ascii_dlg,
	char *binary_dig, char *att,
	int force_areas, char *matt_file, int base)
{
        char temp[20];

	G__file_name( binary_dig, B_DIG, name, current_mapset) ;
	G__make_mapset_element(B_DIG) ;

	G__file_name( att, ATT, name, current_mapset) ;
	G__make_mapset_element(ATT) ;

/*  execute the program  */
	sprintf( command, "v.in.dlg in=%s out=%s ", name, name) ;

	/*  assumes lines making up unlabeled areas are Area edges
	*  unless specified, then their Lines
	*/
	if( matt_file[0] != '\0') {
		sprintf( temp, "matt=%s base=%d ", matt_file, base) ;
		strcat( command, temp) ;
        }

	if( !force_areas)
		strcat( command, "-l") ;


	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert dlg file: '%s' to binary digit file: '%s'\n", PROG, ascii_dlg, binary_dig) ;
		exit(-1) ;
	}

	return 0;
} 


int run_a_b_vect (char *command, char *name, char *ascii_vector,
	char *binary_vector)
{

	G__file_name( binary_vector, B_DIG, name, current_mapset) ;
	G__make_mapset_element(B_DIG) ;


/*  execute the program  */
	/*
	sprintf( command, "v.in.ascii in='%s' out=%s", ascii_vector,
		binary_vector) ;
		*/
	sprintf( command, "v.in.ascii in='%s' out=%s", name, name) ;



	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert ascii vector file: '%s' to binary vector file: '%s'\n", PROG, ascii_vector, binary_vector) ;
		exit(-1) ;
	}

	return 0;
} 



int run_build_vect (char *command, char *name, int snap, int ram)
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
	/*
	sprintf( command, "v.build  map=%s  %s", name, snap ? "-s": "" ) ;
	*/
	sprintf( command, "v.support  map=%s  %s", name, snap ? "-s": "" ) ;


	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not build vector file: '%s'\n"
			, PROG, name) ;
		exit(-1) ;
	}

	return(0) ;
} 
