#include <string.h>
#include <unistd.h>
#include <stdlib.h>
#include    <stdio.h>
#include    <signal.h>
#include    "gis.h"

/**  data directories   **/
#define	B_DIG	"dig"
#define	A_DIG	"dig_ascii"
#define	B_DLG	"bdlg"
#define	A_DLG	"dlg"
#define	ATT	"dig_att"
#define	PLUS	"dig_plus"
#define BUILD_OPT  1        
#define EDIT_OPT 2       

static char  *current_mapset ;
static char  *gbase ;

static char  *PROG ;

int go_interactive(int, char *[], char *, char *);
int run_cats(char *);
int ask_which_level(char *);
int ask_for_name(char *, char *, char *, char *, char *);
int run_support_vect(char *, char *, int);

int 
main (int argc, char *argv[])
{

	struct GModule *module;
	struct Option *option, *map, *s_val;
	int opt;
	struct Flag *s_flag, *p_flag, *r_flag;
	/*    struct Flag *s_flag, *c_flag, *t_flag, *p_flag;*/
	int i;
	int Interactive = 0;

	char buf[BUFSIZ];

	/*  store filename and path  */

	G_gisinit(argv[0]) ;

	module = G_define_module();
	module->description =
		"Creates GRASS support files for "
		"(binary) GRASS vector data.";

	/*****************************COMMAND PARSER******************************/

	map = G_define_option();
	map->key                    = "map";
	map->type                   = TYPE_STRING;
	map->required               = YES;
	map->multiple               = NO;
	map->gisprompt              = "old,dig,vector";
	map->description            = "vector file name";

	option = G_define_option();
	option->key                    = "option";
	option->type                   = TYPE_STRING;
	option->options				   = "build,edit";
	option->required               = NO;
	option->multiple               = NO;
	option->description            = "Build topology info OR Edit categories";

	/*
    c_flag = G_define_flag ();
    c_flag->key = 'c';
    c_flag->description = "Edit Category information";

    t_flag = G_define_flag ();
    t_flag->key = 't';
    t_flag->description = "Build file Topology               (Default)";
*/

	s_flag = G_define_flag();
	s_flag->key = 's';
	s_flag->description = "Snap nodes                        (Valid only with Build option)";

	p_flag = G_define_flag();
	p_flag->key = 'p';
	p_flag->description = "Prompt user for threshold         (Valid only with Build option)";

#ifdef MEMORY_IO
	ram_flag = G_define_flag();
	ram_flag->key = 'r';
	ram_flag->description = "read data into memory";
#endif

	r_flag = G_define_flag();
	r_flag->key = 'r';
	r_flag->description = "Set map region from data          (Valid only with Build option)";

	s_val = G_define_option();
	s_val->key          = "threshold";
	s_val->type         = TYPE_DOUBLE;
	s_val->required     = NO;
	s_val->multiple     = NO;
	s_val->description  = "Snap Threshold value       (Valid only with Build option and -s)";

	Interactive = 0;
	if (argc < 2)
		Interactive = 1;
	else
	{
		if (G_parser (argc, argv))
			exit (-1);


		if ( !map->answer || !*(map->answer) )
		{
			G_usage();

			fprintf (stderr, "\n%s: Command line error: missing map name.\n\n", argv[0]);
			exit (-1);
		}
	}


	/* Get program name */
	/* PROG = argv[0] ; */
	for (i = strlen (argv[0])-1 ; i >= 0 && argv[0][i] != '/' ; i--)
		;
	if (i)
		PROG = &(argv[0][i+1]);
	else
		PROG = argv[0];

	/*****************************Command line  ******************************/

	if (Interactive)
		return go_interactive (argc, argv, PROG, NULL);

	if (option->answer != NULL && (strcmp (option->answer, "edit") == 0))
		opt = EDIT_OPT;
	else
		opt = BUILD_OPT;

	if (opt == EDIT_OPT)
	{
		if (p_flag->answer || s_flag->answer || s_val->answer)
		{     				/* ERROR both options specified */
			fprintf (stderr, "\n%s--COMMAND LINE ERROR: 'threshold', -p, and -s apply only to `Build Topology' option\n", PROG);
			G_usage();
			exit (-1);
		}

		return run_cats (map->answer);
	}
	if (s_val->answer)
		sprintf (buf, "%s/etc/v.build map=%s %s %s %s thresh=%s", G_gisbase (), 
		    map->answer, 
		    s_flag->answer ? "-s" : "", 
		    p_flag->answer ? "-p" : "",
		    r_flag->answer ? "-r" : "",
		    s_val->answer);
	else
		sprintf (buf, "%s/etc/v.build map=%s %s %s %s", G_gisbase (), map->answer, 
		    s_flag->answer ? "-s" : "",
		    p_flag->answer ? "-p" : "",
		    r_flag->answer ? "-r" : "");
	exit (system (buf));
}


/*****************************Interactive ********************************/
int 
go_interactive (int argc, char *argv[], char *PROG, char *fname)
{
	int   phase ;  /*  current level  */
	int   level ;  /* how many levels to convert to digit vector format  */
	int   ram ;
	char *p;

	char  name[128] ;
	char buf[BUFSIZ];

	/*  store filename and path  */
	char  file2[128] ;

	char  command[500] ;

	phase = 0 ;


	gbase = G_gisbase() ;
	current_mapset = G_mapset() ;

	if (fname)
		strcpy (fname, name);
	else
		ask_for_name(file2, " VECTOR (DIGIT) FILENAME ",name,B_DIG,"binary vector");


	while (1)
	{
		system("clear") ;
		level = ask_which_level(name) ;


		/* decide if we override the default for using the RAM file */
		ram = 0 ;	 /* default */
#ifdef CERL
		ram = 1;	/* if cerl turn it on */
#endif
		p = getenv ("GTUNE_VSUP_RAM");	/* if TUNE var is set */
		if (p != NULL)
			if (!strcmp ("ON", p))	/* on */
				ram = 1;
			else
				ram = 0;		/* else off */


		/*  setup the args on the command line for build.vect  */
		switch (level) {
		case 1:
			run_support_vect( command, name, ram) ;
			break;
		case 2:
			run_cats (name);
			break;
		default:
			return 0;
			break;
		}
		/**********************  build.vect  ***********************/
		sleep (2);
		fprintf (stdout,"hit RETURN to continue -->");
		fgets (buf,10,stdin);
	}

	/* NOTREACHED */

}	/*  main()  */

int 
run_cats (char *name)
{
	char  command[500] ;

	sprintf (command, "%s/etc/modcats -v %s", G_gisbase(), name);
	exit (system (command));
}


int 
ask_which_level (char *map)
{
	int	num ;
	char    buf[80] ;

	fprintf (stdout,"Selected file is [%s]\n\n", map);
	fprintf (stdout,"     1  -   Build topology information (Needed for digit)\n") ;
	fprintf (stdout,"     \n") ;
	fprintf (stdout,"     2  -   Edit the category file\n") ;
	fprintf (stdout,"     \n") ;
	fprintf (stdout,"\n\n Enter a number <1-2>\n anything else to quit: ") ;

	if (fgets (buf,80,stdin)==NULL)
		clearerr (stdin), exit (1) ;

	num = atoi(buf) ;
	if( num < 1  ||  num > 2)
		fprintf(stderr, "\n\n ...Leaving %s\n\n", PROG),   exit(1) ;

	return(num) ;
}


int 
ask_for_name (char *file_name, char *header, char *name, char *dir, char *file_desc)
{
	char    *mapset ;

	mapset = G_ask_in_mapset( header, name, dir, file_desc) ;
	if ( ! mapset)
		exit(0) ;

	G__file_name( file_name, dir, name, mapset) ;

	return (0);
}    /**  ask_for_name()  **/

/* run from go_interactive */
int 
run_support_vect (char *command, char *name, int ram)
{
	int snap;

	snap=  G_yes(" Do you want to snap nodes to other nodes within a threshold ", 0)  ;

	G__make_mapset_element(ATT) ;
	G__make_mapset_element(PLUS) ;

/* OLD
*  Usage:  build.vect  mapset  file_name snap=["yes", "no"] ram=["yes", "no"]
*    snap:    "no" to leave nodes as they are,
*	     "yes" to snap nodes
*    ram:     "no" to read/write strictly from file
*	     "yes" read everything into memory
*    thresh:  "no" use the default thresh value
*	     "yes" user wants to set own thresh value for snapping
*  NEW
*    if snap = yes, then set flags to -sp, which means:
*                snap nodes, prompt user for thresh value
*/


	sprintf (command, "%s/etc/v.build  map=%s %s", G_gisbase(), name, snap ? "-sp" : "");


	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not build vector file: '%s'\n"
		    , PROG, name) ;
		exit(-1) ;
	}


	return(0) ;
}

