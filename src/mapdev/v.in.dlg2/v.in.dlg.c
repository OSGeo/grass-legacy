#include        <stdio.h>
#include        <signal.h>
#include        "gis.h"

/*
v.in.dlg:

usage v.in.dlg -bl in=input file out=outfile

             -b   "     "   = binary dlg (default is ascii dlg)
	     -l  force lines instead of areas (areas is default)

/**  data directories   **/
#define         B_DIG           "dig"
#define         A_DIG           "dig_ascii"
#define         B_DLG           "bdlg"
#define         A_DLG           "dlg"
#define         ATT             "dig_att"
#define         PLUS            "dig_plus"

static char  *current_mapset ;
static char  *gbase ;

static char  *PROG ;

main (argc, argv)
        int  argc ;
        char  *argv[] ;
{
    char *a_dlg_name;
    char *b_dlg_name;
    char *dig_name;
    char *mapset;
    char a_dlgfile[200];
    char b_dlgfile[200];
    char digfile[200];
	struct GModule *module;
    struct Option *old, *new;
    /*struct Flag *a_flag;*/
    struct Flag *b_flag, *l_flag;
    int level, force_areas;
    char name[128];
    char command[256];

    G_gisinit (argv[0]);
    setbuf (stdout, NULL);

	module = G_define_module();
	module->description =
		"Converts an ASCII or binary USGS DLG-3 "
		"(bdlg) file to a binary GRASS vector (dig) file.";

    /*
    a_flag = G_define_flag();
    a_flag->key             = 'a';
    a_flag->description     = "convert Ascii DLG file to Vector file (default)";
    */
 
    b_flag = G_define_flag();
    b_flag->key             = 'b';
    b_flag->description     = "Input is Binary DLG file (default is Ascii)";

    l_flag = G_define_flag();
    l_flag->key             = 'l';
    l_flag->description     = "Give precedence to Line information (default is Area)";


    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
    /*
    old->gisprompt		= "old,dlg,dlg";
    */
    old->description		= "dlg input file";
    
    new = G_define_option();
    new->key			= "output";
    new->type			= TYPE_STRING;
    new->required		= YES;
    new->multiple		= NO;
    new->gisprompt		= "new,dig,vector";
    new->description		= "vector output file";


    /*initialize defaults*/
    force_areas = 1;
    level = 1;
    
    gbase = G_gisbase();
    current_mapset = G_mapset();

    if (G_parser (argc, argv))
	exit (-1);

    if (!*old->answer  || !*new->answer )
    {
        fprintf (stderr, "%s: Command line error: missing input or output name.\n\n", argv[0]);
	G_usage();
        exit (-1);
    }


    if (l_flag->answer)
	force_areas = 0;

    if (b_flag->answer)
    {
	level = 2;
	b_dlg_name = old->answer;
    }
    else /*if no b flag, default is ascii to dlg*/
	a_dlg_name = b_dlg_name = old->answer;

    dig_name = new->answer;

    if (!b_flag->answer) /*default: do ascii to bin dlg first*/
    {
	if ((mapset = G_find_file2 (A_DLG, a_dlg_name, "")) == NULL)
	{
	    fprintf (stderr, "Ascii DLG file <%s> not found.\n", a_dlg_name);
	    exit(-1);
	}
	G__file_name (a_dlgfile, A_DLG, a_dlg_name, mapset);
	
	run_a_b_dlg( command, a_dlg_name, a_dlgfile, b_dlgfile) ;
    }

    /*do bin dlg to vect */

    if (level == 2) /*have to get old bin_dlg file*/
    {
	if ((mapset = G_find_file2 (B_DLG, b_dlg_name, "")) == NULL)
	{
	    fprintf (stderr, "Binary DLG file <%s> not found.\n", b_dlg_name);
	    exit(-1);
	}
    }
    else
    {
	/*bin dlg just created, therefore must be in current mapset*/
	mapset = G_mapset();
    }

    G__file_name (b_dlgfile, B_DLG, b_dlg_name, mapset);

    run_dlg_to_digit( command, dig_name, b_dlgfile, force_areas) ;


/*DEBUG*/ exit(0);
    
}

#ifdef FOO
xx/**************************  phase:   a.b.dlg  **************************/
xx
xx	if (level == 1 )
xx	{
xx		ask_for_name( file1, " DLG FILENAME ", name, A_DLG, "ascii dlg") ;
xx		force_areas = ask_force() ;
xx		snap = ask_snap()  ;
xx		show_phase( &phase, "Convert ascii dlg file to binary dlg file.\n") ;
xx		run_a_b_dlg( command, name, file1, file2) ;
xx	}
xx
xx
xx/**************************  phase:  dlg.to.digit  **************************/
xx/*  file2 - binary dlg,   file1 - binary digit,   file3 - att file    */
xx
xx	if (level < 3 )
xx	{
xx
xx		/*  starting with binary dlg file  */
xx		if (level == 2 )
xx		{
xx			ask_for_name( file2, " DLG FILENAME ", name, B_DLG, "binary dlg") ;
xx			force_areas = ask_force() ;
xx			snap = ask_snap()  ;
xx		}
xx
xx		show_phase( &phase, "Convert binary dlg file to binary digit file.\n") ;
xx		run_dlg_to_digit( command, name, file2, file1, file3, force_areas) ;
xx	}
xx
xx
#endif /*FOO*/

show_phase( phase, describe)
	int	*phase ;
	char	*describe ;
{
	++*phase ;
	fprintf(stderr, "\n\n   ----- PHASE %d ----->  %s\n\n", *phase, describe) ;

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



run_dlg_to_digit( command, digname, binary_dlg, force_areas)
	char	*command, *digname, *binary_dlg;
	int	force_areas ;
{

	char vectfile[200];
	char attfile[200];
	G__file_name( vectfile, B_DIG, digname, current_mapset) ;
	G__make_mapset_element(B_DIG) ;

	G__file_name( attfile, ATT, digname, current_mapset) ;
	G__make_mapset_element(ATT) ;

/*  execute the program  */

	sprintf( command, "%s/etc/v.dlg.to.digit %s %s %s ", gbase, binary_dlg, digname, attfile) ;
	/*  assumes lines making up unlabeled areas are Area edges
	*  unless specified, then their Lines
	*/

	if( !force_areas)
		strcat( command, "line") ;


	if (system( command) )
	{
		fprintf(stderr, "ERROR(%s):  Could not convert binary dlg file: '%s' to binary digit file: '%s'\n", PROG, binary_dlg, vectfile) ;
		exit(-1) ;
	}

} 

