#include        <stdio.h>
#include        <signal.h>
#include        "gis.h"

/*
v.in.dlg:

usage v.in.dlg -l in=input file out=outfile

	     -l  force lines instead of areas (areas is default)
*/

/**  data directories   **/
#define         B_DIG           "dig"
#define         A_DIG           "dig_ascii"
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
    struct Option *old, *new;
    struct Flag *l_flag;
    int force_areas;
    char name[128];
    char command[256];

    G_gisinit (argv[0]);

    l_flag = G_define_flag();
    l_flag->key             = 'l';
    l_flag->description     = "Give precedence to Line information (default is Area)";


    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
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
    
    gbase = G_gisbase();
    current_mapset = G_mapset();

    if (G_parser (argc, argv))
	exit (-1);


    if (l_flag->answer)
	force_areas = 0;

    a_dlg_name = old->answer;
    dig_name   = new->answer;

    doit (dig_name, a_dlg_name, force_areas);

    exit(0);
}
