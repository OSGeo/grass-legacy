#include        <stdio.h>
#include        <signal.h>
#include        "gis.h"
#include        "doit.h"

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
#define         OFFSET_ATT      "1"

static char  *current_mapset ;
static char  *gbase ;

static char  *PROG ;

int 
main (int argc, char *argv[])
{
    char *a_dlg_name;
    char *b_dlg_name;
    char *dig_name;
    char *mapset;
    char *add_att=NULL;
	struct GModule *module;
    struct Option *old, *new, *add_attr, *offset;
    struct Flag *l_flag;
    int force_lines;
    int offset_att;
    char name[128];
    char command[256];

    G_gisinit (argv[0]);

	module = G_define_module();
	module->description =
		"Converts an ASCII USGS DLG-3 Optional "
		"file to a binary GRASS vector (dig) file.";

    l_flag = G_define_flag();
    l_flag->key             = 'l';
    l_flag->description     = "Give precedence to line information (default is area)";


    old = G_define_option();
    old->key			= "input";
    old->type			= TYPE_STRING;
    old->required		= YES;
    old->multiple		= NO;
    old->gisprompt              = "old,dlg,dlg";
    old->description		= "dlg input file";
    
    new = G_define_option();
    new->key			= "output";
    new->type			= TYPE_STRING;
    new->required		= YES;
    new->multiple		= NO;
    new->gisprompt		= "new,dig,vector";
    new->description		= "vector output file";

    add_attr = G_define_option();
    add_attr->key			= "matt";
    add_attr->type			= TYPE_STRING;
    add_attr->required		= NO;
    add_attr->multiple		= NO;
    add_attr->description		= "multiple attributes output file";

    offset = G_define_option();
    offset->key			= "base";
    offset->type		= TYPE_INTEGER;
    offset->required		= NO;
    offset->multiple		= NO;
    offset->answer		= OFFSET_ATT;
    offset->description		= "Base for matt file";

    /*initialize defaults*/
    force_lines = 0;
    
    gbase = G_gisbase();
    current_mapset = G_mapset();

    if (G_parser (argc, argv))
	exit (-1);


    if (l_flag->answer)
	force_lines = 1;

    a_dlg_name = old->answer;
    dig_name   = new->answer;
    add_att = add_attr->answer;

    sscanf (offset->answer, "%d", &offset_att);

    doit (dig_name, a_dlg_name, add_att, offset_att, force_lines);

    exit(0);
}
