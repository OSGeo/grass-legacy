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
#define         OFFSET_ATT      "1"

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
	char *add_att=NULL;
	struct Option *old, *new, *add_attr, *offset;
	struct Option *attopt, *lattopt, *subjopt;
	struct Flag *l_flag, *u_flag, *o_flag;
	int force_lines;
	int discard_univ;
	int old_attr;
	int offset_att;
	char name[128];
	char command[1024];

	G_gisinit (argv[0]);

	l_flag = G_define_flag();
	l_flag->key             = 'l';
	l_flag->description     = "Give precedence to line information (default is area)";

	u_flag = G_define_flag();
	u_flag->key             = 'u';
	u_flag->description     = "Discard the first area (universe polygon)";

	o_flag = G_define_flag();
	o_flag->key             = '2';
	o_flag->description     = "Use old 2 column attribute format";


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

        attopt = G_define_option();
        attopt->key              = "att";
        attopt->type             =  TYPE_STRING;
        attopt->required         =  NO;
        attopt->gisprompt        = "old,dlg,AREA attribute";
        attopt->description      = "AREA attribute file name";
 
        lattopt = G_define_option();
        lattopt->key              = "latt";
        lattopt->type             =  TYPE_STRING;
        lattopt->required         =  NO;
        lattopt->gisprompt        = "old,dlg,LINE attribute";
        lattopt->description      = "LINE attribute file name";
 
        subjopt = G_define_option();
        subjopt->key              = "subj";
        subjopt->type             =  TYPE_STRING;
        subjopt->required         =  NO;
        subjopt->gisprompt        = "any,SUBJ,subject";
        subjopt->description      = "subject file name";
 
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
	discard_univ = 0;

	gbase = G_gisbase();
	current_mapset = G_mapset();

	if (G_parser (argc, argv))
		exit (-1);


	if (l_flag->answer)
		force_lines = 1;
	else
		force_lines = 0;

	if (u_flag->answer)
		discard_univ = 1;
	else
		discard_univ = 0;

	if (o_flag->answer)
		old_attr = 1;
	else
		old_attr = 0;

	a_dlg_name = old->answer;
	dig_name   = new->answer;
	add_att = add_attr->answer;

	sscanf (offset->answer, "%d", &offset_att);

	if ((attopt->answer == NULL) && (lattopt->answer == NULL)){
		doit (dig_name, a_dlg_name, add_att, offset_att, force_lines);
	}else{
		doit2 (dig_name, a_dlg_name, add_att, offset_att, force_lines,
			attopt->answer, lattopt->answer, subjopt->answer,
			discard_univ, old_attr);
	}
	if( subjopt->answer != NULL){
		sprintf(command, "cp %s/%s/SUBJ/%s %s/%s/dig_cats/%s\n", G_location_path(), G_mapset(), 
			subjopt->answer, G_location_path(), G_mapset(), dig_name);
		system(command);
	}
	exit(0);
}
