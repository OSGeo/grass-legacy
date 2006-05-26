#include "global.h"

int parser(int argc, char*argv[])
{
    map_opt = G_define_standard_option(G_OPT_V_MAP);

    act_opt = G_define_option();
    act_opt->key         = "action";
    act_opt->type        = TYPE_STRING;
    act_opt->required    = YES;
    act_opt->multiple    = NO;
    act_opt->description = _("The edit action to take.");
    act_opt->options     = "add,delete,move,merge";

    typ_opt = G_define_standard_option(G_OPT_V_TYPE);
    typ_opt->required    = NO;
    typ_opt->description = _("Select type. Required for add action.");
    typ_opt->options     = "point,line,area";
    typ_opt->answer      = "point";
    
    cat_opt = G_define_standard_option(G_OPT_V_CATS);
    cat_opt->required    = NO;
    
    pnt_opt = G_define_option();
    pnt_opt->key         = "coords";
    pnt_opt->key_desc    = "x,y";
    pnt_opt->type        = TYPE_DOUBLE;
    pnt_opt->required    = NO;
    pnt_opt->multiple    = YES;
    pnt_opt->description = _("An x,y list of points. Required for add and move actions.");
    
    val_opt = G_define_option();
    val_opt->key         = "values";
    val_opt->type        = TYPE_STRING;
    val_opt->required    = NO;
    val_opt->multiple    = NO;
    val_opt->description = _("A comma-separated list of attr=val pairs.");

    fld_opt = G_define_standard_option(G_OPT_V_FIELD);

    n_flg = G_define_flag();
    n_flg->key = 'n';
    n_flg->description = _("Create a new map.");

    t_flg = G_define_flag();
    t_flg->key = 't';
    t_flg->description = _("Do not use topology.");

    d_flg = G_define_flag();
    d_flg->key = 'd';
    d_flg->description = _("No database updates");

    if(G_parser(argc, argv))
	return 0;

    /* check that the given arguments makes sense together*/
/** @todo check for incorrect extra parameters */

    if(strcmp(act_opt->answer, "add")==0) { /* add requires a points argument */
	action_mode = MODE_ADD;
	if(pnt_opt->answers == NULL) {
	    help(_("Required parameter <points> not set"));
	    return 0;
	};
    }
    if(strcmp(act_opt->answer, "del")==0) { /* del requires a cats */
	action_mode = MODE_DEL;
	if(cat_opt->answers == NULL) {
	    help(_("Required parameter <cats> not set"));
	    return 0;
	};
    }
    if(strcmp(act_opt->answer, "move")==0) { /* move requires points and cats arguments */
	action_mode = MODE_ADD;
	if((pnt_opt->answers == NULL)||(cat_opt->answers == NULL)) {
	    help(_("Both parameters <points> and <cats> are required."));
	    return 0;
	};
    }
    if(strcmp(act_opt->answer, "merge")==0) { /* merge requires a cats argument */
	action_mode = MODE_ADD;
	if(cat_opt->answers == NULL) {
	    help(_("Required parameter <cats> not set"));
	    return 0;
	};
    }
    return 1;
}

void help(const char *msg)
{
    G_message("\nERROR: %s\n\n", msg);
    G_usage();
}
