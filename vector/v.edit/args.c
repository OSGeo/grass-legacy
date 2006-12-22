#include "global.h"

int parser(int argc, char*argv[])
{
    map_opt = G_define_standard_option(G_OPT_V_MAP);

    tool_opt = G_define_option();
    tool_opt->key         = "tool";
    tool_opt->type        = TYPE_STRING;
    tool_opt->required    = YES;
    tool_opt->multiple    = NO;
    tool_opt->description = _("The edit tool to take.\n"
                             "\t\tcreate - Create new vector file\n"
                             "\t\tadd    - Add new vector feature to existing vector file\n"
                             "\t\tdelete - Delete feature from vector file\n"
                             "\t\tmove   - Move feature in vector file\n"
                             "\t\tvertex - Move just only vertex\n"
                             "\t\tstraight - Remove vertex\n"
                             "\t\tmerge  - Merge two vector lines togher\n"
                             "\t\tbreak  - Add new vertex to existing vector line\n"
                             "\t\tsplit  - Split line into two separate lines");
    tool_opt->options     = "create,add,delete,move,vertex,straight,merge,break,split";

    input_opt = G_define_option();
    input_opt->key      = "input";
    input_opt->type     = TYPE_STRING;
    input_opt->required = NO;
    input_opt->multiple = NO;
    input_opt->description = _("ASCII file to be converted to binary vector file, if not given reads from standard input");

    cat_opt = G_define_standard_option(G_OPT_V_CATS);
    cat_opt->required    = NO;
    
    coord_opt = G_define_option();
    coord_opt->key         = "coords";
    coord_opt->key_desc    = "x,y";
    coord_opt->type        = TYPE_DOUBLE;
    coord_opt->required    = NO;
    coord_opt->multiple    = YES;
    coord_opt->description = _("An x,y list of points. Required for add and move actions.");
    
    move_opt = G_define_option();
    move_opt->key         = "move";
    move_opt->key_desc    = "x,y";
    move_opt->type        = TYPE_DOUBLE;
    move_opt->required    = NO;
    move_opt->multiple    = NO;
    move_opt->description = _("Difference in x,y direction for moving feature or vertex");
    
    at_opt =  G_define_option();
    at_opt->key         = "at";
    at_opt->key_desc    = "x,y";
    at_opt->type        = TYPE_DOUBLE;
    at_opt->required    = NO;
    at_opt->multiple    = NO;
    at_opt->description = _("An x,y location of breaking feature");
    
    bbox_opt =  G_define_option();
    bbox_opt->key         = "bbox";
    bbox_opt->key_desc    = "x1,y1,x2,y2";
    bbox_opt->type        = TYPE_DOUBLE;
    bbox_opt->required    = NO;
    bbox_opt->multiple    = NO;
    bbox_opt->description = _("Bounding box of selected feature");

    snap_opt = G_define_option();
    snap_opt->key         = "snap";
    snap_opt->type        = TYPE_DOUBLE;
    snap_opt->required    = NO;
    snap_opt->multiple    = NO;
    snap_opt->description = _("Object points will snap to existing points within snap units.");
    snap_opt->answer      = "5.0";
    
 
    fld_opt = G_define_standard_option(G_OPT_V_FIELD);

    t_flg = G_define_flag();
    t_flg->key = 't';
    t_flg->description = _("Do not use topology.");

    d_flg = G_define_flag();
    d_flg->key = 'd';
    d_flg->description = _("No database updates.");

    b_flg = G_define_flag();
    b_flg->key = 'b';
    b_flg->description = _("Give cats to boundaries too.");

    c_flg = G_define_flag();
    c_flg->key = 'c';
    c_flg->description = _("Do not close boundaries");

    n_flg = G_define_flag();
    n_flg->key          = 'n';
    n_flg->description  = _("Don't expect a header");

    if(G_parser(argc, argv))
	return 0;

    /*
     * check that the given arguments makes sense together 
     */
    if ( input_opt->answer != NULL ) {
        if ( (ascii = fopen ( input_opt->answer, "r" ) ) == NULL )
        {
            G_warning(_("Could not open ascii file <%s>"), input_opt->answer);
            G_usage();
        }
    } else {
        ascii = stdin;
    }

    /* check for coordinate param */
    if (coord_opt->answers != NULL) {
        int i = 0;
        for (i = 0; coord_opt->answers[i]; i ++)
            ;
        if (i%2 != 0) {
            G_warning(_("Number of coordinates must be odd number"));
            G_usage();
	    return 0;
        }

    }

    /* check for bbox param */
    if (bbox_opt->answers != NULL) {
        int i = 0;
        for (i = 0; bbox_opt->answers[i]; i ++)
            ;
        if (i%2 != 0) {
            G_warning(_("Number of coordinates must be odd number"));
            G_usage();
	    return 0;
        }

    }
    
    /* check for move param */
    if (move_opt->answers != NULL) {
        int i = 0;
        for (i = 0; move_opt->answers[i]; i ++)
            ;
        if (i<1) {
            G_warning(_("Two coordinates are requered"));
            G_usage();
	    return 0;
        }

    }


    /* check that the given arguments makes sense together */
    /** @todo check for incorrect extra parameters */
    if(strcmp(tool_opt->answer, "create")==0) { /* create requires nothing extra*/
	action_mode = MODE_CREATE;
	return 1;
    }
    
    if(strcmp(tool_opt->answer, "add")==0) { /* add requires a points argument */
	action_mode = MODE_ADD;
	return 1;
    }
    else if(strcmp(tool_opt->answer, "delete")==0) { /* del requires a cats or or bbox or coords*/
	action_mode = MODE_DEL;
	if((cat_opt->answers == NULL) && 
           (coord_opt->answers == NULL) &&
           (bbox_opt->answers == NULL)) {
	    G_warning(_("At least one from <%s> must be specified"),"cats, coords, bbox");
            G_usage();
	    return 0;
	};
	return 1;
    }
    else if(strcmp(tool_opt->answer, "move")==0) { /* move requires coords or cats arguments */
	action_mode = MODE_MOVE;
	if(move_opt->answers == NULL) { 
            G_warning(_("For <%s> operation, option <%s> must be set"),"move","move");
            G_usage();
	    return 0;
        }
	if(coord_opt->answers == NULL && 
            (cat_opt->answers == NULL) &&
            (bbox_opt->answers == NULL)) {
	    G_warning(_("At least one from <%s> must be specified"),"cats, coords, bbox");
            G_usage();
	    return 0;
	};
	return 1;
    }
    else if(strcmp(tool_opt->answer, "merge")==0) { /* del requires a cats or or bbox or coords*/
	action_mode = MODE_MERGE;
	if((cat_opt->answers == NULL) && 
           (coord_opt->answers == NULL) &&
           (bbox_opt->answers == NULL)) {
	    G_warning(_("At least one from <%s> must be specified"),"cats, coords, bbox");
            G_usage();
	    return 0;
	};
	return 1;
    }
    else if(strcmp(tool_opt->answer, "break")==0) { /* break line requires a coord and at options */
	action_mode = MODE_BREAK;
	if(coord_opt->answers == NULL) {
	    G_warning(_("Required parameter <coords> not set"));
            G_usage();
	    return 0;
	};
	return 1;
    }
    else if(strcmp(tool_opt->answer, "vertex")==0) { /* vertex requires a coord and snap or bbox options */
        action_mode = MODE_VERTEX;
        if (coord_opt->answers == NULL && 
            bbox_opt->answers == NULL) {
	    G_warning(_("At least one from <%s> must be specified"),"coords, bbox");
            G_usage();
	    return 0;
        }
        if (move_opt->answers == NULL) {
            G_warning(_("For <%s> operation, option <%s> must be set"),"vertex","move");
            G_usage();
	    return 0;

        }
    }
    else if(strcmp(tool_opt->answer, "split")==0) { /* split line requires a coord and at options */
	action_mode = MODE_SPLIT;
	if(coord_opt->answers == NULL) {
	    G_warning(_("Required parameter <coords> not set"));
            G_usage();
	    return 0;
	};
	return 1;
    }
    else if(strcmp(tool_opt->answer, "straight")==0) { /* remove vertex */
	action_mode = MODE_STRAIGHTEN;
	if(coord_opt->answers == NULL) {
	    G_warning(_("Required parameter <coords> not set"));
            G_usage();
	    return 0;
	};
    }

    else {
	G_warning(_("Operation <%s> not implemented."),tool_opt->answer);
        G_usage();
	return 0;
    }
}

