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
			      "\t\tcreate - "
			      "Create new vector file\n"
			      "\t\tadd    - "
			      "Add new vector feature to existing vector file\n"
			      "\t\tdelete - "
			      "Delete feature from vector file\n"
			      "\t\tmove   - "
			      "Move feature in vector file\n"
			      "\t\tvertex - "
			      "Move one vertex\n"
			      "\t\tstraight - "
			      "Remove vertex\n"
			      "\t\tbreak  - "
			      "Add new vertex to existing vector line\n"
			      "\t\tmerge  - "
			      "Merge two vector lines togher\n"
			      "\t\tsplit  - "
			      "Split line into two separate lines\n"
			      "\t\tselect - "
			      "Select lines and print their ID's\n"
			      "\t\tcatadd - "
			      "Set new category to selected lines for defined layer\n"
			      "\t\tcatdel - "
			      "Delete category to selected lines for defined layer\n"
			      "\t\tcopy   - "
			      "Copy selected features\n"
			      "\t\tsnap   - "
			      "Snap one line to another");
    tool_opt->options     = "create,add,delete,move,vertex,straight,merge,"
	"break,split,select,catadd,catdel,copy,snap";

    input_opt = G_define_option();
    input_opt->key      = "input";
    input_opt->type     = TYPE_STRING;
    input_opt->required = NO;
    input_opt->multiple = NO;
    input_opt->description = _("ASCII file to be converted to binary vector file, "
			       "if not given reads from standard input");
    
    type_opt = G_define_standard_option(G_OPT_V_TYPE);
    type_opt->answer           = "point,line,boundary,centroid" ;
    type_opt->options          = "point,line,boundary,centroid" ;

    cat_opt = G_define_standard_option(G_OPT_V_CATS);
    cat_opt->required    = NO;

    id_opt = G_define_standard_option(G_OPT_V_CATS);
    id_opt->required    = NO;
    id_opt->key         = "ids";
    id_opt->description = _("ID's of selected features");
    
    coord_opt = G_define_option();
    coord_opt->key         = "coords";
    coord_opt->key_desc    = "x,y";
    coord_opt->type        = TYPE_DOUBLE;
    coord_opt->required    = NO;
    coord_opt->multiple    = YES;
    coord_opt->description = _("An x,y list of points. "
			       "Required for add and move actions.");
    
    bbox_opt =  G_define_option();
    bbox_opt->key         = "bbox";
    bbox_opt->key_desc    = "x1,y1,x2,y2";
    bbox_opt->type        = TYPE_DOUBLE;
    bbox_opt->required    = NO;
    bbox_opt->multiple    = NO;
    bbox_opt->description = _("Bounding box for selecting features");

    poly_opt =  G_define_option();
    poly_opt->key         = "polygon";
    poly_opt->key_desc    = "x,y";
    poly_opt->type        = TYPE_DOUBLE;
    poly_opt->required    = NO;
    poly_opt->multiple    = YES;
    poly_opt->description = _("Polygon for selecting features");

    move_opt = G_define_option();
    move_opt->key         = "move";
    move_opt->key_desc    = "x,y";
    move_opt->type        = TYPE_DOUBLE;
    move_opt->required    = NO;
    move_opt->multiple    = NO;
    move_opt->description = _("Difference in x,y direction for moving feature or vertex");
    
    maxdist_opt = G_define_option();
    maxdist_opt->key         = "distance";
    maxdist_opt->type        = TYPE_DOUBLE;
    maxdist_opt->required    = NO;
    maxdist_opt->multiple    = NO;
    maxdist_opt->description = _("Threshold distance");
    maxdist_opt->answer      = "0";
 
    fld_opt = G_define_standard_option(G_OPT_V_FIELD);

    t_flg = G_define_flag();
    t_flg->key = 't';
    t_flg->description = _("Do not build topology.");

    i_flg = G_define_flag();
    i_flg->key = 'i';
    i_flg->description = _("Print id's of edited features");

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

    /* check for polygon param */
    if (poly_opt->answers != NULL) {
        int i = 0;
        while (poly_opt->answers[i++])
            ;

	if (i < 6) {
            G_warning(_("Polygon must have at least 3 coordinate pairs"));
            G_usage();
	    return 0;
        }
    }
    
    /*
      check that the given arguments makes sense together
    */
    if(G_strcasecmp (tool_opt->answer, "create") == 0) { 
	/* create requires nothing extra*/
	action_mode = MODE_CREATE;
    }
    else if(G_strcasecmp (tool_opt->answer, "add") == 0) { 
	/* add requires a points argument */
	action_mode = MODE_ADD;
    }
    else if(G_strcasecmp (tool_opt->answer, "delete")==0) {
	/* del requires a cats or or bbox or coords */
	action_mode = MODE_DEL;
    }
    else if(G_strcasecmp (tool_opt->answer, "move") == 0) {
	/* move requires coords or cats arguments */
	action_mode = MODE_MOVE;
    }
    else if(G_strcasecmp (tool_opt->answer, "merge") == 0) { 
	/* del requires a cats or or bbox or coords */
	action_mode = MODE_MERGE;
    }
    else if(G_strcasecmp (tool_opt->answer, "break") == 0) { 
	/* break line requires a coord and at options */
	action_mode = MODE_BREAK;
    }
    else if(G_strcasecmp (tool_opt->answer, "vertex") == 0) { 
	/* vertex requires a coord and snap or bbox options */
        action_mode = MODE_VERTEX;
    }
    else if(G_strcasecmp (tool_opt->answer, "split") ==0 ) { 
        /* split line requires a coord and at options */
	action_mode = MODE_SPLIT;
    }
    else if(G_strcasecmp (tool_opt->answer, "straight") == 0) { 
        /* remove vertex */
	action_mode = MODE_STRAIGHTEN;
    }
    else if(G_strcasecmp (tool_opt->answer, "select") == 0) { 
        /* del requires a cats or or bbox or coords */
	action_mode = MODE_SELECT;
    }
    else if(G_strcasecmp (tool_opt->answer, "catadd") == 0) { 
        /* cat requires a cats or or bbox or coords */
	action_mode = MODE_CATADD;
    }
    else if(G_strcasecmp (tool_opt->answer, "catdel") == 0) {
        /* cat requires a cats or or bbox or coords */
	action_mode = MODE_CATDEL;
    }
    else if(G_strcasecmp(tool_opt->answer, "copy") == 0) { 
        /* del requires a cats or or bbox or coords */
	action_mode = MODE_COPY;
    }
    else if(G_strcasecmp (tool_opt->answer, "snap") == 0) {
	/* del requires a cats or or bbox or coords */ 
	action_mode = MODE_SNAP;
    }
    else
    {
	G_warning(_("Operation <%s> not implemented."),
		  tool_opt->answer);
	G_usage();
	return 0;
    }

    if (action_mode == MODE_DEL ||
	action_mode == MODE_MOVE ||
	action_mode == MODE_MERGE ||
	action_mode == MODE_SELECT ||
	action_mode == MODE_COPY ||
	action_mode == MODE_SNAP)
    {
	if((cat_opt->answers == NULL) && 
           (coord_opt->answers == NULL) &&
           (poly_opt->answers == NULL) &&
           (id_opt->answers == NULL) &&
           (bbox_opt->answers == NULL)) {
	    G_warning(_("At least one option from <%s> must be specified"),
		      "cats, coords, bbox, polygon, id");
            G_usage();
	    return 0;
	}
    }

    if (action_mode == MODE_MOVE ||
	action_mode == MODE_VERTEX)
    {
	if(move_opt->answers == NULL) { 
            G_warning(_("Option <%s> must be set"),
		      "move");
            G_usage();
	    return 0;
        }
    }

    if (action_mode == MODE_BREAK ||
	action_mode == MODE_VERTEX ||
	action_mode == MODE_SPLIT ||
	action_mode == MODE_STRAIGHTEN)
    {
	if(coord_opt->answers == NULL) {
	    G_warning(_("Required parameter <%s> not set"),
		      "coords");
            G_usage();
	    return 0;
	}
    }

    if (action_mode == MODE_CATADD ||
	action_mode == MODE_CATDEL)
    {
        if (cat_opt->answers == NULL) {
            G_warning(_("Required parameter <%s> not set"),
		      "cats");
            return 0;
        }
    }

    return 1;
}
