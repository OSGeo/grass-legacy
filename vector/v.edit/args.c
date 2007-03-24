#include "global.h"

int parser(int argc, char*argv[])
{
    map_opt = G_define_standard_option(G_OPT_V_MAP);

    fld_opt = G_define_standard_option(G_OPT_V_FIELD);

    type_opt = G_define_standard_option(G_OPT_V_TYPE);
    type_opt->answer           = "point,line,boundary,centroid" ;
    type_opt->options          = "point,line,boundary,centroid" ;

    tool_opt = G_define_option();
    tool_opt->key         = "tool";
    tool_opt->type        = TYPE_STRING;
    tool_opt->required    = YES;
    tool_opt->multiple    = NO;
    tool_opt->description = _("Editing tool");
    tool_opt->descriptions = _("create;"
			       "Create empty vector map;"
			       "add;"
			       "Add new feature(s) to existing vector map;"
			       "delete;"
			       "Delete feature(s) from vector map;"
			       "move;"
			       "Move feature(s) in vector map;"
			       "vertex;"
			       "Move one vertex;"
			       "straight;"
			       "Remove vertex;"
			       "break;"
			       "Add new vertex to existing vector line;"
			       "merge;"
			       "Merge vector line(s);"
			       "split;"
			       "Split line(s) into two separate lines;"
			       "select;"
			       "Select lines and print their ID's;"
			       "catadd;"
			       "Set new category to selected line(s) for defined layer;"
			       "catdel;"
			       "Delete category to selected line(s) for defined layer;"
			       "copy;"
			       "Copy selected features;"
			       "snap;"
			       "Snap one line to another");
    tool_opt->options     = "create,add,delete,move,vertex,straight,merge,"
      "break,split,select,catadd,catdel,copy,snap";

    in_opt = G_define_standard_option (G_OPT_F_INPUT);
    in_opt -> required = NO;
    in_opt -> description = _("ASCII file to be converted to binary vector map, "
			      "if not given reads from standard input");

    move_opt = G_define_option();
    move_opt->key         = "move";
    move_opt->key_desc    = "x,y";
    move_opt->type        = TYPE_DOUBLE;
    move_opt->required    = NO;
    move_opt->multiple    = NO;
    move_opt->description = _("Difference in x,y direction for moving feature or vertex");
    
    maxdist_opt = G_define_option();
    maxdist_opt->key         = "thresh";
    maxdist_opt->type        = TYPE_DOUBLE;
    maxdist_opt->required    = NO;
    maxdist_opt->multiple    = NO;
    maxdist_opt->description = _("Threshold distance");
    maxdist_opt->answer      = "0";

    cat_opt = G_define_standard_option(G_OPT_V_CATS);
    cat_opt->required    = NO;
    
    id_opt = G_define_standard_option(G_OPT_V_CATS);
    id_opt->required    = NO;
    id_opt->key         = "ids";
    id_opt->label = _("ID values");
    
    coord_opt = G_define_option();
    coord_opt->key         = "coords";
    coord_opt->key_desc    = "x,y";
    coord_opt->type        = TYPE_DOUBLE;
    coord_opt->required    = NO;
    coord_opt->multiple    = YES;
    coord_opt->description = _("List of point coordinates "
			       "(required for add and move actions)");
    
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

    where_opt = G_define_standard_option(G_OPT_WHERE);

    t_flg = G_define_flag();
    t_flg->key = 't';
    t_flg->description = _("Do not build topology");

    i_flg = G_define_flag();
    i_flg->key = 'i';
    i_flg->description = _("Print ID's of edited features");

    b_flg = G_define_flag();
    b_flg->key = 'b';
    b_flg->description = _("Assign cats to boundaries too");

    c_flg = G_define_flag();
    c_flg->key = 'c';
    c_flg->description = _("Do not close boundaries");

    n_flg = G_define_flag();
    n_flg->key          = 'n';
    n_flg->description  = _("Do not expect a header");

    if(G_parser(argc, argv))
	exit (EXIT_FAILURE);

    /* check for polygon param */
    if (poly_opt->answers != NULL) {
        int i = 0;
        while (poly_opt->answers[i++])
            ;

	if (i < 6)
            G_fatal_error (_("Polygon must have at least 3 coordinate pairs"));
    }
    
    /*
      check that the given arguments makes sense together
    */
    if(G_strcasecmp (tool_opt->answer, "create") == 0) { 
	/* add requires a points argument */
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
	G_fatal_error (_("Operation <%s> not implemented."),
		       tool_opt->answer);
    }

    if (action_mode == MODE_DEL ||
	action_mode == MODE_MOVE ||
	action_mode == MODE_MERGE ||
	action_mode == MODE_SELECT ||
	action_mode == MODE_COPY ||
	action_mode == MODE_SNAP)
    {
	if((cat_opt   -> answers  == NULL) && 
           (coord_opt -> answers  == NULL) &&
           (poly_opt  -> answers  == NULL) &&
           (id_opt    -> answers  == NULL) &&
           (bbox_opt  -> answers  == NULL) &&
	   (where_opt -> answer   == NULL)) {
	    G_fatal_error (_("At least one option from <%s> must be specified"),
			   "cats, coords, bbox, polygon, id, where");
	}
    }

    if (action_mode == MODE_MOVE ||
	action_mode == MODE_VERTEX)
    {
	if(move_opt->answers == NULL) { 
            G_fatal_error (_("Option <%s> must be set"),
			   "move");
        }
    }

    if (action_mode == MODE_BREAK ||
	action_mode == MODE_VERTEX ||
	action_mode == MODE_SPLIT ||
	action_mode == MODE_STRAIGHTEN)
    {
	if(coord_opt->answers == NULL) {
	    G_fatal_error (_("Required parameter <%s> not set"),
			   "coords");
	}
    }

    if (action_mode == MODE_CATADD ||
	action_mode == MODE_CATDEL)
    {
        if (cat_opt->answers == NULL) {
            G_fatal_error (_("Required parameter <%s> not set"),
			   "cats");
        }
    }

    return 1;
}
