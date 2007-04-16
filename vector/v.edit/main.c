/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Wolf Bergenheim, Jachym Cepicky, Martin Landa
 *
 * PURPOSE:    This module edits vector maps. 
 *
 * COPYRIGHT:  (C) 2002-2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 * TODO:       3D support
 ****************************************************************/

#include "global.h"

int main (int argc, char *argv[])
{
    struct GModule *module;
    struct GParams params;
    struct Map_info Map;
    char *mapset;
    enum mode action_mode;
    FILE *ascii, *output;

    int ret, print, layer;
    double move_x, move_y, thresh;
	    
    struct ilist *List;

    ascii  = NULL;
    output = stderr;
    List   = NULL;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("vector, editing, geometry");
    module->description = _("Edits a vector map - allows adding, deleting "
			    "and modifying objects in a vector map.");

    if(!parser(argc, argv, &params, &action_mode))
	exit(EXIT_FAILURE);

    /* open input file */
    if (G_strcasecmp (params.in -> answer, "-") == 0 ||
	(action_mode != MODE_CREATE && params.in -> answer == NULL)) {
	ascii = stdin;
    }
    else if (params.in -> answer) {
	ascii = fopen (params.in -> answer, "r");
	if (ascii == NULL) {
	    G_fatal_error(_("Could not open ASCII file <%s>"),
			  params.in -> answer);
	}
    }

    if (action_mode == MODE_CREATE) {
	/* 3D vector maps? */
	if (-1 == Vect_open_new (&Map, params.map -> answer, 0)) {
	    G_fatal_error (_("Cannot create vector map <%s>"),
			   params.map -> answer);
	}

	G_debug(1, "Map created");
	
	if (ascii) {
	    /* also add new vector features */
	    action_mode = MODE_ADD;
	}
    }
    else {
	/* open selected vector file */    
	mapset = G_find_vector2 (params.map -> answer, G_mapset()); 
	if ( mapset == NULL ) {
	    G_fatal_error (_("Vector map <%s> not found in the current mapset"),
			   params.map -> answer);
	}
	else {
	    if (action_mode != MODE_SELECT)
		ret = Vect_open_update (&Map, params.map -> answer, mapset);
	    else
		ret = Vect_open_old (&Map, params.map -> answer, mapset);
	    
	    if (ret < 2)
		G_fatal_error (_("Cannot open vector map <%s> at topo level [%d]"),
			       params.map -> answer, 2);
	}
	
    }

    G_debug (1, "Map opened");

    print = params.print -> answer ? 1 : 0;
    layer = atoi (params.fld -> answer);
    thresh = atof (params.maxdist -> answer);

    if (action_mode != MODE_CREATE) {
	if (action_mode != MODE_ADD) {
	    /* select lines */
	    List = Vect_new_list();
	    List = select_lines(&Map, action_mode,
				&params,
				List);
	}
    }    

    if ((action_mode != MODE_CREATE &&
	 action_mode != MODE_ADD &&
	 action_mode != MODE_SELECT) &&
	List -> n_values < 1) {
	G_warning (_("No features selected, nothing to edit"));
	action_mode = MODE_NONE;
	ret = 0;
    }

    /* perform requested editation */
    switch(action_mode) {
    case MODE_CREATE:
	print = 0; /* do not print id's */
	break;
    case MODE_ADD:
	print = 0;
	if (!params.header -> answer)
	    read_head(ascii, &Map);
	ret = asc_to_bin(ascii, &Map) ;
	break;
    case MODE_DEL:
	ret = do_del(&Map, List, print);
	break;
    case MODE_MOVE:
	move_x = atof(params.move -> answers[0]);
	move_y = atof(params.move -> answers[1]);
	ret = do_move(&Map, List, print,
		      move_x, move_y);
	break;
    case MODE_VERTEX_MOVE:
	move_x = atof(params.move -> answers[0]);
	move_y = atof(params.move -> answers[1]);
	ret = do_move_vertex (&Map, List, print,
			      params.coord, thresh,
			      move_x, move_y);
	break;
    case MODE_VERTEX_ADD:
	ret = do_add_vertex (&Map, List, print,
			     params.coord, thresh);
	break;
    case MODE_VERTEX_DELETE:
	ret = do_remove_vertex(&Map, List, print,
			       params.coord, thresh);
	break;
    case MODE_BREAK:
	ret = do_split(&Map, List, print,
		       params.coord, thresh);
	break;
    case MODE_MERGE:
	ret = do_merge(&Map, List, print);
	break;
    case MODE_SELECT:
	print = 1;
	ret = do_print_selected(List);
	break;
    case MODE_CATADD:
	ret = cats(&Map, List, print,
		   layer, 0, params.cat -> answer);
	break;
    case MODE_CATDEL:
	ret = cats(&Map, List, print,
		   layer, 1, params.cat -> answer);
	break;
    case MODE_COPY:
	ret = do_copy(&Map, List, print);
	break;
    case MODE_SNAP:
	ret = do_snap(&Map, List, print);
	break;
    case MODE_FLIP:
	ret = do_flip(&Map, List, print);
	break;
    case MODE_NONE:
	print = 0;
	break;
    default:
	G_warning(_("Operation not implemented"));
	ret = -1;
	break;
    }

    if (print && ret > 0) {
	fprintf (stdout, "\n");
	fflush (stdout);
    }

    Vect_hist_command(&Map);
    
    if (ret == 0) {
	output = NULL;
    }

    /* build topology only if requested or if tool!=select */
    if  (!(action_mode == MODE_SELECT || params.topo -> answer == 1)) {
        Vect_build_partial(&Map, GV_BUILD_NONE, NULL);
        Vect_build (&Map, output);
    }

    if (List)
	Vect_destroy_list(List);

    Vect_close(&Map);
    
    G_debug(1, "Map closed");

    G_done_msg (NULL);

    if (ret > -1) {
	exit (EXIT_SUCCESS);
    }
    else {
	exit (EXIT_FAILURE);
    }
}
