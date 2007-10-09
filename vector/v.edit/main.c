/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *             Wolf Bergenheim, Jachym Cepicky, Martin Landa
 *
 * PURPOSE:    This module edits vector map.
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
    struct Map_info **BgMap; /* backgroud vector maps */
    int nbgmaps;             /* number of registrated background maps */
    char *mapset;
    enum mode action_mode;
    FILE *ascii;

    int i;
    int move_first, snap;
    int ret, print, layer;
    double move_x, move_y, thresh;
    
    struct line_pnts *coord;

    struct ilist *List;

    ascii  = NULL;
    List   = NULL;
    BgMap  = NULL;
    nbgmaps = 0;
    coord  = NULL;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("vector, editing, geometry");
    module->description = _("Edits a vector map, allows adding, deleting "
			    "and modifying selected vector features.");

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
	    G_fatal_error(_("Unable to open ASCII file <%s>"),
			  params.in -> answer);
	}
    }

    if (action_mode == MODE_CREATE) {
	/* 3D vector maps? */
	if (-1 == Vect_open_new (&Map, params.map -> answer, 0)) { /* new */
	    G_fatal_error (_("Unable to create vector map <%s>"),
			   params.map -> answer);
	}

	G_debug(1, "Map created");
	
	if (ascii) {
	    /* also add new vector features */
	    action_mode = MODE_ADD;
	}
    }
    else { /* open selected vector file */
	mapset = G_find_vector2 (params.map -> answer, G_mapset()); 
	if ( mapset == NULL ) {
	    G_fatal_error (_("Vector map <%s> not found in the current mapset"),
			   params.map -> answer);
	}
	else if (action_mode == MODE_ADD) { /* write */
	    ret = Vect_open_update (&Map, params.map -> answer, mapset); 
	}
	else { /* read-only -- select features */
	    ret = Vect_open_old (&Map, params.map -> answer, mapset);
	}
	
	if (ret < 2)
	    G_fatal_error (_("Unable to open vector map <%s> at topological level %d"),
			   params.map -> answer, 2);
    }
    
    G_debug (1, "Map opened");

    /* open backgroud maps */
    if (params.bmaps->answer) {
	i = 0;
	char *bmap;
	while (params.bmaps->answers[i]) {
	    bmap = params.bmaps->answers[i];
	    mapset = G_find_vector2 (bmap, ""); 
	    if (mapset == NULL) {
		G_fatal_error (_("Vector map <%s> not found"),
			       bmap);
	    }

	    if (strcmp(G_fully_qualified_name((const char*) params.map -> answer, (const char*) G_mapset()),
		       G_fully_qualified_name((const char*) bmap, (const char*) mapset)) == 0) {
		G_fatal_error (_("Unable to open vector map <%s> as the backround map. "
				 "It is given as vector map to be edited."),
			       bmap);
	    }
	    nbgmaps++;
	    BgMap = (struct Map_info**) G_realloc ((void *) BgMap, nbgmaps * sizeof(struct Map_info*));
	    BgMap[nbgmaps-1] = (struct Map_info *) G_malloc (sizeof(struct Map_info));
	    if (Vect_open_old(BgMap[nbgmaps-1], bmap, mapset) == -1) {
		G_fatal_error (_("Unable to open vector map <%s>"),
			       bmap);
	    }
	    i++;
	}
    }

    layer = atoi (params.fld -> answer);
    thresh = atof (params.maxdist -> answer);
    move_first = params.move_first->answer ? 1 : 0;
    snap = NO_SNAP;
    if (strcmp(params.snap->answer, "node") == 0)
	snap = SNAP;
    else if (strcmp(params.snap->answer, "vertex") == 0)
	snap = SNAPVERTEX;

    if (action_mode != MODE_CREATE && action_mode != MODE_ADD) {
	/* select lines */
	List = Vect_new_list();
	List = select_lines(&Map, action_mode,
			    &params,
			    List);
    }

    if ((action_mode != MODE_CREATE && action_mode != MODE_ADD &&
	 action_mode != MODE_SELECT)) {
	if (List -> n_values < 1) {
	    G_warning (_("No features selected, nothing to edit"));
	    action_mode = MODE_NONE;
	    ret = 0;
	}
	else {
	    /* reopen the map for updating */
	    Vect_close (&Map);
	    Vect_open_update (&Map, params.map -> answer, mapset); 
	}
    }

    /* coords option -> array */
    if (params.coord -> answers) {
	coord = Vect_new_line_struct();
	int i = 0;
	double east, north;
	while (params.coord -> answers[i]) {
	    east = atof (params.coord -> answers[i]);
	    north = atof (params.coord -> answers[i+1]);
	    Vect_append_point(coord, east, north, 0.0);
	    i+=2;
	}
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
	struct ilist *List_added;
	List_added = Vect_new_list();
	ret = asc_to_bin(ascii, &Map, List_added);
	G_message(_("%d features added"), ret);
	if (ret > 0) {
	    if (snap != NO_SNAP) { /* apply snapping */
		do_snapping (&Map, BgMap, nbgmaps,
			     List_added,
			     thresh,
			     snap == SNAP ? 0 : 1); /* snap to vertex ? */
	    }
	    if (params.close -> answer) { /* close boundaries */
		int nclosed;
		nclosed = do_close (&Map, GV_BOUNDARY, thresh);
		G_message (_("%d lines closed"), nclosed);
	    }
	}
	Vect_destroy_list (List_added);
	break;
    case MODE_DEL:
	ret = do_delete(&Map, List);
	G_message(_("%d features deleted"), ret);
	break;
    case MODE_MOVE:
	move_x = atof(params.move -> answers[0]);
	move_y = atof(params.move -> answers[1]);
	ret = do_move(&Map, List,
		      move_x, move_y, snap, thresh);
	G_message(_("%d features moved"), ret);
	break;
    case MODE_VERTEX_MOVE:
	move_x = atof(params.move -> answers[0]);
	move_y = atof(params.move -> answers[1]);
	ret = do_move_vertex (&Map, BgMap, nbgmaps,
			      List,
			      coord, thresh,
			      move_x, move_y,
			      move_first, snap);
	G_message(_("%d vertices moved"), ret);
	break;
    case MODE_VERTEX_ADD:
	ret = do_add_vertex (&Map, List,
			     coord, thresh);
	G_message(_("%d vertices added"), ret);    
	break;
    case MODE_VERTEX_DELETE:
	ret = do_remove_vertex(&Map, List,
			       coord, thresh);
	G_message(_("%d vertices removed"), ret);
	break;
    case MODE_BREAK:
	ret = do_break(&Map, List,
		       coord, thresh, NULL);
	G_message(_("%d lines broken"), ret);
	break;
    case MODE_CONNECT:
	ret = do_connect(&Map, List,
			 thresh);
	G_message(_("%d lines connected"), ret);
      	break;
    case MODE_MERGE:
	ret = do_merge(&Map, List);
	G_message (_("%d lines merged"), ret);
    	break;
    case MODE_SELECT:
	print = 1;
	ret = do_print_selected(List);
	break;
    case MODE_CATADD:
	ret = cats(&Map, List,
		   layer, 0, params.cat -> answer);
	G_message(_("%d features modified"), ret);
    	break;
    case MODE_CATDEL:
	ret = cats(&Map, List,
		   layer, 1, params.cat -> answer);
	G_message(_("%d features modified"), ret);
	break;
    case MODE_COPY:
	if (BgMap && BgMap[0])
	    ret = do_copy(&Map, BgMap[0], List);
	else
	    ret = do_copy(&Map, NULL, List);
	G_message (_("%d features copied"), ret);
	break;
    case MODE_SNAP:
	ret = do_snap(&Map, List, thresh);
	break;
    case MODE_FLIP:
	ret = do_flip(&Map, List);
	G_message(_("%d lines flipped"), ret);
	break;
    case MODE_NONE:
	print = 0;
	break;
    default:
	G_warning(_("Operation not implemented"));
	ret = -1;
	break;
    }

    /*
    if (print && ret > 0) {
	for (i = 0; i < Vect_get_num_updated_lines(&Map); i++) {
	    if (i > 0)
		fprintf (stdout, ",");
	    fprintf (stdout, "%d", Vect_get_updated_line(&Map, i));
	}
	if (Vect_get_num_updated_lines(&Map) > 0)
	    fprintf (stdout, "\n");
	fflush (stdout);
    }
    */

    Vect_hist_command(&Map);
    
    /* build topology only if requested or if tool!=select */
    if  (!(action_mode == MODE_SELECT || params.topo -> answer == 1 || !MODE_NONE)) {
        Vect_build_partial(&Map, GV_BUILD_NONE, NULL);
	if (G_verbose() > G_verbose_min())
	    Vect_build (&Map, stderr);
	else
	    Vect_build (&Map, NULL);
    }

    if (List)
	Vect_destroy_list(List);

    Vect_close(&Map);
    
    G_debug(1, "Map closed");

    /* close background maps */
    for (i = 0; i < nbgmaps; i++) {
	Vect_close(BgMap[i]);
	G_free ((void *) BgMap[i]);
    }
    G_free ((void *) BgMap);

    if (coord)
	Vect_destroy_line_struct(coord);

    G_done_msg (" ");

    if (ret > -1) {
	exit (EXIT_SUCCESS);
    }
    else {
	exit (EXIT_FAILURE);
    }
}
