/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *
 * PURPOSE:    This module edits vector maps. It is inteded to be mainly
 * 	       used by the the new v.digit GUI.
 *
 * COPYRIGHT:  (C) 2002-2007 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 * TODO:       
 ****************************************************************/

#define MAIN
#include "global.h"

int main (int argc, char *argv[])
{
    int ret;
    FILE *output=stderr;

    G_gisinit(argv[0]);

    module = G_define_module();
    module->keywords = _("vector, editing, geometry");
    module->description = _("Edits a vector map - allows adding, deleting "
			    "and modifying objects in a vector map.");

    if(!parser(argc, argv))
	exit(EXIT_FAILURE);

    /* open input file */
    if (in_opt -> answer) {
	ascii = fopen (in_opt -> answer, "r");
	if (ascii == NULL) {
	    G_fatal_error(_("Could not open ASCII file <%s>"),
			  in_opt -> answer);
	}
    }
    else {
	ascii = stdin;
    }

    if (action_mode == MODE_CREATE) {
	/* 3D vector maps? */
	if (-1 == Vect_open_new (&Map, map_opt -> answer, 0)) {
	    G_fatal_error (_("Cannot create vector map <%s>"),
			   map_opt -> answer);
	}

	G_debug(1, "Map created");
	
	Vect_hist_command (&Map);
	
	Vect_build (&Map, output);
	Vect_close (&Map);

	exit (EXIT_SUCCESS);
    }

    /* open selected vector file */    
    mapset = G_find_vector2 (map_opt->answer, G_mapset()); 
    if ( mapset == NULL ) {
	G_fatal_error (_("Cannot open vector map <%s>"),
		       map_opt->answer);
    }
    else {
        if (action_mode != MODE_SELECT)
            ret = Vect_open_update (&Map, map_opt->answer, mapset);
        else
            ret = Vect_open_old (&Map, map_opt->answer, mapset);
	
	if (ret < 2)
	    G_fatal_error (_("Cannot open vector map <%s> at topo level [%d]"),
			   map_opt -> answer, 2);
    }

    G_debug (1, "Map opened");

    /* perform requested editation */
    switch(action_mode) {
      case MODE_ADD:
        if ( ! n_flg->answer )
            read_head(ascii, &Map);
        ret = asc_to_bin(ascii, &Map) ;
	break;
      case MODE_DEL:
	ret = do_del(&Map);
	break;
      case MODE_MOVE:
        ret = do_move(&Map);
        break;
      case MODE_VERTEX:
        ret = do_move_vertex(&Map);
        break;
      case MODE_BREAK:
        ret = do_break(&Map);
        break;
      case MODE_STRAIGHTEN:
        ret = do_remove_vertex(&Map);
        break;
      case MODE_SPLIT:
        ret = do_split(&Map);
        break;
      case MODE_MERGE:
        ret = do_merge(&Map);
        break;
      case MODE_SELECT:
        ret = do_print_selected(&Map);
        break;
      case MODE_CATADD:
        ret = cats(&Map, 0);
        break;
      case MODE_CATDEL:
        ret = cats(&Map, 1);
        break;
      case MODE_COPY:
        ret = do_copy(&Map);
        break;
      case MODE_SNAP:
        ret = do_snap(&Map);
        break;
      default:
	G_warning(_("Operation not implemented."));
	ret = -1;
	break;
    }

    if(ret == -1)
	exit(EXIT_FAILURE);

    if(ret == 0)
	output=NULL;
    else
	Vect_hist_command(&Map);

    /* build topology only if requested or if tool!=select */
    if  (!(action_mode == MODE_SELECT || t_flg->answer == 1)) {
        Vect_build_partial(&Map, GV_BUILD_NONE, NULL);
        Vect_build (&Map, output);
    }

    Vect_close(&Map);
    G_debug(1, "Map closed");

    G_done_msg ("");


    exit(EXIT_SUCCESS);
}
