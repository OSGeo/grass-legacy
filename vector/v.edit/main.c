/****************************************************************
 *
 * MODULE:     v.edit
 *
 * AUTHOR(S):  GRASS Development Team
 *
 * PURPOSE:    This module edits vector maps. It is inteded to be mainly
 * 	       used by the the new v.digit GUI.
 *
 * COPYRIGHT:  (C) 2002-2006 by the GRASS Development Team
 *
 *             This program is free software under the
 *             GNU General Public License (>=v2).
 *             Read the file COPYING that comes with GRASS
 *             for details.
 *
 * TODO:       
 ****************************************************************/
#include <grass/gis.h>
#include <grass/glocale.h>

#define MAIN
#include "global.h"

/* static int error_routine(const char*msg, int fatal); */

int main (int argc, char *argv[])
{
    G_gisinit(argv[0]);
    int ret;
    FILE *output=stdout;

    module = G_define_module();
    module->keywords = _("vector, editing");
    module->description = _("Edits a vector map; allows adding, deleting and modifying objects in a vector map.");

    if(!parser(argc, argv))
	exit(EXIT_FAILURE);

/*     G_set_error_routine(error_routine); */
    mapset = G_find_vector2 (map_opt->answer, G_mapset()); 

    if ( mapset == NULL ) {
	if ( action_mode == MODE_CREATE ) {
	    Vect_open_new (&Map, map_opt->answer, 0 );
	    Vect_build ( &Map, NULL );
	    Vect_close (&Map);
	    Vect_open_update (&Map, map_opt->answer, G_mapset());
	    G_message(_("New empty map created."));
	    Vect_close(&Map);
	    G_debug (1, "Map closed");
	    exit(EXIT_SUCCESS);
	} else {
	    G_message(_("Map does not exist. Add flag -n to create a new map."));
	    exit(EXIT_FAILURE);
	}
    }
    else {
/* 	Vect_set_open_level(2); */
        if (action_mode != MODE_SELECT)
            Vect_open_update (&Map, map_opt->answer, mapset);
        else
            Vect_open_old (&Map, map_opt->answer, mapset);
    }
/*     Vect_set_category_index_update ( &Map ); */

    G_debug (1, "Map opened");
    cat_init(&Map);

    switch(action_mode) {
      case MODE_ADD:
	G_message(_("Adding new features to vector file ..."));
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
        ret = do_select(&Map);
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
      default:
	G_warning("Sorry this is not yet implemented");
	ret=0;
	break;
    }

    
    if(ret == 0)
	output=NULL;
    else
	Vect_hist_command(&Map);

    if  (action_mode != MODE_SELECT || t_flg->answer!=0) {
        Vect_build_partial(&Map, GV_BUILD_NONE, NULL);
        Vect_build(&Map, output );
    }

    Vect_close(&Map);
    G_debug(1, "Map closed");

/*     G_unset_error_routine(); */
    
    if(ret)
	exit(EXIT_SUCCESS);
    else
	exit(EXIT_FAILURE);
}
/*
static int error_routine(const char*msg, int fatal)
{
    fprintf(stderr,"%s%s\7", fatal?_("ERROR: "),_("WARNING: "), msg);
    fflush(stderr);
    if(fatal) {
	G_debug (1, "In error routine");
	Vect_build ( &Map, NULL );
	Vect_close(&Map);
	G_debug (1, "Map closed");
    }
    return 0;
}
*/
/*    
	struct Map_info map;
	static struct line_pnts *points;
	struct line_cats *cats;
        int    i, type, cat;
	char   *mapset;


	
	
	
	
	
	
	
	
	
	
	
	
	
	old = G_define_standard_option(G_OPT_V_INPUT);
	
	new = G_define_standard_option(G_OPT_V_OUTPUT);

        if (G_parser (argc, argv))
	    exit(EXIT_FAILURE); 
	
        Points = Vect_new_line_struct ();
	Cats = Vect_new_cats_struct ();
	
	Vect_check_input_output_name ( new->answer, old->answer, GV_FATAL_EXIT );

	if ((mapset = G_find_vector2 (old->answer, "")) == NULL)
	     G_fatal_error ( _("Could not find input %s"), old->answer);
	
        Vect_set_open_level (2);
	
	if (1 > Vect_open_old (&In, old->answer, mapset) )
	     G_fatal_error ( _("Could not open input") );
	
	if (0 > Vect_open_new (&Out, new->answer, WITHOUT_Z))
	{
	     Vect_close (&In);
	     G_fatal_error ( _("Could not open output") );
	}

	Vect_copy_head_data (&In, &Out);
	Vect_hist_copy (&In, &Out);
	Vect_hist_command ( &Out );

	i=1;
	while ( (type = Vect_read_next_line (&In, Points, Cats)) > 0)
	  {
	    if ( type == GV_LINE )
	       {
                 if( Vect_cat_get (Cats, 1, &cat) == 0)
	           {
                      Vect_cat_set (Cats, 1, i);
	              i++;
	           }
	       }	   
	    Vect_write_line ( &Out, type, Points, Cats );  
	  }
	  
	Vect_build (&Out, stdout );
	Vect_close (&In);
	Vect_close (&Out);

	exit(EXIT_SUCCESS) ;
}

*/
