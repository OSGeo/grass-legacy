/*-s.delaunay
**
** Author: James Darrell McCauley (mccauley@ecn.purdue.edu)
**         USDA Fellow
**         Department of Agricultural Engineering
**         Purdue University
**         West Lafayette, Indiana 47907-1146 USA
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted. This
** software is provided "as is" without express or implied warranty.
**
** Modification History:
** 06 Feb 93 - James Darrell McCauley <mccauley@ecn.purdue.edu> pieced
**             this together from stuff he found on netlib (see the manpage).
**/

#define MAIN
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "glocale.h"
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "sw_defs.h"
#include "defs.h"

typedef struct {
    double x, y;
} COOR;

int cmp ( void *a, void *b ) {
    COOR *ca = (COOR*) a;
    COOR *cb = (COOR*) b;
    double thresh = 1e-10; 
    double ma, mb;

    /* calculate measure */
    ma = mb = 0.0;
    
    if ( fabs(ca->y - Box.S) < thresh ) { /* bottom */
	ma = ca->x - Box.W;
    } else if ( fabs(ca->x - Box.E) < thresh ) { /* right */
	ma = (Box.E - Box.W) + (ca->y - Box.S) ;
    } else if ( fabs(ca->y - Box.N) < thresh ) { /* top */
	ma = (Box.E - Box.W) + (Box.N - Box.S) + (Box.E - ca->x);
    } else { /* left */
	ma = 2*(Box.E - Box.W) + (Box.N - Box.S) + (Box.N - ca->y);
    }

    
    if ( fabs(cb->y - Box.S) < thresh ) { /* bottom */
	mb = cb->x - Box.W;
    } else if ( fabs(cb->x - Box.E) < thresh ) { /* right */
	mb = (Box.E - Box.W) + (cb->y - Box.S) ;
    } else if ( fabs(cb->y - Box.N) < thresh ) { /* top */
	mb = (Box.E - Box.W) + (Box.N - Box.S) + (Box.E - cb->x);
    } else { /* left */
	mb = 2*(Box.E - Box.W) + (Box.N - Box.S) + (Box.N - cb->y);
    }
    
    if ( ma < mb ) return -1;
    if ( ma > mb ) return 1;
    return 0;
}

int 
main (int argc, char **argv)
{
  int i;
  int **cats, *ncats, nfields, *fields;
  char *mapset;
  struct Flag *line_flag;
  /* struct Flag *all_flag; */
  struct Option *in_opt, *out_opt;
  struct Flag *table_flag;
  struct GModule *module;
  struct line_pnts *Points;
  struct line_cats *Cats;
  int node, nnodes;
  COOR *coor;
  int ncoor, acoor;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description = "Create a Delaunay triangulation from an input vector of points or centroids.";

  in_opt = G_define_standard_option(G_OPT_V_INPUT);
  out_opt = G_define_standard_option(G_OPT_V_OUTPUT);

  /*
  all_flag = G_define_flag ();
  all_flag->key = 'a';
  all_flag->description = "Use all sites (do not limit to current region)";
  */

  line_flag = G_define_flag ();
  line_flag->key = 'l';
  line_flag->description = "Output triangulation as a graph (lines), not areas";

  table_flag = G_define_flag ();
  table_flag->key             = 't';
  table_flag->description     = "Do not create attribute table.";

  if (G_parser (argc, argv))
    exit (1);

  if ( line_flag->answer ) 
      Type = GV_LINE;
  else
      Type = GV_BOUNDARY;

  All = 0;

  Points = Vect_new_line_struct ();
  Cats = Vect_new_cats_struct ();

  /* open files */
  if ((mapset = G_find_vector2 (in_opt->answer, "")) == NULL) {
      G_fatal_error ( "Could not find input map <%s>\n", in_opt->answer);
  }

  Vect_set_open_level (2);
  Vect_open_old (&In, in_opt->answer, mapset);
  
  if (0 > Vect_open_new (&Out, out_opt->answer, 0)) {
    G_fatal_error ( "Not able to open vector file <%s>\n", out_opt->answer);
  }

  /* initialize working region */
  G_get_window (&Window);
  Vect_region_box ( &Window, &Box );

  freeinit(&sfl, sizeof *sites);

  readsites ( );
  
  siteidx = 0;
  geominit ();

  triangulate = 0;
  plot = 0; 
  debug = 0;
  voronoi ( triangulate, nextone);

  /* Close free ends by cyrrent region */
  Vect_build_partial ( &Out, GV_BUILD_BASE, NULL );

  ncoor = 0;
  acoor = 100;
  coor = (COOR *) malloc ( sizeof(COOR) * acoor );

  nnodes = Vect_get_num_nodes ( &Out );
  for ( node = 1; node <= nnodes; node++ ) {
      double x, y;

      if ( Vect_get_node_n_lines(&Out,node) < 2 ) { /* add coordinates */
	  Vect_get_node_coor ( &Out, node, &x, &y, NULL );

	  if ( ncoor == acoor - 5 ) { /* always space for 5 region corners */
	      acoor += 100; 
	      coor = (COOR *) realloc ( coor, sizeof(COOR) * acoor );
	  }
	  
	  coor[ncoor].x = x;
	  coor[ncoor].y = y;
	  ncoor++;
      }
  }
    
  /* Add region corners */
  coor[ncoor].x = Box.W;
  coor[ncoor].y = Box.S;
  ncoor++;
  coor[ncoor].x = Box.E;
  coor[ncoor].y = Box.S;
  ncoor++;
  coor[ncoor].x = Box.E;
  coor[ncoor].y = Box.N;
  ncoor++;
  coor[ncoor].x = Box.W;
  coor[ncoor].y = Box.N;
  ncoor++;

  /* Sort */
  qsort ( coor, ncoor, sizeof(COOR), (void *)cmp );
  
  /* add last (first corner) */
  coor[ncoor].x = Box.W;
  coor[ncoor].y = Box.S;
  ncoor++;

  for ( i = 1; i < ncoor; i++ ) {
     if ( coor[i].x == coor[i-1].x && coor[i].y == coor[i-1].y ) continue; /* duplicate */

     Vect_reset_line ( Points );
     Vect_append_point ( Points, coor[i].x, coor[i].y, 0.0 );
     Vect_append_point ( Points, coor[i-1].x, coor[i-1].y, 0.0 );
     Vect_write_line ( &Out, Type, Points, Cats );
  }

  free (coor);

  /* Copy input points as centroids */
  nfields = Vect_cidx_get_num_fields ( &In );
  cats = (int **) G_malloc ( nfields * sizeof(int *) );
  ncats = (int *) G_malloc ( nfields * sizeof(int) );
  fields = (int *) G_malloc ( nfields * sizeof(int) );
  for ( i = 0; i < nfields; i++ ) {
    ncats[i] = 0;
    cats[i] = (int *) G_malloc ( Vect_cidx_get_num_cats_by_index(&In,i) * sizeof(int) );
    fields[i] = Vect_cidx_get_field_number ( &In, i );
  }

  if ( 1 ) {
      int line, nlines, type;
  
      nlines = Vect_get_num_lines ( &In );

      for ( line = 1; line <= nlines; line++ ) {
	  
	  type = Vect_read_line ( &In, Points, Cats, line );
	  if ( !(type & GV_POINTS ) ) continue;

	  if ( !Vect_point_in_box ( Points->x[0], Points->y[0], 0.0, &Box) ) continue;

	  Vect_write_line ( &Out, GV_CENTROID, Points, Cats );

	  
	  for ( i = 0; i < Cats->n_cats; i++ ) {
		int f, j; 
		for ( j = 0; j < nfields; j++ ) { /* find field */
	    	    if ( fields[j] == Cats->field[i] ) {
			f = j;
			break;
		    }
		}
		cats[f][ncats[f]] = Cats->cat[i];
		ncats[f]++;
	  }
      }

  }

  /* Copy tables */
  if ( !(table_flag->answer)) {
        int ttype, ntabs=0;
        struct field_info *IFi, *OFi;

	/* Number of output tabs */
	for ( i = 0; i < Vect_get_num_dblinks ( &In ); i++ ) {
	    int f, j;
	    
	    IFi = Vect_get_dblink ( &In, i );
	    
	    for ( j = 0; j < nfields; j++ ) { /* find field */
	    	if ( fields[j] == IFi->number ) {
		    f = j;
		    break;
		}
	    }
	    if ( ncats[f] > 0 ) ntabs++;
	}
	
	if ( ntabs > 1 )
	    ttype = GV_MTABLE;
	else 
	    ttype = GV_1TABLE;
	
	for ( i = 0; i < nfields; i++ ) {
	    int ret;

	    if ( fields[i] == 0 ) continue;
	
	    G_message ( "Layer %d", fields[i] );

	    /* Make a list of categories */
	    IFi = Vect_get_field ( &In, fields[i] );
	    if ( !IFi ) { /* no table */
		G_message ( "No table." );
		continue;
	    }
	    
	    OFi = Vect_default_field_info ( &Out, IFi->number, IFi->name, ttype );

	    ret = db_copy_table_by_ints ( IFi->driver, IFi->database, IFi->table,
				  OFi->driver, Vect_subst_var(OFi->database,&Out), OFi->table,
				  IFi->key, cats[i], ncats[i] );

	    if ( ret == DB_FAILED ) {
		G_warning ( _("Cannot copy table") );
	    } else {
		Vect_map_add_dblink ( &Out, OFi->number, OFi->name, OFi->table, 
				      IFi->key, OFi->database, OFi->driver);
	    }
	    G_message ( "Done." );
	}
  }
	  

  Vect_close ( &In );
  Vect_build ( &Out, stderr );
  Vect_close ( &Out );

  return 0;
}

