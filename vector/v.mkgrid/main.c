/*
 * Written by GRASS, Fall of 88,  Michael H. Updated <02 Jun 1992> by Darrell
 * McCauley <mccauley@ecn.purdue.edu> angle option added.
 * Upgrade to 5.7 Radim Blazek 10/2004
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "grid_structs.h"
#include "local_proto.h"

int 
main (int argc, char *argv[])
{

  /* loop */
  int i,j;

  /* store filename and path  */
  char *dig_file;
  char buf[2000];

  /* Other local variables */
  int attCount;

  struct grid_description grid_info;
  struct Cell_head window;
  struct Map_info Map;
  struct Option *vectname, *grid, *coord, *box, *angle, *position_opt;
  struct Flag *q;
  struct GModule *module;

  struct line_pnts *Points;
  struct line_cats *Cats;

  /* Attributes */
  struct field_info *Fi;
  dbDriver *Driver;
  dbString sql;

  G_gisinit (argv[0]);
  
  /* Set description */
  module              = G_define_module();
  module->description = "Creates a (binary) GRASS vector map of a user-defined grid.";

  vectname = G_define_option ();
  vectname->key = "map";
  vectname->type = TYPE_STRING;
  vectname->required = YES;
  vectname->multiple = NO;
  vectname->gisprompt = "new,vector,vector";
  vectname->description = "name of vector map";

  grid = G_define_option ();
  grid->key = "grid";
  grid->key_desc = "rows,columns";
  grid->type = TYPE_INTEGER;
  grid->required = YES;
  grid->multiple = NO;
  grid->description = "number of ROWS and COLUMNS in grid";
  
  position_opt = G_define_option ();
  position_opt->key = "position";
  position_opt->type = TYPE_STRING;
  position_opt->required = YES;
  position_opt->multiple = NO;
  position_opt->options = "region,coor";
  position_opt->answer = "region";
  position_opt->description = "Where to place the grid:\n"
    			      "\tregion - current region\n"
			      "\tcoor - use 'coor' and 'box' options";

  coord = G_define_option ();
  coord->key = "coor";
  coord->key_desc = "x,y";
  coord->type = TYPE_DOUBLE;
  coord->required = NO;
  coord->multiple = NO;
  coord->description = "lower left EASTING and NORTHING coordinates of map";

  box = G_define_option ();
  box->key = "box";
  box->key_desc = "width,height";
  box->type = TYPE_DOUBLE;
  box->required = NO;
  box->multiple = NO;
  box->description = "WIDTH and HEIGHT of boxes in grid";

  angle = G_define_option ();
  angle->key = "angle";
  angle->type = TYPE_DOUBLE;
  angle->required = NO;
  angle->description = "angle of rotation";
  angle->answer = "0";

  q = G_define_flag ();
  q->key = 'q';
  q->description = "Quiet; No chatter";
  q->answer = 0;

  if (G_parser (argc, argv))
    exit (-1);

  /* get the current window  */
  G_get_window (&window);

  /*
   * information we need to collect from user: origin point x and y (lower
   * left), shift in x, shift in y,  number of rows, number of cols
   */
  dig_file = G_store (vectname->answer);

  /* Number of row and cols */
  grid_info.num_rows = atoi (grid->answers[0]);
  grid_info.num_cols = atoi (grid->answers[1]);
    
  grid_info.angle = 3.1415927 / 180 * atof (angle->answer);

  /* Position */
  if ( position_opt->answer[0] == 'r' ) { /* region */
    if ( coord->answer ) 
      G_warning ( "'coor' option ignored with 'position=region'" );

    if ( box->answer ) 
      G_warning ( "'box' option ignored with 'position=region'" );

    grid_info.origin_x = window.west;
    grid_info.origin_y = window.south;

    grid_info.length = (window.east - window.west) / grid_info.num_cols;
    grid_info.width = (window.north - window.south) / grid_info.num_rows;

    G_debug ( 2, "x = %e y = %e l = %e w = %e", grid_info.origin_x, grid_info.origin_y, 
	                                        grid_info.length, grid_info.width );

    if ( grid_info.angle != 0.0 ) {
	G_warning ( "'angle' ignored ");
	grid_info.angle = 0.0;
    }
  } else {
    if ( !coord->answer ) 
      G_fatal_error ( "'coor' option missing" );

    if ( !box->answer ) 
      G_fatal_error ( "'box' option missing" );

    if(!G_scan_easting(coord->answers[0], &(grid_info.origin_x), window.proj))
	G_fatal_error("Invalid easting!");;
    if(!G_scan_northing(coord->answers[1], &(grid_info.origin_y), window.proj))
	G_fatal_error("Invalid northing!");;

    if(!G_scan_resolution(box->answers[0], &(grid_info.length), window.proj))
	G_fatal_error("Invalid distance!");;
    if(!G_scan_resolution(box->answers[1], &(grid_info.width), window.proj))
	G_fatal_error("Invalid distance!");;

  }

  /*
   * vector rows are the actual number of rows of vectors to make up the
   * entire grid.   ditto for cols.
   */
  grid_info.num_vect_rows = grid_info.num_rows + 1;
  grid_info.num_vect_cols = grid_info.num_cols + 1;

  Points = Vect_new_line_struct ();
  Cats = Vect_new_cats_struct ();
  db_init_string (&sql);

  /* Open output map */
  if (0 > Vect_open_new (&Map, dig_file, 0))
  {
    G_fatal_error ( "Cannot open vector output file <%s>\n", dig_file);
  }

  /* Open database, create table */
  Fi = Vect_default_field_info ( &Map, 1, NULL, GV_1TABLE );
  Vect_map_add_dblink ( &Map, Fi->number, Fi->name, Fi->table, Fi->key, Fi->database, Fi->driver);
  
  Driver = db_start_driver_open_database ( Fi->driver, Vect_subst_var(Fi->database,&Map) );
  if (Driver == NULL)
    G_fatal_error("Cannot open database %s by driver %s", Fi->database, Fi->driver);

  if ( grid_info.num_rows < 27 && grid_info.num_cols < 27 ) {
      sprintf ( buf, "create table %s ( cat integer, row integer, col integer, "
	                                "rown varchar(1), coln varchar(1))", Fi->table );
  } else {
      sprintf ( buf, "create table %s ( cat integer, row integer, col integer)", Fi->table );
  }
  db_set_string ( &sql, buf);

  G_debug ( 1, "SQL: %s", db_get_string(&sql) );
  
  if (db_execute_immediate (Driver, &sql) != DB_OK ) {
    G_fatal_error ( "Cannot create table: %s", db_get_string ( &sql ) );
  }

  if (db_grant_on_table (Driver, Fi->table, DB_PRIV_SELECT, DB_GROUP|DB_PUBLIC ) != DB_OK )
      G_fatal_error ( "Cannot grant privileges on table %s", Fi->table );

  if (!q->answer)
    fprintf ( stderr, "Creating vector grid ...\n" );
  
  write_grid (&grid_info, &Map, q->answer);

  /* Create a grid of label points at the centres of the grid cells */
  if (!q->answer)
    fprintf ( stderr, "Creating centroids ...\n" );
  
  /* Write out centroids and attributes */
  attCount = 0;
  for( i = 0; i < grid_info.num_rows; ++i ) {
      for( j = 0; j < grid_info.num_cols; ++j ) {
	  double x, y;

	  x = grid_info.origin_x + ( 0.5 + j ) * grid_info.length;
	  y = grid_info.origin_y + ( 0.5 + i ) * grid_info.width;

	  rotate ( &x, &y, grid_info.origin_x, grid_info.origin_y, grid_info.angle );
	  
	  Vect_reset_line ( Points );
	  Vect_reset_cats ( Cats );

	  Vect_append_point ( Points, x, y, 0.0 );
	  Vect_cat_set ( Cats, 1, attCount+1 );
	  Vect_write_line ( &Map, GV_CENTROID, Points, Cats );

          if ( grid_info.num_rows < 27 && grid_info.num_cols < 27 ) {
	      sprintf ( buf, "insert into %s values ( %d, %d, %d, '%c', '%c' )", Fi->table, attCount+1, 
 	                            grid_info.num_rows-i, j+1, 'A'+grid_info.num_rows-i-1, 'A'+j );
	  } else { 
	      sprintf ( buf, "insert into %s values ( %d, %d, %d )", Fi->table, attCount+1, i+1, j+1);
	  }
	  db_set_string ( &sql, buf);

          G_debug ( 3, "SQL: %s", db_get_string(&sql) );

	  if (db_execute_immediate (Driver, &sql) != DB_OK ) {
	      G_fatal_error ( "Cannot insert row: %s", db_get_string ( &sql ) );
	  }
	  attCount++;
      }
  }
  
  db_close_database_shutdown_driver(Driver);
  
  Vect_build (&Map, stderr);
  Vect_close (&Map);

  exit(0);
}

