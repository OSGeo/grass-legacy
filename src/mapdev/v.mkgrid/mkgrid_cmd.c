/*
 * Written by GRASS, Fall of 88,  Michael H. Updated <02 Jun 1992> by Darrell
 * McCauley <mccauley@ecn.purdue.edu> angle option added.
 */
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "dig_atts.h"
#include "grid_structs.h"
#include "local_proto.h"

/**  data directories   **/
#define		B_DIG		"dig"

double atof ();
static char *PROG;

int 
main (int argc, char *argv[])
{

  /* loop */
  int i,j;

  /* store filename and path  */
  char *dig_file;
  char errmsg[200];

  /* array to store central points */
  double *grid_point_x, *grid_point_y;
  int *grid_val;	

  /* Other local variables */
  AttributeType  att_type;
  struct attribute *Att1;
  struct atts_index *attindx;
  struct Categories cats;
  int cval, attCount;
  int nr, nc;

  FILE *fatt;

  struct grid_description grid_info;
  struct Cell_head window;
  struct dig_head d_head;
  struct Map_info Map;
  struct Option *vectname, *grid, *coord, *box, *angle, *type, *attval;
  struct Flag *q;

  vectname = G_define_option ();
  vectname->key = "map";
  vectname->type = TYPE_STRING;
  vectname->required = YES;
  vectname->multiple = NO;
  vectname->gisprompt = "new,dig,vector";
  vectname->description = "name of vector map";

  grid = G_define_option ();
  grid->key = "grid";
  grid->key_desc = "rows,columns";
  grid->type = TYPE_INTEGER;
  grid->required = YES;
  grid->multiple = NO;
  grid->description = "number of ROWS and COLUMNS in grid";

  coord = G_define_option ();
  coord->key = "coordinate";
  coord->key_desc = "x,y";
  coord->type = TYPE_DOUBLE;
  coord->required = YES;
  coord->multiple = NO;
  coord->description = "lower left EASTING and NORTHING coordinates of map";

  box = G_define_option ();
  box->key = "box";
  box->key_desc = "width, height";
  box->type = TYPE_DOUBLE;
  box->required = YES;
  box->multiple = NO;
  box->description = "WIDTH and HEIGHT of boxes in grid";

  angle = G_define_option ();
  angle->key = "angle";
  angle->type = TYPE_DOUBLE;
  angle->required = NO;
  angle->description = "angle of rotation";
  angle->answer = "0";

  type = G_define_option ();
  type->key = "type";
  type->type = TYPE_STRING;
  type->required = NO;
  type->description = "provide attributes of type: const,rows,cols";
  type->answer = "";

  attval = G_define_option ();
  attval->key = "value";
  attval->type = TYPE_INTEGER;
  attval->required = NO;
  attval->description = "angle of rotation";
  attval->answer = "1";

  q = G_define_flag ();
  q->key = 'q';
  q->description = "Quiet; No chatter";
  q->answer = 0;

  PROG = argv[0];
  G_gisinit (argv[0]);
  setbuf (stdout, NULL);

  /* get the current window  */
  G_get_window (&window);

  if (G_parser (argc, argv))
    exit (-1);

  /* make sure dig directory is there  */
  G__make_mapset_element (B_DIG);
  G__make_mapset_element("dig_cats") ; 
  
  /*
   * information we need to collect from user: origin point x and y (lower
   * left), shift in x, shift in y,  number of rows, number of cols
   */
  dig_file = G_store (vectname->answer);

  if( G_find_file( B_DIG, dig_file, G_mapset() ) != NULL ) {
    sprintf( errmsg, "Map `%s' already exists in current mapset.\n", dig_file );
    G_fatal_error(errmsg);
  }

  if (dig_file == NULL)
  {
    fprintf (stderr, "%s: Command line error.\n\n", argv[0]);
    G_usage ();
    exit (-1);
  }

  if (!*dig_file || (G_legal_filename (dig_file) != 1))
  {
    fprintf (stderr, "%s: Command line error. missing or illegal map name.\n\n", argv[0]);
    G_usage ();
    exit (-1);
  }

  /* Set the value of the constant attribute value if appropriate */

  cval = atoi(attval->answer);
  if( proc_const_attribute_value( SET_VAL, &cval ) != 0 ) 
    G_warning( "Unable to set the attribute value %d.\n", cval );

  /* if ( (fp_digit = G_fopen_vector_new(dig_file))  ==  NULL) */
  if (0 > Vect_open_new (&Map, dig_file))
  {
    sprintf (errmsg, " %s: Cannot open vector output file <%s>\n", PROG, dig_file);
    G_fatal_error (errmsg);
  }

  grid_info.num_rows = atoi (grid->answers[0]);
  grid_info.num_cols = atoi (grid->answers[1]);

/*
  grid_info.origin_x = atof (coord->answers[0]);
  grid_info.origin_y = atof (coord->answers[1]);
*/
  if(!G_scan_easting(coord->answers[0], &(grid_info.origin_x), window.proj))
      G_fatal_error("Invalid easting!");;
  if(!G_scan_northing(coord->answers[1], &(grid_info.origin_y), window.proj))
      G_fatal_error("Invalid northing!");;

/*
  grid_info.length = atof (box->answers[0]);
  grid_info.width = atof (box->answers[1]);
*/

  if(!G_scan_resolution(box->answers[0], &(grid_info.length), window.proj))
      G_fatal_error("Invalid distance!");;
  if(!G_scan_resolution(box->answers[1], &(grid_info.width), window.proj))
      G_fatal_error("Invalid distance!");;
  grid_info.angle = 3.1415927 / 180 * atof (angle->answer);

  /*
   * vector rows are the actual number of rows of vectors to make up the
   * entire grid.   ditto for cols.
   */
  grid_info.num_vect_rows = grid_info.num_rows + 1;
  grid_info.num_vect_cols = grid_info.num_cols + 1;

  /* initialize and write the digit vector header  */
  if (!q->answer)
    fprintf (stdout,"\n Creating vector header...");
  init_header (&window, &d_head);
  Vect_copy_head_data (&d_head, &Map.head);

  if (!q->answer)
    fprintf (stdout,"\n Creating vector grid...");
  write_grid (&grid_info, &Map, q->answer);
  if (!q->answer)
    fprintf (stdout,"\n Finished\n\n");

  Vect_close (&Map);

  /* Create a grid of label points at the centres of the grid cells */

  if( strcmp(type->answer, "const" ) == 0 ) att_type = ATT_CONSTANT;
  else if ( strcmp(type->answer, "rows" ) == 0 ) att_type = ATT_ROWS;
  else if ( strcmp(type->answer, "cols" ) == 0 ) att_type = ATT_COLS;
  else att_type = ATT_NONE;

  if( att_type == ATT_NONE ) exit (0);

  nr = grid_info.num_rows;
  nc = grid_info.num_cols;

  grid_point_x = (double *)malloc( nr * nc * sizeof(double) );
  grid_point_y = (double *)malloc( nr * nc * sizeof(double) );
  grid_val = (int *)malloc( nr * nc * sizeof(int) );
  if ( att_type != ATT_CONSTANT )
    G_init_cats (nr*nc, "", &cats);    
  else
    G_init_cats (0, "", &cats);    

  set_grid_area_points( grid_point_x, grid_point_y, &grid_info );

  set_grid_attributes( grid_val, &cats, &grid_info, att_type );


  /* Allocate space for attribute structures */

  Att1 = (struct attribute *)malloc( sizeof(struct attribute) );
  attindx = (struct atts_index *)malloc( sizeof(struct atts_index) );


  /* Initialise attribute index structure and attribute file */

  if( (fatt = G_fopen_new( "dig_att", vectname->answer )) == NULL ) {
    G_warning("Unable to open attributes file. Not writing attributes.\n");
    exit(0);
  }

  /* Write out the attributes */
  attCount = 0;
  for( i = 0; i < grid_info.num_rows; ++i ) {
    for( j = 0; j < grid_info.num_cols; ++j ) {
      Att1->type='A';
      Att1->x = grid_point_x[i*grid_info.num_cols+j];
      Att1->y = grid_point_y[i*grid_info.num_cols+j];
      Att1->cat = grid_val[i*grid_info.num_cols+j];
      Att1->offset = ++attCount;
      write_att_struct( fatt, Att1 );
    }
  }
  if (G_write_vector_cats (vectname->answer, &cats) == -1)
  {
    G_warning("Unable to open category labes file. Not writing category labels.\n");  
    exit(0);
  }
  G_free_cats (&cats); 
  
  if(fatt) fclose(fatt);

  exit(0);
}				/* main()  */
