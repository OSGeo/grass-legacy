/*
 * Written by GRASS, Fall of 88,  Michael H. Updated <02 Jun 1992> by Darrell
 * McCauley <mccauley@ecn.purdue.edu> angle option added.
 */
#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "grid_structs.h"

/**  data directories   **/
#define		B_DIG		"dig"

double atof ();
static char *PROG;

main (argc, argv)
  int argc;
  char *argv[];
{

  /* store filename and path  */
  char *dig_file;
  char buffer[128];
  char errmsg[200];

  FILE *fp_digit, *fopen ();

  struct grid_description grid_info;
  struct Cell_head window;
  struct dig_head d_head;
  struct Map_info Map;
  struct Option *vectname, *grid, *coord, *box, *angle;
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
  box->key_desc = "length,width";
  box->type = TYPE_DOUBLE;
  box->required = YES;
  box->multiple = NO;
  box->description = "LENGTH and WIDTH of boxes in grid";

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

  PROG = argv[0];
  G_gisinit (argv[0]);
  setbuf (stdout, NULL);

  /* get the current window  */
  G_get_window (&window);

  if (G_parser (argc, argv))
    exit (-1);

  /* make sure dig directory is there  */
  G__make_mapset_element (B_DIG);

  /*
   * information we need to collect from user: origin point x and y (lower
   * left), shift in x, shift in y,  number of rows, number of cols
   */
  dig_file = G_store (vectname->answer);

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

  /* if ( (fp_digit = G_fopen_vector_new(dig_file))  ==  NULL) */
  if (0 > Vect_open_new (&Map, dig_file))
  {
    sprintf (errmsg, " %s: Cannot open vector output file <%s>\n", PROG, dig_file);
    G_fatal_error (errmsg);
  }

  grid_info.num_rows = atoi (grid->answers[0]);
  grid_info.num_cols = atoi (grid->answers[1]);

  grid_info.origin_x = atof (coord->answers[0]);
  grid_info.origin_y = atof (coord->answers[1]);

  grid_info.length = atof (box->answers[0]);
  grid_info.width = atof (box->answers[1]);

  grid_info.angle = 3.1415927 / 180 * atof (angle->answer);

  /*
   * vector rows are the actual number of rows of vectors to make up the
   * entire grid.   ditto for cols.
   */
  grid_info.num_vect_rows = grid_info.num_rows + 1;
  grid_info.num_vect_cols = grid_info.num_cols + 1;

  /* initialize and write the digit vector header  */
  if (!q->answer)
    printf ("\n Creating vector header...");
  init_header (fp_digit, &window, &d_head);
  Vect_copy_head_data (&d_head, &Map.head);

  if (!q->answer)
    printf ("\n Creating vector grid...");
  write_grid (fp_digit, &grid_info, &Map, q->answer);
  if (!q->answer)
    printf ("\n Finished\n\n");

  Vect_close (&Map);
}				/* main()  */
