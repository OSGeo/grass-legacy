#include <stdio.h>
#include "gis.h"
#include "Vect.h"

init_header (fp_digit, window, d_head)
  FILE *fp_digit;
  struct Cell_head *window;
  struct dig_head *d_head;
{
  char *date;
  char *name;

  /* CALCULATE TODAY'S DATE */
  date = G_date ();

  /* DETERMINE USER'S NAME */
  name = G_whoami ();

  strcpy (d_head->organization, "US Army Const. Eng. Rsch. Lab");
  strcpy (d_head->date, date);
  strcpy (d_head->your_name, name);
  strcpy (d_head->map_name, "");
  strcpy (d_head->source_date, "");
  strcpy (d_head->line_3, "");
  d_head->orig_scale = 1;
  d_head->plani_zone = 0;
  d_head->digit_thresh = 0.0;
  d_head->map_thresh = 0.0;

  /* load default window settings into digit header  */
  d_head->W = window->west;
  d_head->E = window->east;
  d_head->S = window->south;
  d_head->N = window->north;

}
