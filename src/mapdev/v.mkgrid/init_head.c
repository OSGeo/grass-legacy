
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

int 
init_header (struct Cell_head *window, struct dig_head *d_head)
{
  char *date;
  char *organization;

  /* CALCULATE TODAY'S DATE */
  date = G_date ();

  /* DETERMINE USER'S NAME */
  /* name = G_whoami (); */

  if (getenv("GRASS_ORGANIZATION"))  /* added MN 5/2001 */
  {
    organization=(char *)getenv("GRASS_ORGANIZATION");
    sprintf(d_head->organization, "%s", organization);
  }
  else
    strcpy(d_head->organization, "GRASS Development Team");
  strcpy (d_head->date, date);
  strcpy (d_head->your_name, "v.mkgrid");
  strcpy (d_head->map_name, "");
  strcpy (d_head->source_date, "");
  strcpy (d_head->line_3, "");
  d_head->orig_scale = 0;
  d_head->plani_zone = 0;
  d_head->digit_thresh = 0.0;
  d_head->map_thresh = 0.0;

  /* load default window settings into Vect.header  */
  d_head->W = window->west;
  d_head->E = window->east;
  d_head->S = window->south;
  d_head->N = window->north;

  /* superceded by Vect_copy_head_data in calling prog */
  /* dig_write_head_binary( fp_digit, d_head) ; */

  return 0;
}
