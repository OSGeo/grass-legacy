#include "gis.h"
#include "methods.h"

/*
 * For small raster files it may be quicker to load
 * the whole file and then operate solely from memory.
 */


static int		raster_rows	= 0;


CELL** loadraster (fd , verbose)
int		fd;
int		verbose;
{
  int		indx;
  CELL		**raster;

  raster_rows	= G_window_rows ();
  raster		= (CELL **) G_calloc (raster_rows , sizeof(CELL *));

  if (raster == NULL)
    G_fatal_error ("could not allocate memory for raster");

  G_sleep_on_error (0);


  if (verbose)
	(void) fprintf (stderr, "Allocating memory for raster ...    ");

  for (indx = 0 ; indx < raster_rows ; indx++)
  {
    raster[indx]	= G_allocate_cell_buf ();

    if (verbose)
      G_percent (indx , raster_rows - 1 , 1);
  }


  if (verbose)
    (void) fprintf (stderr, "Loading raster to memory ...        ");

  for (indx = 0 ; indx < raster_rows ; indx++)
  {
    if (G_get_map_row (fd , raster[indx], indx) < 0)
      G_fatal_error ("Problem reading cell file");

    if (verbose)
      G_percent(indx , raster_rows - 1 , 1);
  }

  G_sleep_on_error (1);

  return raster;
}


void dropraster (raster)
CELL	**raster;
{
  int		indx;

  for (indx = 0 ; indx < raster_rows ; indx++)
    (void)free (raster[indx]);
  (void)free (raster);
}
