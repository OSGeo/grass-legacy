#include "globals.h"

/* open and allocate space for the subgroup band files */
open_band_files()
{
  int n, nbands;
  char *name, *mapset;
  extern int numfiles;
  extern char *lname[20], *Gmapset[20];

  /* allocate row buffers and open cell files */
  nbands = numfiles;
  Bandbuf = (CELL **) G_malloc (nbands * sizeof (CELL *));
  Bandfd = (int *) G_malloc (nbands * sizeof (int));
  for (n=0; n < nbands; n++)
    {
      Bandbuf[n] = G_allocate_cell_buf();
      name   = lname[n];
      mapset = Gmapset[n];
      if ((Bandfd[n] = G_open_cell_old (name, mapset)) < 0)
	G_fatal_error("Unable to open band files.\n");
    }
}


/* close and free space for the subgroup band files */
close_band_files()
{
  int n, nbands;
  extern int numfiles;

  nbands = numfiles;
  for (n=0; n < nbands; n++)
    {
      free(Bandbuf[n]);
      G_close_cell(Bandfd[n]);
    }
  free(Bandbuf);
  free(Bandfd);
}
