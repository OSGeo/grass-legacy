#include<stdio.h>
#include "gis.h"
#include "svfit.h"

/* char *hgnwrite (HGN *, int) */

char *hgnwrite (list, nwork)
  int nwork;
  HGN *list;
/*
 * Writes working list to a temporary file in the form
 * $$\magnitude{\vec{h}}\:\gamma\:N\left(\vec{h}\right).$$ 
 * A pointer to a char
 * array, containing the name of the temporary file, is returned.
 */
{
  char errmsg[256], *tmpfilename;
  int i,npoints=0;
  FILE *tmpfileptr;

  /* Write data to file */
  tmpfilename = G_tempfile ();
  if ((tmpfileptr = fopen (tmpfilename, "w")) == NULL)
  {
    sprintf (errmsg, "Failed to open temp file (%s)", tmpfile);
    G_fatal_error (errmsg);
  }

  for (i = 0; i < nwork; ++i)
  {
    fprintf (tmpfileptr, "%f %f %d\n", list[i].h, list[i].g, list[i].n);
    npoints++;
  }

  fclose (tmpfileptr);
  return (npoints==0)? NULL : tmpfilename;
}

