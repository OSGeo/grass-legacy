#include "mkappa.h"

readin_data()
{
  int i, j;
  FILE *fd;

  if (input == NULL)
    fd = stdin;
  else
    if ((fd = fopen (input, "r")) == NULL) {
      fprintf (stderr, "ERROR: can't open <%s> to read in data\n", input);
      exit();
    }

/* read in data from the input file or stdin */
  if (fscanf (fd, "%d\n", &ncat) != 1)
    G_fatal_error ("ERROR: reading error matrix size");
  if ((mat = (long **) G_malloc (ncat * sizeof (long *))) == NULL)
    G_fatal_error ("ERROR: can't allocate memory");
  else
    for (i=0; i<ncat; ++i)
      if ((mat[i] = (long *) G_malloc (ncat * sizeof (long))) == NULL)
	G_fatal_error ("ERROR: can't allocate memory");

  for (i=0; i<ncat; ++i)
    for (j=0; j<ncat; ++j)
      if (fscanf (fd, "%ld\n", &mat[i][j]) != 1)
	G_fatal_error ("ERROR: read in error matrix data");

  fclose (fd);
}
