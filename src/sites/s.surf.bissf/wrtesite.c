#include"gis.h"

int writesites (name, x,y,z,n,verbose)
  char *name;
  float *x,*y,*z;
  int n,verbose;
{
  int j;
  FILE *fd=NULL;
  char errmsg[256];
 
  fd = G_fopen_sites_new (name);
/*  fd=stderr; */
  if (fd == NULL)
  {
    sprintf (errmsg, "%s can't create sites file [%s]",
             G_program_name (), name);
    G_fatal_error (errmsg);
  }
  if (verbose)
    fprintf (stderr, "Writing sites list ...              ");
 
  fprintf (fd, "name|%s\n",  G_program_name());
  fprintf (fd, "desc|output of %s\n", G_program_name());
  for (j = 0; j < n; ++j)
  {
      sprintf (errmsg, "%g", z[j]);
      G_put_site (fd, x[j], y[j], errmsg);
    if (verbose)
      G_percent (j, n, 1);
  }
  fclose(fd);
  if (verbose)
    G_percent (1, 1, 1);
  return 1;
}
