#include"gis.h"

int writesites2 (name, x,y,z,nx,ny,verbose)
  char *name;
  float *x,*y,*z;
  int nx,ny,verbose;
{
  int i,j;
  FILE *fd=NULL;
  char errmsg[256];
 
  fd = G_fopen_sites_new (name);
  if (fd == NULL)
  {
    sprintf (errmsg, "%s can't create sites file [%s]",
             G_program_name (), name);
    G_fatal_error (errmsg);
  }
  if (verbose)
    fprintf (stderr, "Writing sites list ...              ");
 
  fprintf (fd, "name|%s\n", name);
  fprintf (fd, "desc|output of %s\n", G_program_name());
  for (j = 0; j < nx; ++j)
  {
    for (i = 0; i < ny; ++i)
    {
      sprintf (errmsg, "%g", z[j+i*(ny+1)]);
      G_put_site (fd, x[j], y[i], errmsg);
    }
    if (verbose)
      G_percent (j, nx, 1);
  }
  fclose(fd);
  if (verbose)
    G_percent (1, 1, 1);
  return 1;
}
