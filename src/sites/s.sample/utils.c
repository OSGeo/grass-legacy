#include "gis.h"

FILE *opensites (basename, i, ext)
  char *basename, *ext;
  int i;

{
  FILE *fd;
  char filename[1024], errmsg[256];

  sprintf (filename, "%s-%s.%d", basename, ext, i);
  fd = G_fopen_sites_new (filename);
  if (fd == NULL)
  {
    sprintf (errmsg, "%s can't create sites file [%s]",
	     G_program_name (), filename);
    G_fatal_error (errmsg);
  }
  fprintf (fd, "name|%s\n", filename);
  fprintf (fd, "desc|%s partition\n", ext);
  return fd;
}


double scancatlabel(str)
 char *str;
{
  double val;

  if (strcmp(str,"no data") != 0)
    sscanf(str, "%lf", &val);
  else
  {
    G_warning("\"no data\" label found; setting to zero");
    val=0.0;
  }

  return val;
}

