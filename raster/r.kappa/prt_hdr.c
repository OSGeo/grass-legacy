#include <string.h>
#include "kappa.h"

prn_header()
{
  int i,len;
  char buf[1024], *titles, *label;
  char *mask, *maskinfo();
  FILE *fd;

  if (output == NULL)
    fd = stdout;
  else
    if ((fd = fopen (output, "w")) == NULL) {
      fprintf (stderr, "ERROR: can't open file <%s> to write header\n",
	      output);
      return;
    }

/* print header */
  fprintf (fd, "\t\t\t%s\n", title);
  sprintf (buf, "LOCATION: %s\t\t\t\t%s", G_location(), G_date());
  fprintf (fd, "%s\n", buf);
  if ((mask = maskinfo()))
    sprintf (buf, "MASK: %s", mask);
  fprintf (fd, "%s\n", buf);
  fprintf (fd, "MAPS: ");
  label = "MAP";
  len = strlen (label);
  for (i=0; i<nlayers; i++) {
    titles = G_get_cats_title (&(layers[i].labels));
    if (titles) G_strip (titles);
    if (titles == NULL || *titles == 0)
      titles = "(untitled)";
    sprintf(buf, "%*s%-*s%d = %s (%s in %s)", i*6, "", len, label, i+1,
	titles, layers[i].name, layers[i].mapset);
    fprintf (fd, "%s\n", buf);
  }
  if (output != NULL)
    fclose(fd);
}
