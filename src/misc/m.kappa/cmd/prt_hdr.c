#include "mkappa.h"

prn_header()
{
  FILE *fd;

  if (output == NULL)
    fd = stdout;
  else
    if ((fd = fopen(output, "w")) == NULL) {
      fprintf (stderr,
	"ERROR: can't open <%s> to write header information\n", output);
      exit(1);
    }

  fprintf (fd, "\n\t\t%s\n", title);
  fprintf (fd, "\t\t\t\t(%s)\n\n", G_date());

  if (output != NULL)
    fclose(fd);
}
