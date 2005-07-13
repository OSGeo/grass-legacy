#include "kappa.h"

int
prt_label(void)
{
  int i,j;
  long *cats;
  char *cl;
  FILE *fd;

  if (output == NULL)
    fd = stdout;
  else
    if ((fd = fopen (output, "a")) == NULL) {
      fprintf (stderr, "ERROR: can't open file <%s> to write label\n",
	      output);
      return;
    }

/* print labels */
  for (i=0; i<nlayers; i++) {
    fprintf (fd, "\n");
    fprintf (fd, "MAP%-d Category Description\n", i+1);
    for (j=0; j<ncat; j++) {
      cats = rlst;
      cl = G_get_cat((CELL) cats[j], &(layers[i].labels));
      if (cl) G_strip (cl);
      if (cl == NULL || *cl == 0)
        cl = "(no description)";
      fprintf (fd, "%d:  %s\n", rlst[j], cl);
    }
  }
  if (output != NULL)
    fclose(fd);
}
