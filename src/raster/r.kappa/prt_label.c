#include "kappa.h"

prt_label()
{
  int i,j;
  long *cats;
  char *cl;
  FILE *fd;

  if ((fd = fopen (output, "a")) != NULL) {
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
    fclose(fd);
  }
  else
    fprintf (stderr, "ERROR: can't open file <%s> to write header\n", 
	output);
}
