#include <stdio.h>
#include "gis.h"
#include "svfit.h"

#define PLT_STR "x0(t),f0(x0(t)) 2 2,x1(t),f1(x1(t)) 2 2 "

/* int plot_exponential (HGN *list) */

int plot_exponential (list, parameters, nwork, verbose, save)
  char *save;
  int nwork, verbose;
  HGN *list;
  PARAM parameters;
/*
 * Plotting function. Creates data file and {\tt [g.]gnuplot} instruction
 * file for plotting the working list with a exponential semivariogram
 * model defined in structure {\tt parameters}. Does not check to
 * see if model parameters are given.
 */
{
  char *tmp_file_name, buf[1024], **rbuf, sbuf[128];
  char command[1024];
  char *file;
  int i, n;
  FILE *tmp_file;
  extern char *plot_file, *data_file;

  file = hgnwrite (list, nwork);

  tmp_file_name = G_tempfile ();
  if ((tmp_file = fopen (tmp_file_name, "w")) == NULL)
    G_fatal_error ("Unable to open the temporary file.");

  rbuf = cdpath (file, &n, "cd '", "'");
  for (i = 0; i < n - 1; ++i)
    fprintf (tmp_file, "%s\n", rbuf[i]);
  fprintf (tmp_file, "set size 1,0.9\n");
  fprintf (tmp_file, "set title 'Exponential Semivariogram'\n");
  fprintf (tmp_file, "set nokey\n");
  fprintf (tmp_file, "set xlabel 'lag (h)'\n");
#ifdef DUMB
  fprintf (tmp_file, "set term dumb\n");
#endif				/* DUMB */

  fprintf (tmp_file, "set parametric\n");
  fprintf (tmp_file, "a=0\n");
  fprintf (tmp_file, "b=0.0001\n");
  fprintf (tmp_file, "c=0.0001\n");
  fprintf (tmp_file, "d=%f\n", list[nwork - 1].h);

  fprintf (tmp_file, "x0(t) = a+(b-a)*t\n");
  fprintf (tmp_file, "x1(t) = c+(d-c)*t\n");
  fprintf (tmp_file, "f0(x) = %f*x/b\n", parameters.c0);
  fprintf (tmp_file, "f1(x) = %f+%f*(1.0-exp(-x/%f))\n",
	   parameters.c0, parameters.c1, parameters.range);

  fprintf (tmp_file, "plot [t=0:1] %s using 1:2 with points 2 1, %s\n",
	   rbuf[n - 1], PLT_STR);
  sprintf (command,
	   "plot [t=0:1] '%s' using 1:2 title %c%c with points 2 1, %s%s",
	   "%s", 042, 042, PLT_STR, "\n");
#ifdef DUMB
  fprintf (tmp_file, "pause -1  '%s'\n", RETURN);
#endif				/* DUMB */
  fclose (tmp_file);		/* very important */

  strcat (buf, sbuf);
  sprintf (buf, "%s %s ", PLOTPROG, tmp_file_name);
  G_system (buf);

  /* get rid of the previous temporary files */
  unlink (plot_file);
  unlink (data_file);

  /*
   * Save the names of the data file and instruction file so that these
   * can be recovered later.
   */
  plot_file = tmp_file_name;
  data_file = file;

  if (verbose && save)
    fprintf (stderr, "Saving plot files ...               ");
  if (save)
    save_plot (command, save);
  if (verbose && save)
    G_percent (1, 1, 1);
  return 0;
}
