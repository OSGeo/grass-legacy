/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include <stdio.h>
#include <stdlib.h>
#include <unistd.h>
#include <math.h>
#include "gis.h"
#include "probplot.h"

int pltsqq(z, width, log, save, verbose)
  char * save;
  int log, verbose;
  double width, z[];

/*
 * Plotting function. Creates data file and {\tt [g.]gnuplot} instruction
 * file for plotting a normal probility plot of sample values in {\tt z}
 * with a class width of {\tt width}. Assumes that {\tt z} is sorted in
 * ascending order. If {\tt log} is non-zero, a lognormal
 * Q-Q plot will be produced instead of a Gaussian Q-Q plot.
 */
{
  char *tmp_plot_file, *tmp_data_file, buf[1024], **rbuf;
  char command[1024];
  extern char *plot_file, *data_file;
  extern int nsites;
  int i, k = 0 /* number of points plotted */ , n, freq;
  double cwidth = 0;		/* current class */
  double tmp, p;
  FILE *tfp;

  /* open a data file */
  tmp_data_file = G_tempfile ();
  if ((tfp = fopen (tmp_data_file, "w")) == NULL)
    G_fatal_error ("Unable to open the temporary file.");
  fprintf (tfp, "# X z p cumulative nsites \n");

  /* Advance class to low end of range of data */
  cwidth = width * floor (z[0] / width);

  /* Compute histogram and write to file */
  freq = i = 0;
  while (i < nsites)
  {
    while (z[i] < cwidth && i < nsites)
    {
      i++;
      freq++;
    }
    if (freq != 0)
    {
      k++;
      if (i == nsites)
	tmp = (tmp > 0.99) ? (tmp+1.0)/2.0 : 0.99;
      else
	tmp = ((double) freq) / nsites;
      fprintf (tfp, "%g %g %g %d %d\n", cwidth - width,
	       inverse_normal (tmp), tmp, freq, nsites);
    }
    cwidth += width;
  }
  fclose (tfp);

  if (k == 0)
  {
    G_warning ("No data to plot");
    return -1;
  }

  /* Open file for plotting instructions */
  tmp_plot_file = G_tempfile ();
  if ((tfp = fopen (tmp_plot_file, "w")) == NULL)
    G_fatal_error ("Unable to open the temporary file.");

  /*
   * Kludge to change directory to $LOCATION/.tmp/uname, that is, 'cd
   * tmp_data_file/..'
   */
  rbuf = cdpath (tmp_data_file, &n, "cd '", "'");
  for (i = 0; i < n - 1; ++i)
    fprintf (tfp, "%s\n", rbuf[i]);

  /* Write plotting instructions to a file */
  /* fprintf (tfp, "set boxwidth %g\n", width); */
  fprintf (tfp, "set size 1,0.9\n");
  fprintf (tfp, "set grid \n");
  fprintf (tfp, "set title '%s Probability Plot'\n",
	   ((log == 0) ? "Lognormal" : "Normal"));
  fprintf (tfp, "set nozeroaxis\n");
  fprintf (tfp, "set yrange [-3.71899:3.71899]\n");
  fprintf (tfp, "set ytics (\"1\" -2.32647, \"2\" -2.05386, \\\n");
  fprintf (tfp, "  \"5\" -1.64494, \"10\" -1.28165, \\\n");
  fprintf (tfp, "  \"20\" -0.841735, \"50\" 0, \\\n");
  fprintf (tfp, "  \"80\" 0.841735, \"90\" 1.28165, \\\n");
  fprintf (tfp, "  \"95\" 1.64494, \\\n");
  fprintf (tfp, "  \"98\" 2.05386, \"99\" 2.32647)\n");
  fprintf (tfp, "set nokey\n");
  fprintf (tfp, "set xlabel '%s'\n", (log==0) ? "Z" : "log(Z)");
  fprintf (tfp, "set ylabel 'Cum Freq (%%)'\n");
  if (log)
    fprintf (tfp, "set log x\n");
#ifdef DUMB
  fprintf (tfp, "set term dumb\n");
#endif				/* DUMB */

  fprintf (tfp, "plot %s title \"\" with %s 2 8\n",
	   rbuf[n - 1], ((k > 50) ? "dots" : "linespoints"));
  sprintf (command, "plot '%s' title %c%c with %s 2 8%s",
	   "%s", 042, 042, ((k > 50) ? "dots" : "linespoints"), "\n");

#ifdef PAUSE
  fprintf (tfp, "pause -1  '%s'\n", RETURN);
#endif				/* PAUSE */
  fclose (tfp);

  sprintf (buf, "%s %s ", PLOTPROG, tmp_plot_file);
  G_system (buf);

  /* get rid of the previous temporary files */
  unlink (plot_file);
  unlink (data_file);

  /*
   * Save the names of the data file and instruction file so that these
   * can be recovered later.
   */
  plot_file = tmp_plot_file;
  data_file = tmp_data_file;

  /* save plot if requested */
  if (verbose && save)
  {
    G_percent (1, 1, 1);
    fprintf (stderr, "Saving plot files ...               ");
  }
  save_plot (command, save);

  return 1;
}
