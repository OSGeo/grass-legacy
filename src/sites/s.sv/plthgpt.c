#include <stdio.h>
#include <string.h>
#include "gis.h"
#include "sv.h"

int 
plot_hg_points (HGN *list, int nh, int verbose, char *save)
/*
 * Plotting function. Creates data file and {\tt [g.]gnuplot} instruction file
 * for plotting a sample semivariogram.
 */
{
  char *tmp_file_name, buf[1024], **rbuf, sbuf[128];
  char command[1024];
  char *file;
  extern char *plot_file, *data_file;
  int i, n;
  FILE *tfp;

  data_file = G_tempfile ();
  if ((tfp = fopen (data_file, "w")) == NULL)
    G_fatal_error ("Unable to open the temporary file.");
  fprintf (tfp, "#h g N(n) \n");
  for(i=0;i<nh;++i)
     if (list[i].g > 0)
        fprintf(tfp, "%g %g %d\n", list[i].h, list[i].g, list[i].n);
  fclose(tfp);

  plot_file = G_tempfile ();
  if ((tfp = fopen (plot_file, "w")) == NULL)
    G_fatal_error ("Unable to open the temporary file.");

  rbuf = cdpath (data_file, &n, "cd '", "'");
  for (i = 0; i < n - 1; ++i)
    fprintf (tfp, "%s\n", rbuf[i]);
  fprintf (tfp, "set size 1,0.9\n");
  fprintf (tfp, "set title 'Sample Semivariogram'\n");
  fprintf (tfp, "set nokey\n");
  fprintf (tfp, "set xlabel 'lag (h)'\n");
#ifdef DUMB
  fprintf (tfp, "set term dumb\n");
#endif				/* DUMB */

  fprintf (tfp, "plot %s using 1:2 title \"\" with %s\n",
	   rbuf[n - 1], ((nh > 100) ? "dots" : "points" ) );
  sprintf (command, "plot '%s' using 1:2 title %c%c with points%s",
	   "%s", 042, 042, "\n");
#ifdef PAUSE
  fprintf (tfp, "pause -1  '%s'\n", RETURN);
#endif				/* PAUSE */
  fclose (tfp);		/* very important */

  strcat (buf, sbuf);
  sprintf (buf, "%s %s ", plot_program, plot_file);
  G_system (buf);

  /* save plot if requested */
  if (verbose && save)
  {
    G_percent (1, 1, 1);
    fprintf (stderr, "Saving plot files ...               ");
  }
  if (save)
    save_plot (command, save);

  return 0;
}
