#include <stdio.h>
#include "gis.h"
#include "semivar.h"
#include "graphics.h"

/* int plot_crst (HGN_LIST *list) */

int plot_crst (list, parameters, nwork, verbose, save)
  HGN_LIST *list;

/*
 * Plotting function. Creates data file and {\tt [g.]gnuplot} instruction
 * file for plotting the working list with a the radial basis function
 * used by s.surf.2d.
 */
{
  char *tmp_file_name, buf[1024], **rbuf, sbuf[128];
  char command[1024];
  char *file;
  int i, n;
  FILE *tmp_file;
  extern char *plot_file, *data_file;
  extern int nwork;
  extern PARAM parameters;

  graph_window (); /* Select graph window */

  file = hgnwrite (list, NOTRIM, 0);

  tmp_file_name = G_tempfile ();
  if ((tmp_file = fopen (tmp_file_name, "w")) == NULL)
    G_fatal_error ("Unable to open the temporary file.");

  rbuf = cdpath (file, &n, "cd '", "'");
  for (i = 0; i < n - 1; ++i)
    fprintf (tmp_file, "%s\n", rbuf[i]);
  fprintf (tmp_file, "set size 1,0.9\n");
  fprintf (tmp_file, "set title 'RBF of Mitas & Mitasova'\n");
  fprintf (tmp_file, "set nokey\n");
  fprintf (tmp_file, "set xlabel 'lag (h)'\n");
#ifndef GRAPHICS
  fprintf (tmp_file, "set term dumb\n");
#endif				/* GRAPHICS */

  fprintf (tmp_file, "set zero 1.e-99\n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "c0=8.5733287401\n");
  fprintf (tmp_file, "c1=18.0590169730\n");
  fprintf (tmp_file, "c2=8.6347608925\n");
  fprintf (tmp_file, "c3=0.2677737343\n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "b0=9.5733223454\n");
  fprintf (tmp_file, "b1=25.6329561486\n");
  fprintf (tmp_file, "b2=21.0996530827\n");
  fprintf (tmp_file, "b3=3.9584969228\n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "ce=0.57721566\n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "u0=1.\n");
  fprintf (tmp_file, "u1=-.25\n");
  fprintf (tmp_file, "u2=0.055555555555556e+00\n");
  fprintf (tmp_file, "u3=-.010415555555556e+00\n");
  fprintf (tmp_file, "u4=0.166666666666667e-02\n");
  fprintf (tmp_file, "u5=-2.31481481481482e-04\n");
  fprintf (tmp_file, "u6=2.83446712018141e-05\n");
  fprintf (tmp_file, "u7=-3.10019841269841e-06\n");
  fprintf (tmp_file, "u8=3.06192435822065e-07 \n");
  fprintf (tmp_file, "u9=-2.75573192239859e-08\n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "e1(r) = (c3 + r * (c2 + r * (c1 \\\n");
  fprintf (tmp_file, "         + r * (c0 + r))))/(b3 \\\n");
  fprintf (tmp_file, "         + r * (b2 + r * (b1 \\\n");
  fprintf (tmp_file, "         + r * (b0 + r)))) / (r * exp (r))\n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "res3(r) = e1(r) + ce + log (r)\n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "res2(r) = (r > 25.e+00) ?  ce+log(r) : res3(r) \n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "res1(r) = r * (u0 + r * (u1 \\\n");
  fprintf (tmp_file, "         + r * (u2 + r * (u3 + r * (u4 \\\n");
  fprintf (tmp_file, "         + r * (u5 + r * (u6 \\\n");
  fprintf (tmp_file, "         + r * (u7 + r * (u8 \\\n");
  fprintf (tmp_file, "         + r * u9)))))))))\n");
  fprintf (tmp_file, "\n");
  fprintf (tmp_file, "f(t,x) = (x*x*t*t*0.25 < 1.e+00) ?  \\\n");
  fprintf (tmp_file, "         res1(t*t*x*x*0.25) : \\\n");
  fprintf (tmp_file, "         res2(x*x*t*t*0.25) \n");
  fprintf (tmp_file, "plot %s using 1:2 with points 2 1, %g+f(%g,x)\n",
	   rbuf[n - 1], parameters.c0, parameters.c1);
  sprintf (command,
	   "plot '%s' using 1:2 with points 2 1, %g+f(%g,x)%s",
	   "%s", parameters.c0, parameters.c1, "\n");
#ifdef PAUSE
  fprintf (tmp_file, "pause -1  '%s'\n", RETURN);
#endif				/* PAUSE */
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

  /* save plot if requested */
  save_plot (command);
}
