/*
 * Copyright (C) 1994. James Darrell McCauley.  (darrell@mccauley-usa.com)
 * 	                                        http://mccauley-usa.com/
 *
 * This program is free software under the GPL (>=v2)
 * Read the file GPL.TXT coming with GRASS for details.
 */

#include <unistd.h>
#include "gis.h"
#include "probplot.h"

/* void save_plot (char *command) */

void save_plot ( char *command, char *keep)
/*
 * Saves two files: {\tt [g.]gnuplot} instruction file and the data file.
 * The associated temporary filenames are stored in global variables,
 * which are {\tt plot\_file} and {\tt data\_file}, respectively. This
 * function is called after each plot is made to the graphics monitor.
 */
{
  FILE *old = NULL, *new = NULL;
  char print[1024], plot[1024], data[1024], buf[1024];
  extern char *plot_file, *data_file;

  if (keep == NULL)
  {
    return;
  }
  else
    G_strcpy (print, keep);

  G_strip (print);
  /* make filenames from this basename */
  if (!strcmp (print, ""))
  {
    sprintf (plot, "%s.gp", "plot");
    sprintf (data, "%s.dat", "plot");
  }
  else
  {
    sprintf (plot, "%s.gp", print);
    sprintf (data, "%s.dat", print);
  }

  /* first save the plotting instructions */
  if ((old = fopen (plot_file, "r")) == NULL ||
      (new = fopen (plot, "w")) == NULL)
  {
    G_warning ("Error opening temporary file");
    fclose (old);
    fclose (new);
    return;
  }
  else
  {
    while (fgets (buf, 1024, old) != (char *) NULL)
    {
      if (buf[0] != 'c' || buf[1] != 'd')
      {
	if (buf[0] == 'p' && buf[1] == 'l' && buf[2] == 'o')
	  fprintf (new, command, data, data, data);
	else if (new != NULL)
	  fprintf (new, "%s", buf);
      }
    }
    fclose (old);
    fclose (new);
    unlink (plot_file);
  }

  /* next save the data */
  if ((old = fopen (data_file, "r")) == NULL ||
      (new = fopen (data, "w")) == NULL)
  {
    G_warning ("Error opening temporary file");
    fclose (old);
    fclose (new);
  }
  else
  {
    while (fgets (buf, 1024, old) != NULL)
      fprintf (new, "%s", buf);
    fclose (old);
    fclose (new);
    unlink (data_file);
  }

  /* For good measure */
  unlink (data_file);
  unlink (plot_file);
}
