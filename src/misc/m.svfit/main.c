/*-m.svfit
**
** Author: James Darrell McCauley
**         McCauley Technical Services
**         PO Box 2485
**         West Lafayette, Indiana 47906-0485 USA
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted. This
** software is provided "as is" without express or implied warranty.
**
** Modification History:
** 0.1B <17 Oct 1994> pieced together first version from s.semivar -jdm
** 0.2B <18 Oct 1994> ditched MESCHACH and used G_svdcmp() for least
**                    squares regression (solve.c, Gmakefile). -jdm
** 0.3B <19 Oct 1994> dparam.c: give warning for negative nuggets.
**                    main.c: add more chatter.
**                    main.c, m.svfit.man: make weighted LS optional (-w) -jdm
** 0.4B <20 Oct 1994> premodel.c: bug with quadratic (used g instead of h)
**                    pltquad.c: doesn't stop at range now
**                    plthole.c: doesn't stop at range now
**
**/

#include <string.h>
#include <math.h>
#include "gis.h"
#include "svfit.h"

char *plot_file, *data_file;

int main (argc, argv)
  char **argv;
  int argc;
{
  char *mapset, *input, *graphfile, errmsg[200];
  int i, j, nh, nwork, in_range;
  int verbose, plot, weighted, model;
  double range;
  int m, n;
  double **a, *b, *x;
  PARAM parameters;
  HGN *list;
  FILE *fd;
  struct GModule *module;
  struct
  {
    struct Flag *q, *p, *w;
  } flag;
  struct
  {
    struct Option *input, *model, *range, *save;
  } parm;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =
	"Semivariogram model fitting.";

  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = NO;
  parm.input->description = "unix file containing sample semivariogram";
  parm.input->gisprompt = "input";

  parm.model = G_define_option ();
  parm.model->key = "model";
  parm.model->type = TYPE_INTEGER;
  parm.model->required = YES;
  /* make sure this jives with svfit.h */
  parm.model->description = "Integer Model Index:\n\
          1 Linear\n\
          2 Spherical\n\
          3 Exponential\n\
          4 Gaussian\n\
          5 Quadratic\n\
          6 Wave or Hole Effect";
  parm.model->options = "1-6";

  parm.range = G_define_option ();
  parm.range->key = "range";
  parm.range->type = TYPE_DOUBLE;
  parm.range->required = YES;
  parm.range->description = "range of semivariogram";

  parm.save = G_define_option ();
  parm.save->key = "graph";
  parm.save->type = TYPE_STRING;
  parm.save->required = NO;
  parm.save->description = "basename of a graphing data/commands files\n\
          (implies -p)";

  flag.p = G_define_flag ();
  flag.p->key = 'p';
  flag.p->description = "Plot model & sample semivariogram";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  flag.w = G_define_flag ();
  flag.w->key = 'w';
  flag.w->description = "Use weighted least squares";

  if (G_parser (argc, argv))
    exit (1);
  G_sleep_on_error (0);

  /* Process arguments */
  verbose = (!flag.q->answer);
  weighted = flag.w->answer;

  if (input = parm.input->answer)
  {
    if ((fd = fopen (input, "r")) == NULL)
    {
      fprintf (stderr, "%s - ", G_program_name ());
      perror (input);
      exit (1);
    }
  }
  else
    fd = stdin;

  if ((i = sscanf (parm.model->answer, "%d", &model)) != 1)
    G_fatal_error ("error scanning model");
  if ((i = sscanf (parm.range->answer, "%lf", &range)) != 1)
    G_fatal_error ("error scanning range");
  if (range <= 0 )
    G_fatal_error ("cannot have negative or zero range");

  plot = (flag.p->answer);
  graphfile = parm.save->answer;
  /* need graphics. program will exit here if driver is not available */
  if (parm.save->answer)
  {
    plot = 1;
    R_open_driver ();
    R_close_driver ();
  }

  nh = 100;
  in_range = nwork = 0;
  if ((list = (HGN *) G_malloc (nh * sizeof (HGN))) == NULL)
    G_fatal_error ("No memory");
  if(isatty(fileno(fd)))
  {
    printf ("Enter semivariogram points, one per line, in the format: h gamma N(h)\n");
    printf ("When finished, type: Control-D\n");
  }

  while ((j = fscanf (fd, "%lf %lf %d", &list[nwork].h,
		      &list[nwork].g, &list[nwork].n)) != EOF)
  {
    if (list[nwork].h <= range)
      in_range++;
    if (nwork == nh - 1)
    {
      nh += 100;
      if ((list = (HGN *) G_realloc (list, nh * sizeof (HGN))) == NULL)
	G_fatal_error ("No more memory");
    }
    if (j != 3)
      G_warning ("problem scanning input; line ignored");
    else
      nwork++;
  }
  if (nwork==0)
  {
    G_usage();
    exit(1);
  }
  else if (verbose)
    fprintf (stderr, "%d samples found, %d below range\n",nwork, in_range);

  a = (double **) G_malloc (nwork * sizeof (double *));
  b = (double *) G_malloc (nwork * sizeof (double));
  x = (double *) G_malloc (PMAX * sizeof (double));
  if (a == NULL || x == NULL || b == NULL)
    G_fatal_error ("Memory allocation error ");
  for (i = 0; i < nwork; ++i)
  {
    a[i] = (double *) G_malloc (PMAX * sizeof (double));
    if (a[i] == NULL)
      G_fatal_error ("Memory allocation error 2 (domodel.c)");
  }

  parameters = pre_model (a, &m, &n, b, list, range, i, model, weighted);
  parameters.estimator = 1;

  if (verbose)
  {
    if (weighted)
      fprintf (stderr, "Weighted least squares reg ...      ");
    else
      fprintf (stderr, "Least squares regression ...        ");
  }
  if (m > n)
    x = solvex (a, n, m, b);
  else
    G_fatal_error ("Not enough points - range too small");
  if (verbose)
    G_percent (1, 1, 1);

  update_parameters (x, &parameters);
  if (verbose)
    display_parameters (parameters);
  if (plot)
    plot_model (list, parameters, nwork, graphfile, verbose);

  for (i = 0; i < nwork; ++i)
    free (a[i]);
  free (a);
  free (b);
  free (x);
  free (list);
  exit(0);
}
