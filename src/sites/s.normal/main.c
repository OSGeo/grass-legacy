/*-
 * s.normal - GRASS program for distributional testing.
 * Copyright (C) 1994-1995. James Darrell McCauley.
 *
 * Author: James Darrell McCauley darrell@mccauley-usa.com
 * 	                          http://mccauley-usa.com/
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 *
 * Modification History:
 * <23 Jan 2001> - added field parameter, fixed reading of sites (MN)
 * <27 Aug 1994> - began coding. Adapted cdh.f from statlib (jdm)
 * <30 Sep 1994> - finished alpha version of cdh-c (jdm)
 * <10 Oct 1994> - announced version 0.1B on pasture.ecn.purdue.edu (jdm)
 * <02 Jan 1995> - cleaned man page, added html page (jdm)
 * <25 Feb 1995> - cleaned 'gcc -Wall' warnings (jdm)
 * <21 Jun 1995> - adapted to use new sites API (jdm)
 * <13 Sep 2000> - released under GPL
 *
 */

#pragma ident "s.normal v 0.4B <21 Jun 1995>; Copyright (C) 1994-1995. James Darrell McCauley"

#include <math.h>
#include "gis.h"
#include "cdhc.h"

struct Cell_head window;

int main (int argc, char **argv)
{
  char *isiteslist, errmsg[256], *mapset;
  int i, nsites, verbose, warn_once = 0, readz (), scan_cats ();
  int field, all;
  long x, y;
  extern struct Cell_head window;
  struct GModule *module;
  struct
  {
    struct Option *input, *tests, *dfield;
  } parm;
  struct
  {
    struct Flag *q, *l, *all;
  } flag;
  FILE *fdisite = NULL;
  double *w, *z;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =        
                  "tests for normality for sites.";
  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "sites list defining sample points";
  parm.input->gisprompt = "old,site_lists,sites";

  parm.tests = G_define_option ();
  parm.tests->key = "tests";
  parm.tests->key_desc = "range";
  parm.tests->type = TYPE_STRING;
  parm.tests->multiple = YES;
  parm.tests->required = YES;
  parm.tests->description = "Lists of tests: e.g. 1,3-8,13";

  parm.dfield = G_define_option ();
  parm.dfield->key = "field";
  parm.dfield->type = TYPE_INTEGER;
  parm.dfield->answer = "1";
  parm.dfield->multiple = NO;
  parm.dfield->required = NO;
  parm.dfield->description = "which decimal attribute (if multiple)";

  flag.all = G_define_flag ();
  flag.all->key = 'a';
  flag.all->description = "Use all sites (do not limit to current region)";
      
  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  flag.l = G_define_flag ();
  flag.l->key = 'l';
  flag.l->description = "lognormal";

  if (G_parser (argc, argv))
    exit (1);

  if (parm.tests->answer == NULL)
  {
    G_usage ();
    exit (2);
  }

  isiteslist = parm.input->answer;
  all = flag.all->answer;
  verbose = (flag.q->answer == (char) NULL) ? 1 : 0;
  sscanf(parm.dfield->answer,"%d", &field);

  if (!all)
    G_get_window (&window);   
  else
    G_get_default_window (&window);

  if ((mapset = G_find_file ("site_lists", isiteslist, "")) == NULL)
  {
    sprintf (errmsg, "sites file [%s] not found", isiteslist);
    G_fatal_error (errmsg);
  }
  if ((fdisite = G_fopen_sites_old (isiteslist, mapset)) == NULL)
  {
    sprintf (errmsg, "can't open sites file [%s]", isiteslist);
    G_fatal_error (errmsg);
  }

  if (field < 1)
  {
    sprintf (errmsg, "Decimal attribute field 0 doesn't exist.");
    G_fatal_error (errmsg);
  }
  

  nsites = G_readsites (fdisite, all, verbose, field, &z);

  /* fprintf (stdout,"%g %g ... %g %g\n",z[0],z[1],z[nsites-2],z[nsites-1]); */

  if (verbose)
    fprintf (stdout,"Number of sites: %d\n", nsites);
  if (nsites < 4)
    fprintf (stdout,"WARNING: I'm not so sure of myself for small samples\n");

  if (nsites <= 0)
    G_fatal_error ("No sites found");

  if (flag.l->answer)
  {
    fprintf (stderr, "Doing log transformation\n");
    warn_once = 0;
    for (i = 0; i < nsites; ++i)
    {
      if (z[i] > 1.0e-10)
	z[i] = log10 (z[i]);
      else if (!warn_once)
      {
	G_warning ("Negative or very small sites set to -10.0");
	z[i] = -10.0;
	warn_once = 1;
      }
    }
  }

  for (i = 0; parm.tests->answers[i]; i++)
    if (!scan_cats (parm.tests->answers[i], &x, &y))
    {
      G_usage ();
      exit (1);
    }
  for (i = 0; parm.tests->answers[i]; i++)
  {
    scan_cats (parm.tests->answers[i], &x, &y);
    while (x <= y)
      switch (x++)
      {
      case 1:			/* moments */
	fprintf (stdout, "Moments \\sqrt{b_1} and b_2:");
	w = omnibus_moments (z, nsites);
	fprintf (stdout,"%g %g\n", w[0], w[1]);
	break;
      case 2:			/* geary */
	fprintf (stdout, "Geary's a-statistic & an approx. normal: ");
	w = geary_test (z, nsites);
	fprintf (stdout,"%g %g\n", w[0], w[1]);
	break;
      case 3:			/* extreme deviates */
	fprintf (stdout, "Extreme normal deviates: ");
	w = extreme (z, nsites);
	fprintf (stdout,"%g %g\n", w[0], w[1]);
	break;
      case 4:			/* D'Agostino */
	fprintf (stdout, "D'Agostino's D & an approx. normal: ");
	w = dagostino_d (z, nsites);
	fprintf (stdout,"%g %g\n", w[0], w[1]);
	break;
      case 5:			/* Kuiper */
	fprintf (stdout, "Kuiper's V (regular & modified for normality): ");
	w = kuipers_v (z, nsites);
	fprintf (stdout,"%g %g\n", w[1], w[0]);
	break;
      case 6:			/* Watson */
	fprintf (stdout,
		 "Watson's U^2 (regular & modified for normality): ");
	w = watson_u2 (z, nsites);
	fprintf (stdout,"%g %g\n", w[1], w[0]);
	break;
      case 7:			/* Durbin */
	fprintf (stdout,
		 "Durbin's Exact Test (modified Kolmogorov): ");
	w = durbins_exact (z, nsites);
	fprintf (stdout,"%g\n", w[0]);
	break;
      case 8:			/* Anderson-Darling */
	fprintf (stdout,
	  "Anderson-Darling's A^2 (regular & modified for normality): ");
	w = anderson_darling (z, nsites);
	fprintf (stdout,"%g %g\n", w[1], w[0]);
	break;
      case 9:			/* Cramer-Von Mises */
	fprintf (stdout,
	     "Cramer-Von Mises W^2(regular & modified for normality): ");
	w = cramer_von_mises (z, nsites);
	fprintf (stdout,"%g %g\n", w[1], w[0]);
	break;
      case 10:			/* Kolmogorov-Smirnov */
	fprintf (stdout,
	  "Kolmogorov-Smirnov's D (regular & modified for normality): ");
	w = kolmogorov_smirnov (z, nsites);
	fprintf (stdout,"%g %g\n", w[1], w[0]);
	break;
      case 11:			/* chi-square */
	fprintf (stdout,
	       "Chi-Square stat (equal probability classes) and d.f.: ");
	w = chi_square (z, nsites);
	fprintf (stdout,"%g %d\n", w[0], (int) w[1]);
	break;
      case 12:			/* Shapiro-Wilk */
	if (nsites > 50)
	{
	  fprintf (stdout, "\nShapiro-Wilk's W cannot be used for n > 50\n");
	  if (nsites < 99)
	    fprintf (stdout, "Use Weisberg-Binghams's W''\n\n");
	  else
	    fprintf (stdout, "\n");
	}
	else
	{
	  fprintf (stdout, "Shapiro-Wilk W: ");
	  w = shapiro_wilk (z, nsites);
	  fprintf (stdout,"%g\n", w[0]);
	}
	break;
      case 13:			/* Weisberg-Bingham */
	if (nsites > 99 || nsites < 50)
	  fprintf (stdout,
		   "\nWeisberg-Bingham's W'' cannot be used for n < 50 or n > 99\n\n");
	else
	{
	  fprintf (stdout, "Weisberg-Bingham's W'': ");
	  w = weisberg_bingham (z, nsites);
	  fprintf (stdout,"%g\n", w[0]);
	}
	break;
      case 14:			/* Royston */
	if (nsites > 2000)
	  fprintf (stdout,
	  "\nRoyston only extended Shapiro-Wilk's W up to n = 2000\n\n");
	else
	{
	  fprintf (stdout, "Shapiro-Wilk W'': ");
	  w = royston (z, nsites);
	  fprintf (stdout,"%g\n", w[0]);
	}
	break;
      case 15:			/* Kotz */
	fprintf (stdout,
		 "Kotz' T'_f (Lognormality vs. Normality): ");
	w = kotz_families (z, nsites);
	fprintf (stdout,"%g\n", w[0]);
	break;
      default:
	break;
      }
  }
  exit (0);
}
