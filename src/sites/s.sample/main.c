/*-
 * s.sample - GRASS program to sample a raster file at site locations.
 * Copyright (C) 1994. James Darrell McCauley.
 *
 * Author: James Darrell McCauley (mccauley@ecn.purdue.edu)
 *         USDA Fellow
 *         Department of Agricultural Engineering
 *         Purdue University
 *         West Lafayette, Indiana 47907-1146 USA
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for non-commercial purposes is hereby granted. This 
 * software is provided "as is" without express or implied warranty.
 *
 * JAMES DARRELL MCCAULEY (JDM) MAKES NO EXPRESS OR IMPLIED WARRANTIES
 * (INCLUDING BY WAY OF EXAMPLE, MERCHANTABILITY) WITH RESPECT TO ANY
 * ITEM, AND SHALL NOT BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL
 * OR CONSEQUENTAL DAMAGES ARISING OUT OF THE POSSESSION OR USE OF
 * ANY SUCH ITEM. LICENSEE AND/OR USER AGREES TO INDEMNIFY AND HOLD
 * JDM HARMLESS FROM ANY CLAIMS ARISING OUT OF THE USE OR POSSESSION 
 * OF SUCH ITEMS.
 *
 * Modification History:
 * <04 Jan 1994> - began coding (jdm)
 * <06 Jan 1994> - announced version 0.1B on pasture.ecn.purdue.edu (jdm)
 * <21 Jan 1994> - changed wording on help screen. Revised to 0.2B (jdm)
 * <24 Jan 1994> - got rid of diagnostic messages. Revised to 0.3B (jdm)
 * ?? Revised to 0.4B (jdm)
 * <19 Dec 1994> - fixed bug in readsites, added html. Revised to 0.5B (jdm)
 * <02 Jan 1995> - cleaned Gmakefile, man page, html. 
 *                 fixed memory error in bilinear and cubic 0.6B (jdm)
 * <25 Feb 1995> - cleaned 'gcc -Wall' warnings 0.7B (jdm)
 * <15 Jun 1995> - fixed pointer error for G_{col,row}_to_{easting,northing}.
 *                 0.8B (jdm)
 *
 */

#pragma ident "s.sample v 0.8B <15 Jun 1995>; Copyright (c) 1994-1995. James Darrell McCauley"

#include <math.h>
#include "gis.h"
#include "methods.h"

#ifndef lint
static char *methodnames[NMETHODS+1] = {
        "(undefined)",
        "Nearest Cell",
        "Bilinear Interpolation",
        "Cubic Convolution Interpolation" };
#endif

int main (argc, argv)
  char **argv;
  int argc;
{
  extern char *methodnames[NMETHODS+1];
  char *isiteslist, *ositeslist, errmsg[256], *raster, *mapset;
  double scale, predicted, actual;
  int i, b, c, nsites, verbose;
  int method = 0;		/* one of NEAREST, BILINEAR, or CUBIC */
  int usesitecat, usecelldesc, dodiff; /* indicator variables */
  int fdrast;	/* file descriptor for raster file is int */
  struct Cell_head window;
  struct Categories cats;
  struct
  {
    struct Option *input, *output, *rast, *z;
  } parm;
  struct
  {
    struct Flag *B, *C, *c, *d, *l, *q;
  } flag;
  FILE *fdisite = NULL, *fdosite = NULL;
  Site **z;

  G_gisinit (argv[0]);

  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "sites list defining sample points";
  parm.input->gisprompt = "old,site_lists,sites";

  parm.output = G_define_option ();
  parm.output->key = "output";
  parm.output->type = TYPE_STRING;
  parm.output->required = NO;
  parm.output->description = "sites list to store differences";
  parm.output->gisprompt = "new,site_lists,sites";

  parm.rast = G_define_option ();
  parm.rast->key = "rast";
  parm.rast->type = TYPE_STRING;
  parm.rast->required = YES;
  parm.rast->description = "raster file to be sampled";
  parm.rast->gisprompt = "old,cell,raster";

  parm.z = G_define_option ();
  parm.z->key = "z";
  parm.z->type = TYPE_DOUBLE;
  parm.z->required = NO;
  parm.z->description = "raster scaling factor";
  parm.z->options = NULL;

  flag.B = G_define_flag ();
  flag.B->key = 'B';
  flag.B->description = "Bilinear interpolation [default is nearest neighbor]";

  flag.C = G_define_flag ();
  flag.C->key = 'C';
  flag.C->description = "Cubic convolution interpolation [default is nearest neighbor]";

  flag.l = G_define_flag ();
  flag.l->key = 'l';
  flag.l->description = "Use raster category labels (instead of category values)";

  flag.d = G_define_flag ();
  flag.d->key = 'd';
  flag.d->description = "Calculate difference between sites and cells";

  flag.c = G_define_flag ();
  flag.c->key = 'c';
  flag.c->description = "Use site categories (instead of floating point attributes; implies -d flag)";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  if (G_parser (argc, argv))
    exit (1);

  isiteslist=parm.input->answer;
  ositeslist=parm.output->answer;
  raster=parm.rast->answer;
  if (parm.z->answer == NULL)
    scale = 1.0;
  else
    sscanf (parm.z->answer, "%lf", &scale);
  if (!scale)
    scale = 1.0;

  b = (flag.B->answer == (char)NULL) ? 0 : 1;
  c = (flag.C->answer == (char)NULL) ? 0 : 1;
  if (b || c)
  {
    if (c)
      method = CUBIC;
    else
      method = BILINEAR;
    if (b && c)
      G_warning ("Flags -B & -C mutually exclusive. Bilinear method used.");
  }
  else
    method = NEAREST;

#ifdef DEBUG
  G_warning("Nearest forced");
  method = NEAREST;
#endif
  
  usecelldesc = (flag.l->answer == (char)NULL) ? 0 : 1;
  usesitecat = (flag.c->answer == (char)NULL) ? 0 : 1;
  verbose = (flag.q->answer == (char)NULL) ? 1 : 0;
  dodiff = (flag.d->answer == (char)NULL) ? 0 : 1;
  if (usesitecat) dodiff=1;

  G_get_window (&window);

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

  /* mapset may be different for rast, input, and output */
  if ((mapset = G_find_cell2 (raster, "")) == NULL)
  {
    sprintf (errmsg, "cell file [%s] not found", raster);
    G_fatal_error (errmsg);
  }
  if ((fdrast = G_open_cell_old (raster, mapset)) < 0)
  {
    sprintf (errmsg, "can't open cell file [%s]", raster);
    G_fatal_error (errmsg);
  }
  if (usecelldesc && G_read_cats (raster, mapset, &cats) < 0)
  {
    sprintf (errmsg, "can't open category file [%s]", raster);
    G_fatal_error (errmsg);
  }

  if (ositeslist == NULL)
    fdosite = stdout;
  else
  {
    if ((fdosite = G_fopen_sites_new (ositeslist)) == NULL)
    {
      sprintf (errmsg, "can't create sites file [%s]\nUsing standard output",
	       ositeslist);
      G_warning (errmsg);
      fdosite = stdout;
    }
  }
  if (fdosite != stdout )
  {
    fprintf (fdosite, "name|%s\n", ositeslist);
    if (dodiff)
      fprintf (fdosite, "desc|error:%s[rast]-%s[sites] by %s\n", 
               raster, isiteslist, methodnames[method]);
    else
      fprintf (fdosite, "desc|%s[rast] sampled at %s[sites] by %s\n", 
               raster, isiteslist, methodnames[method]);
  }

  /* This should probably be changed to read only one site at a time.
     That way we don't have to load all of the sites into memory at
      once. */
 
  z = readsites (fdisite, verbose, &nsites, window);

  if (nsites <= 0)
    G_fatal_error ("No sites found");

  if (verbose)
    fprintf (stderr, "Checking sites ...                  ");

  for (i = 0; i < nsites; ++i)	/* for each partition */
  {

    /* find predicted value */
    switch (method)
    {
    case BILINEAR:
      predicted=scale*
	bilinear (fdrast, window, cats, z[i]->north, z[i]->east, usecelldesc);
      break;
    case CUBIC:
      predicted=scale*
	cubic (fdrast, window, cats, z[i]->north, z[i]->east, usecelldesc);
      break;
    case NEAREST:
      predicted=scale*
	nearest (fdrast, window, cats, z[i]->north, z[i]->east, usecelldesc);
      break;
    default:
      G_fatal_error ("unknown method");	/* cannot happen */
      break;
    }

    /* find actual value */
    if (dodiff)
    {
      if (usesitecat){
	switch(z[i]->cattype){
	  case CELL_TYPE:
	    actual = (double) z[i]->ccat;
	    break;
	  case FCELL_TYPE:
	    actual = (double) z[i]->fcat;
	    break;
	  case DCELL_TYPE:
	    actual = (double) z[i]->dcat;
	    break;
	}
      }
      else
        actual = (double) z[i]->dbl_att[0];
      z[i]->dbl_att[0]=predicted-actual;
    }
    else
      z[i]->dbl_att[0]=predicted;
/*    G_site_put (fdosite, *z[i], 0);*/
      G_site_put (fdosite, z[i]);
    if (verbose)
      G_percent (i, nsites, 1);
  }
  fclose (fdosite);
  G_close_cell (fdrast);

  if (verbose)
    G_percent (1, 1, 1);
  exit (0);
}
