/*-
 * s.sample - GRASS program to sample a raster file at site locations.
 * Copyright (C) 1994. James Darrell McCauley.
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
 * <13 Sep 2000> - released under GPL
 *
 */

#pragma ident "s.sample v 0.8B <15 Jun 1995>; Copyright (c) 1994-1995. James Darrell McCauley"

#include <string.h>
#include <math.h>
#include "gis.h"
#include "site.h"
#include "methods.h"

#ifndef lint
static char *methodnames[NMETHODS+1] = {
        "(undefined)",
        "Nearest Cell",
        "Bilinear Interpolation",
        "Cubic Convolution Interpolation" };
#endif

#define MY_XYZ_SIZE 100

int main (argc, argv)
  char **argv;
  int argc;
{
  extern char *methodnames[NMETHODS+1];
  char *isiteslist, *ositeslist, errmsg[256], *raster, *mapset;
  double scale, predicted, actual;
  int i, j, b, c, nsites, verbose, sfield, sindex;
  int method = 0;		/* one of NEAREST, BILINEAR, or CUBIC */
  int usesitecat, usecelldesc, dodiff; /* indicator variables */
  int fdrast;	/* file descriptor for raster file is int */
  struct Cell_head window;
  struct GModule *module;
  struct Categories cats;
  RASTER_MAP_TYPE map_type;
  int strs, dbls, dims;
  struct
  {
    struct Option *input, *output, *rast, *z, *sfield, *sindex;
  } parm;
  struct
  {
    struct Flag *B, *C, *l, *q;
  } flag;
  FILE *fdisite = NULL, *fdosite = NULL;
  SITE_XYZ xyz[MY_XYZ_SIZE];
  Site *outSite;
  
  G_gisinit (argv[0]);
  
  module = G_define_module();
  module->description =        
                  "Sample a raster file at site locations.";
                  
  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "sites list defining sample points";
  parm.input->gisprompt = "old,site_lists,sites";

  parm.sfield = G_define_option();
  parm.sfield->key = "attr";
  parm.sfield->type = TYPE_STRING;
  parm.sfield->required = NO;
  parm.sfield->description = "Calculate difference between site attribute and "
	  "cell value";
  parm.sfield->options = "none,cat,dim,decimal";
  parm.sfield->answer  = "none";
  
  parm.sindex = G_define_option();
  parm.sindex->key = "index";
  parm.sindex->type = TYPE_INTEGER;
  parm.sindex->required = NO;
  parm.sindex->description = "Site attribute index for dim or decimal fields";
  parm.sindex->answer = "1";

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
    if (b)
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
  
  dodiff = usesitecat = 0;
  if (strncmp(parm.sfield->answer, "none", 4) == 0)
    sfield = SITE_COL_NUL;
  else if (strncmp(parm.sfield->answer, "dim", 3) == 0) {
    dodiff = 1;
    sfield = SITE_COL_DIM;
  }
  else if (strncmp(parm.sfield->answer, "cat", 3) == 0) {
    sfield = SITE_COL_NUL;
    usesitecat = 1;
    dodiff = 1;
  }
  else if (strncmp(parm.sfield->answer, "decimal", 7) == 0) {
    sfield = SITE_COL_DBL;
    dodiff = 1;
  }
  else { /* G_parser() shouldn't let this happen */ 
    G_fatal_error("%s: Unknown attribute type %s", G_program_name(), 
        parm.sfield->answer);
  }

  if (sfield == SITE_COL_DBL || sfield == SITE_COL_DIM) {
    sindex = atoi(parm.sindex->answer);
    if (sfield == SITE_COL_DIM) {
      if (sindex < 3)
        G_fatal_error("%s: Site dimension attribute must be greater than 2",
            G_program_name());
      sindex -= 2;
    }
    else if (sfield = SITE_COL_DBL) {
      if (sindex < 1)
        G_fatal_error("%s: Site decimal attribute must be greater than 0",
            G_program_name());
    }
  }
  else
    sindex = -1;

  usecelldesc = (flag.l->answer == (char)NULL) ? 0 : 1;
  verbose = (flag.q->answer == (char)NULL) ? 1 : 0;

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

  if (G_site_describe(fdisite, &dims, &map_type, &strs, &dbls) != 0)
    G_fatal_error("Failed to guess site format");
  
  outSite = G_site_new_struct(map_type, 2, 0, 1);
  if (!outSite)
    G_fatal_error("Unable to allocate memory.");
 
  if (verbose)
    fprintf (stderr, "Checking sites ...                 \n ");

  j = 0;
  while ((nsites = G_readsites_xyz(fdisite, sfield, sindex, MY_XYZ_SIZE,
          &window, xyz)) > 0)
  {
    for (i = 0; i < nsites; ++i)	/* for each partition */
    {
      j++;
      /* Set output Site east & north and cat (if any) */
      outSite->east = xyz[i].x;
      outSite->north = xyz[i].y;
      switch(xyz[i].cattype) {
        case CELL_TYPE:
          outSite->cattype = CELL_TYPE;
          outSite->ccat = xyz[i].cat.c;
          break;
        case FCELL_TYPE:
          outSite->cattype = FCELL_TYPE;
          outSite->fcat = xyz[i].cat.f;
          break;
        case DCELL_TYPE:
          outSite->cattype = DCELL_TYPE;
          outSite->dcat   = xyz[i].cat.d;
          break;
        default: /* No cattype, use 'j' */
          outSite->cattype = CELL_TYPE;
          outSite->ccat = j;
      }
      
      /* find predicted value */
      switch (method)
      {
      case BILINEAR:
        predicted=scale*
          bilinear (fdrast, window, cats, xyz[i].y, xyz[i].x, usecelldesc);
        break;
      case CUBIC:
        predicted=scale*
          cubic (fdrast, window, cats, xyz[i].y, xyz[i].x, usecelldesc);
        break;
      case NEAREST:
        predicted=scale*
          nearest (fdrast, window, cats, xyz[i].y, xyz[i].x, usecelldesc);
        break;
      default:
        G_fatal_error ("unknown method");	/* cannot happen */
        break;
      }

      /* find actual value */
      if (dodiff)
      {
        if (usesitecat){
          switch(xyz[i].cattype){
            case CELL_TYPE:
              actual = (double) xyz[i].cat.c;
              break;
            case FCELL_TYPE:
              actual = (double) xyz[i].cat.f;
              break;
            case DCELL_TYPE:
              actual = (double) xyz[i].cat.d;
              break;
            default:
              G_fatal_error("Input site has missing category value(s)");
          }
        }
        else if (sfield == SITE_COL_DIM || sfield == SITE_COL_DBL) {
          actual = xyz[i].z;
	}
        outSite->dbl_att[0]=predicted-actual;
      }
      else
        outSite->dbl_att[0]=predicted;
      G_site_put (fdosite, outSite);
    } /* End for(i...) */
  } /* End while(G_readsites...) */

  fclose (fdosite);
  G_close_cell (fdrast);

  exit (0);
}
