/*-
 * s.surf.rst: main program for interpolation and topographic analysis
 * from scattered point data using regularized spline with tension
 *
 * Original program (1989) and various modifications:
 * Lubos Mitas
 *
 * GRASS4.1 version of the program and GRASS4.2 modifications:
 * H. Mitasova
 * I. Kosinovsky, D. Gerdes
 * D. McCauley
 *
 * Copyright (C) 1989, 1993, 1995, 1997 L. Mitas,  H. Mitasova,
 * I. Kosinovsky, D.Gerdes, D. McCauley
 *
 *This program is free software; you can redistribute it and/or
 *modify it under the terms of the GNU General Public License
 *as published by the Free Software Foundation; either version 2
 *of the License, or (at your option) any later version.
 *
 *This program is distributed in the hope that it will be useful,
 *but WITHOUT ANY WARRANTY; without even the implied warranty of
 *MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *GNU General Public License for more details.
 *
 *You should have received a copy of the GNU General Public License
 *along with this program; if not, write to the Free Software
 *Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA
 *
 *
 * Notes on modifications:
 *
 * modified by McCauley in August 1995
 * modified by Mitasova in August 1995 segmentation, tension
 * modified by Mitasova in November 1996, reference info, no mod. needed
 *                      for variable smoothing
 * modified by Brown in June 1999 to allow site attribute selection
 *                      for elev & smoothing
 *
 * Modified by Brown in Oct 1999 to fix writing of TimeStamp info so
 * that the TimeStamp from the original sites file is copied to the
 * new devi sites file if it is being written. (Was using datetime and
 * only writing current creation date). Site head uses TimeStamp instead
 * of datetime. TimeStamp is also written to any raster files created,
 * if a valid TimeStamp is read.
 *
 * Modified by Bill Hughes in Oct. 1999 for compliance with ANSI?
 * Modified by Mitasova in Nov. 1999 added -t flag for dnorm-independent tension
 *
 */


#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#include "config.h"
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#include "gis.h"
#include "site.h"
#include "Vect.h"
#include "linkm.h"
#include "bitmap.h"

#include "interpf.h"
#include "surf.h"

struct Map_info TreeMap, OverMap;

struct interp_params params;


#define SCIK1 1
#define SCIK2 1
#define SCIK3 1

double /* pargr */ ns_res, ew_res;
double dmin, ertre;
int KMAX2, KMIN, KMAX, totsegm, deriv, dtens;

DCELL *az = NULL, *adx = NULL, *ady = NULL, *adxx = NULL, *adyy = NULL,
*adxy = NULL;
double /* error */ ertot, ertre, zminac, zmaxac, zmult;
struct multtree *root;
struct tree_info *info;

int total = 0;
int NPOINT = 0;
int OUTRANGE = 0;
int NPT = 0;

double DETERM;
int NERROR, cond1, cond2;
char fncdsm[32];
char filnam[10];

FILE *fdinp, *fdredinp, *fdzout, *fddxout, *fddyout, *fdxxout, *fdyyout,
*fd4, *fxyout, *fddevi = NULL;

FCELL *zero_array_cell;

char *input;
char *treefile = NULL;
char *overfile = NULL;
char *mapset = NULL;
char *mapset1 = NULL;
char *elev = NULL;
char *slope = NULL;
char *aspect = NULL;
char *pcurv = NULL;
char *tcurv = NULL;
char *mcurv = NULL;
char *maskmap = NULL;
char *redinp = NULL;
char *devi = NULL;
int sdisk, disk, ddisk, sddisk;
FILE *Tmp_fd_z = NULL;
char *Tmp_file_z = NULL;
FILE *Tmp_fd_dx = NULL;
char *Tmp_file_dx = NULL;
FILE *Tmp_fd_dy = NULL;
char *Tmp_file_dy = NULL;
FILE *Tmp_fd_xx = NULL;
char *Tmp_file_xx = NULL;
FILE *Tmp_fd_yy = NULL;
char *Tmp_file_yy = NULL;
FILE *Tmp_fd_xy = NULL;
char *Tmp_file_xy = NULL;

double gmin, gmax, c1min, c1max, c2min, c2max, fi, rsm;
double xmin, xmax, ymin, ymax, zmin, zmax;
int elattr, smattr;

struct BM *bitmask;
struct Cell_head cellhd;
struct GModule *module;

char msg[1024];

static int print_tree (struct multtree *, double , double, struct Map_info *);

int main ( int argc, char *argv[]) 
{
  int per, npmin;
  int ii, i, n_rows, n_cols;
  double x_orig, y_orig, dnorm, deltx, delty, xm, ym;
  char dminchar[200];
  Site_head inhead, devihead;
  struct quaddata *data;
  struct multfunc *functions;
  struct multtree *tree;

  struct
  {
    struct Option *input, *smatt, *elatt, 
    *elev, *slope, *aspect, *pcurv, *tcurv, *mcurv, *maskmap,
    *dmin1, *zmult, *fi, *rsm, *segmax, *npmin, *devi, *treefile, *overfile;
  } parm;
  struct
  {
    struct Flag *deriv, *cprght;
  } flag;
  int dims = 0, cat = 0, strs = 0, dbls = 0;

  G_gisinit (argv[0]);

  module = G_define_module();
  module->description =        
                  "Interpolation and topographic analysis from given site "
                  "data to GRASS floating point raster format using "
                  "regularized spline with tension";
                  
  if (G_get_set_window (&cellhd) == -1)
    exit (0);
  ew_res = cellhd.ew_res;
  ns_res = cellhd.ns_res;
  n_cols = cellhd.cols;
  n_rows = cellhd.rows;
  x_orig = cellhd.west;
  y_orig = cellhd.south;
  xm = cellhd.east;
  ym = cellhd.north;
  if (ew_res < ns_res)
    dmin = ew_res / 2;
  else
    dmin = ns_res / 2;
  disk = n_rows * n_cols * sizeof (int);
  sdisk = n_rows * n_cols * sizeof (short int);
  sprintf (dminchar, "%f", dmin);

    fprintf (stderr, "\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Version: GRASS5.0 beta, last update: Nov 9 1999\n");
    fprintf (stderr, "input is x|y|%%z1 %%z2..., output is FP raster files\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Authors: original version L.Mitas, H.Mitasova\n");
    fprintf (stderr, "         GRASS implementation I.Kosinovsky, D.P. Gerdes\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Methods used in this program are described in the following papers:\n");
    fprintf (stderr, "Mitasova, H., and  Mitas, L., 1993,\n");
    fprintf (stderr, "Interpolation by Regularized Spline with Tension:\n");
    fprintf (stderr, "I. Theory  and  implementation.  Mathematical Geology, 25, 641-55.\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Mitasova, H., and Hofierka, J., 1993\n");
    fprintf (stderr, "Interpolation by Regularized Spline with Tension:\n");
    fprintf (stderr, "II. Application to terrain modeling and surface   geometry  analysis.\n");
    fprintf (stderr, "Mathematical Geology, 25, 657-69.\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Mitasova, H., Mitas, L., Brown, W.M., Gerdes, D.P., Kosinovsky, I.,\n");
    fprintf (stderr, "Baker, T., 1995, Modeling spatially and temporally\n");
    fprintf (stderr, "distributed phenomena: New methods and tools for GRASS GIS.\n");
    fprintf (stderr, "International Journal of Geographic Information Systems,9(4), 433-46.\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "The postscript versions of these papers are available via Internet at\n");
    fprintf (stderr, "http://www2.gis.uiuc.edu:2280/modviz/papers/listsj.html\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "Please cite these references in publications where the results of this\n");
    fprintf (stderr, "program were used.\n");
    fprintf (stderr, "\n");
    fprintf (stderr, "\n");


  parm.input = G_define_option ();
  parm.input->key = "input";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->gisprompt = "old,site_lists,sites";
  parm.input->description = "Name of the site file with input x|y|%z [%sm]";

  parm.elev = G_define_option ();
  parm.elev->key = "elev";
  parm.elev->type = TYPE_STRING;
  parm.elev->required = NO;
  parm.elev->gisprompt = "new,cell,raster";
  parm.elev->description = "Output z-file (elevation)";

  parm.slope = G_define_option ();
  parm.slope->key = "slope";
  parm.slope->type = TYPE_STRING;
  parm.slope->required = NO;
  parm.slope->gisprompt = "new,cell,raster";
  parm.slope->description = "Slope";

  parm.aspect = G_define_option ();
  parm.aspect->key = "aspect";
  parm.aspect->type = TYPE_STRING;
  parm.aspect->required = NO;
  parm.aspect->gisprompt = "new,cell,raster";
  parm.aspect->description = "Aspect";

  parm.elatt = G_define_option ();
  parm.elatt->key = "elatt";
  parm.elatt->type = TYPE_INTEGER;
  parm.elatt->options = "1-100";
  parm.elatt->answer = "1";
  parm.elatt->required = NO;
  parm.elatt->description = 
	  "Which fp site attribute to use for elevation (1=first)";

  parm.smatt = G_define_option ();
  parm.smatt->key = "smatt";
  parm.smatt->type = TYPE_INTEGER;
  parm.smatt->options = "1-100";
  parm.smatt->required = NO;
  parm.smatt->description = 
  "Which fp site attribute to use for smoothing (or use smooth= for constant)";

  parm.rsm = G_define_option ();
  parm.rsm->key = "smooth";
  parm.rsm->type = TYPE_DOUBLE;
  /*
  parm.rsm->options = "0.00-1000.00";
  */
  parm.rsm->required = NO;
  parm.rsm->description = "Smoothing parameter";

  parm.fi = G_define_option ();
  parm.fi->key = "tension";
  parm.fi->type = TYPE_DOUBLE;
  parm.fi->answer = TENSION;
  parm.fi->required = NO;
  parm.fi->description = "Tension";

  parm.devi = G_define_option ();
  parm.devi->key = "devi";
  parm.devi->type = TYPE_STRING;
  parm.devi->required = NO;
  parm.devi->gisprompt = "new,site_lists,sites";
  parm.devi->description = "Name of the output deviations site file";

  parm.treefile = G_define_option ();
  parm.treefile->key = "treefile";
  parm.treefile->type = TYPE_STRING;
  parm.treefile->required = NO;
  parm.treefile->gisprompt = "new,dig,vector";
  parm.treefile->description = "Output vector file showing segmentation";

  parm.overfile = G_define_option ();
  parm.overfile->key = "overfile";
  parm.overfile->type = TYPE_STRING;
  parm.overfile->required = NO;
  parm.overfile->gisprompt = "new,dig,vector";
  parm.overfile->description = "Output vector file showing overlapping segments";

  flag.deriv = G_define_flag ();
  flag.deriv->key = 'd';
  flag.deriv->description = "Output partial derivatives instead";

  parm.pcurv = G_define_option ();
  parm.pcurv->key = "pcurv";
  parm.pcurv->type = TYPE_STRING;
  parm.pcurv->required = NO;
  parm.pcurv->gisprompt = "new,cell,raster";
  parm.pcurv->description = "Profile curvature";

  parm.tcurv = G_define_option ();
  parm.tcurv->key = "tcurv";
  parm.tcurv->type = TYPE_STRING;
  parm.tcurv->required = NO;
  parm.tcurv->gisprompt = "new,cell,raster";
  parm.tcurv->description = "Tangential curvature";

  parm.mcurv = G_define_option ();
  parm.mcurv->key = "mcurv";
  parm.mcurv->type = TYPE_STRING;
  parm.mcurv->required = NO;
  parm.mcurv->gisprompt = "new,cell,raster";
  parm.mcurv->description = "Mean curvature";

  parm.maskmap = G_define_option ();
  parm.maskmap->key = "maskmap";
  parm.maskmap->type = TYPE_STRING;
  parm.maskmap->required = NO;
  parm.maskmap->gisprompt = "old,cell,raster";
  parm.maskmap->description = "Name of the raster file used as mask";

  parm.dmin1 = G_define_option ();
  parm.dmin1->key = "dmin";
  parm.dmin1->type = TYPE_DOUBLE;
  parm.dmin1->answer = dminchar;
  parm.dmin1->required = NO;
  parm.dmin1->description = "Min distance between points (extra points ignored)";

  parm.zmult = G_define_option ();
  parm.zmult->key = "zmult";
  parm.zmult->type = TYPE_DOUBLE;
  parm.zmult->answer = ZMULT;
  parm.zmult->required = NO;
  parm.zmult->description = "Conversion factor for z-values";

  parm.segmax = G_define_option ();
  parm.segmax->key = "segmax";
  parm.segmax->type = TYPE_INTEGER;
  parm.segmax->answer = MAXSEGM;
  parm.segmax->required = NO;
  parm.segmax->description = "Max number of points in segment";

  parm.npmin = G_define_option ();
  parm.npmin->key = "npmin";
  parm.npmin->type = TYPE_INTEGER;
  parm.npmin->answer = MINPOINTS;
  parm.npmin->required = NO;
  parm.npmin->description = "Min number of points for interpolation(>segmax)";

  flag.cprght = G_define_flag ();
  flag.cprght->key = 't';
  flag.cprght->description = "Use dnorm-independent tension";


  if (G_parser (argc, argv))
    exit (1);

  per = 1;
  input = parm.input->answer;
  maskmap = parm.maskmap->answer;
  elev = parm.elev->answer;
  devi = parm.devi->answer;
  slope = parm.slope->answer;
  aspect = parm.aspect->answer;
  pcurv = parm.pcurv->answer;
  tcurv = parm.tcurv->answer;
  mcurv = parm.mcurv->answer;
  treefile = parm.treefile->answer;
  overfile = parm.overfile->answer;
  /*
   * G_set_embedded_null_value_mode(1);
   */
  if ((elev == NULL) && (pcurv == NULL) && (tcurv == NULL) && (mcurv == NULL)
      && (slope == NULL) && (aspect == NULL) && (devi == NULL))
    fprintf (stderr, "Warning -- you are not outputing any raster or site files\n");

  cond2 = ((pcurv != NULL) || (tcurv != NULL) || (mcurv != NULL));
  cond1 = ((slope != NULL) || (aspect != NULL) || cond2);
  deriv = flag.deriv->answer;
  dtens = flag.cprght->answer;

  ertre = 0.1;
  sscanf (parm.dmin1->answer, "%lf", &dmin);
  sscanf (parm.fi->answer, "%lf", &fi);
  sscanf (parm.segmax->answer, "%d", &KMAX);
  sscanf (parm.npmin->answer, "%d", &npmin);
  sscanf (parm.zmult->answer, "%lf", &zmult);
  sscanf (parm.elatt->answer, "%d", &elattr);
  if(parm.rsm->answer){
      sscanf (parm.rsm->answer, "%lf", &rsm);
      if(rsm < 0.0)
	  G_fatal_error("Smoothing must be a positive value");
      if(parm.smatt->answer){
	  G_warning("Both smatt and smooth options specified - using constant");
	  smattr = 0;
      }
  }
  else{
      sscanf (SMOOTH, "%lf", &rsm);
      if(parm.smatt->answer){
	  sscanf (parm.smatt->answer, "%d", &smattr);
	  rsm = -1; /* used in InterpLib to indicate variable smoothing */
      }
  }
  if (npmin > MAXPOINTS - 50)
    KMAX2 = npmin + 50;
  else
    KMAX2 = MAXPOINTS;
  dmin = dmin * dmin;
  KMIN = npmin;
/*  fprintf (stderr, "MAXPOINTS=%d,KMAX2=%d,KMIN=%d\n", MAXPOINTS,KMAX2,KMIN);*/
  az = G_alloc_vector (n_cols + 1);
  if (!az)
  {
    G_fatal_error ("Not enough memory for az");
  }
  if (cond1)
  {
    adx = G_alloc_vector (n_cols + 1);
    if (!adx)
    {
      G_fatal_error ("Not enough memory for adx");
    }
    ady = G_alloc_vector (n_cols + 1);
    if (!ady)
    {
      G_fatal_error ("Not enough memory for ady");
    }
    if (cond2)
    {
      adxx = G_alloc_vector (n_cols + 1);
      if (!adxx)
      {
	G_fatal_error ("Not enough memory for adxx");
      }
      adyy = G_alloc_vector (n_cols + 1);
      if (!adyy)
      {
	G_fatal_error ("Not enough memory for adyy");
      }
      adxy = G_alloc_vector (n_cols + 1);
      if (!adxy)
      {
	G_fatal_error ("Not enough memory for adxy");
      }
    }
  }
  if ((data = quad_data_new (x_orig, y_orig, xm, ym, n_rows, n_cols, 0, KMAX)) == NULL)
    G_fatal_error ("cannot create quaddata");
  if ((functions = MT_functions_new (quad_compare, quad_divide_data, quad_add_data, quad_intersect, quad_division_check, quad_get_points)) == NULL)
    G_fatal_error ("cannot create quadfunc");
  if ((tree = MT_tree_new (data, NULL, NULL, 0)) == NULL)
    G_fatal_error ("cannot create tree");
  root = tree;
  if ((info = MT_tree_info_new (root, functions, dmin, KMAX)) == NULL)
    G_fatal_error ("cannot create tree info");

  mapset = G_find_file ("site_lists", input, "");
  if (mapset == NULL)
  {
    sprintf (msg, "file [%s] not found", input);
    G_fatal_error (msg);
  }
  if ((fdinp = G_fopen_sites_old (input, mapset)) == NULL)
  {
    sprintf (msg, "Cannot open %s", input);
    G_fatal_error (msg);
  }
  if (G_site_describe (fdinp, &dims, &cat, &strs, &dbls) != 0)
    G_fatal_error ("failed to guess site format");
  if (G_site_get_head (fdinp, &inhead) != 0)
    G_fatal_error ("failed to read site header");

  if (dbls == 0)
  {
    if (cat != -1)
    {
      fprintf (stderr, "\nWARNING: old data format detected.\n");
      if ((mapset = G_find_file ("site_lists", input, G_mapset ())) == NULL)
      {
	fprintf (stderr,
	       "%sSite list [%s] cannot be used in its current format\n",
		 "ERROR:\t", input);
	fprintf (stderr, "\tIt cannot be converted to the new format ");
	fprintf (stderr, "since it is not\n\tin the current mapset\n");
	exit (-1);
      }
      fprintf (stdout,"Convert site file to new format?\n");
      cat = G_yes ("Answering yes will modify your site list file.", 1);
      if (cat == 1)
      {
	fclose (fdinp);
	sprintf (msg, "%s/etc/s.o2n.tps %s", G_gisbase (), input);
	G_system (msg);
	if ((fdinp = G_fopen_sites_old (input, mapset)) == NULL)
	{
	  sprintf (msg, "Cannot open %s", input);
	  G_fatal_error (msg);
	}
      }
      else
	G_fatal_error ("no decimal attributes found in site list");
    }
    else
      G_fatal_error ("no decimal attributes found in site list");
  }

  if (devi != NULL)
  {
    if ((fddevi = G_fopen_sites_new (devi)) == NULL)
    {
      sprintf (msg, "Cannot open %s", devi);
      G_fatal_error (msg);
    }
    else
    {
      devihead.name = devi;
      devihead.desc = G_strdup ("deviations at sample points");
      devihead.desc = (char *) G_malloc (128 * sizeof (char));
      sprintf (devihead.desc, "deviations of %s [raster] at %s [sites]",
	       elev, input);
      devihead.time = inhead.time;
      devihead.stime = inhead.stime;
      devihead.labels = NULL;
      devihead.form = NULL;
      G_site_put_head (fddevi, &devihead);
    }
  }


  ertot = 0.;
  /* if (per) fprintf (stderr, "Percent complete: "); */
  if (elev != NULL)
    Tmp_file_z = G_tempfile ();
  if (slope != NULL)
    Tmp_file_dx = G_tempfile ();
  if (aspect != NULL)
    Tmp_file_dy = G_tempfile ();
  if (pcurv != NULL)
    Tmp_file_xx = G_tempfile ();
  if (tcurv != NULL)
    Tmp_file_yy = G_tempfile ();
  if (mcurv != NULL)
    Tmp_file_xy = G_tempfile ();

  zero_array_cell = (FCELL *) malloc (sizeof (FCELL) * n_cols);
  if (!zero_array_cell)
    G_fatal_error ("Not enough memory for zero_array_cell");

  for (i = 0; i < n_cols; i++)
  {
    zero_array_cell[i] = (FCELL) 0;
  }

  if (Tmp_file_z != NULL)
  {
    if (NULL == (Tmp_fd_z = fopen (Tmp_file_z, "w+")))
    {
      sprintf (msg, "Can't open temp file [%s] ", Tmp_file_z);
      G_fatal_error (msg);
    }
    for (i = 0; i < n_rows; i++)
    {
      if (!(fwrite (zero_array_cell, sizeof (FCELL), n_cols, Tmp_fd_z)))
	G_fatal_error ("Not enough disk space -- cannot write files");
    }
  }
  if (Tmp_file_dx != NULL)
  {
    if (NULL == (Tmp_fd_dx = fopen (Tmp_file_dx, "w+")))
    {
      sprintf (msg, "Can't open temp file [%s] ", Tmp_file_dx);
      G_fatal_error (msg);
    }
    for (i = 0; i < n_rows; i++)
    {
      if (!(fwrite (zero_array_cell, sizeof (FCELL), n_cols, Tmp_fd_dx)))
	G_fatal_error ("Not enough disk space -- cannot write files");
    }
  }
  if (Tmp_file_dy != NULL)
  {
    if (NULL == (Tmp_fd_dy = fopen (Tmp_file_dy, "w+")))
    {
      sprintf (msg, "Can't open temp file [%s] ", Tmp_file_dy);
      G_fatal_error (msg);
    }
    for (i = 0; i < n_rows; i++)
    {
      if (!(fwrite (zero_array_cell, sizeof (FCELL), n_cols, Tmp_fd_dy)))
	G_fatal_error ("Not enough disk space -- cannot write files");
    }
  }

  if (Tmp_file_xx != NULL)
  {
    if (NULL == (Tmp_fd_xx = fopen (Tmp_file_xx, "w+")))
    {
      sprintf (msg, "Can't open temp file [%s] ", Tmp_file_xx);
      G_fatal_error (msg);
    }
    for (i = 0; i < n_rows; i++)
    {
      if (!(fwrite (zero_array_cell, sizeof (FCELL), n_cols, Tmp_fd_xx)))
	G_fatal_error ("Not enough disk space -- cannot write files");
    }
  }
  if (Tmp_file_yy != NULL)
  {
    if (NULL == (Tmp_fd_yy = fopen (Tmp_file_yy, "w+")))
    {
      sprintf (msg, "Can't open temp file [%s] ", Tmp_file_yy);
      G_fatal_error (msg);
    }
    for (i = 0; i < n_rows; i++)
    {
      if (!(fwrite (zero_array_cell, sizeof (FCELL), n_cols, Tmp_fd_yy)))
	G_fatal_error ("Not enough disk space -- cannot write files");
    }
  }
  if (Tmp_file_xy != NULL)
  {
    if (NULL == (Tmp_fd_xy = fopen (Tmp_file_xy, "w+")))
    {
      sprintf (msg, "Can't open temp file [%s] ", Tmp_file_xy);
      G_fatal_error (msg);
    }
    for (i = 0; i < n_rows; i++)
    {
      if (!(fwrite (zero_array_cell, sizeof (FCELL), n_cols, Tmp_fd_xy)))
	G_fatal_error ("Not enough disk space -- cannot write files");
    }
  }

/*
void IL_init_params_2d(struct interp_params *, FILE *, int, int, double,
   int, int, char *, int, int,
   FCELL *, FCELL *, FCELL *, FCELL *, FCELL *, FCELL *,
   double, int, int, int, int, double,
   char *, char *, char *, char *, char *, char *,
   double, double, double, int,
   FILE *, FILE *, FILE *, FILE *, FILE *, FILE *, FILE *, struct TimeStamp *);
*/
  IL_init_params_2d (&params, fdinp, elattr, smattr, zmult, KMIN, KMAX, 
		     maskmap, n_rows, n_cols, az, adx, ady, adxx, adyy, adxy, 
		     fi, KMAX2, SCIK1, SCIK2, SCIK3,
		     rsm, elev, slope, aspect, pcurv, tcurv, mcurv, dmin, 
		     x_orig, y_orig, deriv,
		     Tmp_fd_z, Tmp_fd_dx, Tmp_fd_dy, 
		     Tmp_fd_xx, Tmp_fd_yy, Tmp_fd_xy, fddevi, inhead.time);

  IL_init_func_2d (&params, IL_grid_calc_2d, IL_matrix_create, IL_check_at_points_2d,
		 IL_secpar_loop_2d, IL_crst, IL_crstg, IL_write_temp_2d);

  totsegm = IL_input_data_2d (&params, info, &xmin, &xmax, &ymin, &ymax, &zmin, &zmax, &NPOINT);
  if (totsegm <= 0)
    G_fatal_error ("Input failed");

  if (treefile != NULL)
  {
    if (0 > Vect_open_new (&TreeMap, treefile))
    {
      sprintf (msg, "Not able to open vector file <%s>\n", treefile);
      G_fatal_error (msg);
    }

    TreeMap.head.W = cellhd.west;
    TreeMap.head.E = cellhd.east;
    TreeMap.head.N = cellhd.north;
    TreeMap.head.S = cellhd.south;
    sprintf (TreeMap.head.your_name, "grass");
    sprintf (TreeMap.head.map_name, "Quad tree for %s", input);
    TreeMap.head.orig_scale = 100000;
    TreeMap.head.plani_zone = G_zone ();
    print_tree (root, x_orig, y_orig, &TreeMap);
    Vect_close (&TreeMap);
  }
  disk = disk + totsegm * sizeof (int) * 4;
  sdisk = sdisk + totsegm * sizeof (int) * 4;
  if (elev != NULL)
    ddisk += disk;
  if (slope != NULL)
    sddisk += sdisk;
  if (aspect != NULL)
    sddisk += sdisk;
  if (pcurv != NULL)
    ddisk += disk;
  if (tcurv != NULL)
    ddisk += disk;
  if (mcurv != NULL)
    ddisk += disk;
  ddisk += sddisk;
  fprintf (stderr, "\n");
  fprintf (stderr, "Processing all selected output files \n");
  fprintf (stderr, "will require %d bytes of disk space for temp files \n", ddisk
    );
  fprintf (stderr, "\n");

  deltx = xmax - xmin;
  delty = ymax - ymin;
  dnorm = sqrt ((deltx * delty * KMIN) / NPOINT);

  if(dtens)
 {
  params.fi = params.fi * dnorm / 1000.;
  fprintf (stderr, "dnorm = %f, rescaled tension = %f\n", dnorm, params.fi);
  }

  if (maskmap != NULL)
    bitmask = BM_create (n_cols, n_rows);

  IL_create_bitmask (&params, bitmask);
  if (totsegm <= 0)
    G_fatal_error ("Input failed");
  ertot = 0.;
  if (per)
    fprintf (stderr, "Percent complete: ");
  if (!IL_interp_segments_2d (&params, info, info->root, bitmask,
		      zmin, zmax, &zminac, &zmaxac, &gmin, &gmax, &c1min,
		 &c1max, &c2min, &c2max, &ertot, totsegm, n_cols, dnorm))

    G_fatal_error ("Interp_segmets failed");

  G_free_vector (az);
  if (cond1)
  {
    G_free_vector (adx);
    G_free_vector (ady);
    if (cond2)
    {
      G_free_vector (adxx);
      G_free_vector (adyy);
      G_free_vector (adxy);
    }
  }
  ii = IL_output_2d (&params, &cellhd, zmin, zmax, zminac, zmaxac, c1min, c1max, c2min, c2max,
		     gmin, gmax, ertot, input, dnorm, dtens, 0, NPOINT);
  if (ii < 0)
    G_fatal_error ("Cannot write cell files -- try to increase resolution");
  free (zero_array_cell);
  if (elev != NULL)
    fclose (Tmp_fd_z);
  if (slope != NULL)
    fclose (Tmp_fd_dx);
  if (aspect != NULL)
    fclose (Tmp_fd_dy);
  if (pcurv != NULL)
    fclose (Tmp_fd_xx);
  if (tcurv != NULL)
    fclose (Tmp_fd_yy);
  if (mcurv != NULL)
    fclose (Tmp_fd_xy);

  if (overfile != NULL)
  {
    if (0 > Vect_open_new (&OverMap, overfile))
    {
      sprintf (msg, "Not able to open vector file <%s>\n", overfile);
      G_fatal_error (msg);
    }

    OverMap.head.W = cellhd.west;
    OverMap.head.E = cellhd.east;
    OverMap.head.N = cellhd.north;
    OverMap.head.S = cellhd.south;
    sprintf (OverMap.head.your_name, "grass");
    sprintf (OverMap.head.map_name, "Overlap segments for %s", input);
    OverMap.head.orig_scale = 100000;
    OverMap.head.plani_zone = G_zone ();
    print_tree (root, x_orig, y_orig, &OverMap);
    Vect_close (&OverMap);
  }
  if (elev != NULL)
    unlink (Tmp_file_z);
  if (slope != NULL)
    unlink (Tmp_file_dx);
  if (aspect != NULL)
    unlink (Tmp_file_dy);
  if (pcurv != NULL)
    unlink (Tmp_file_xx);
  if (tcurv != NULL)
    unlink (Tmp_file_yy);
  if (mcurv != NULL)
    unlink (Tmp_file_xy);

  if (fddevi != NULL)
    fclose (fddevi);

  exit (0);
}

static int print_tree (struct multtree *tree,
    double x_orig, double y_orig, struct Map_info *Map)
{
  double x_se, x_sw, x_ne, x_nw, y_se, y_sw, y_ne, y_nw;
  double xarray[5], yarray[5];
  struct line_pnts *Points;
  static int k = 0;
  static int i = 1;
  int j;
  int type = LINE;

  if (tree == NULL)
    return 0;
  if (tree->data == NULL)
    return 0;
  if (tree->leafs != NULL)
  {
    for (j = 0; j < 4; j++)
    {
      print_tree (tree->leafs[j], x_orig, y_orig, Map);
    }
  }
  else
  {
    Points = Vect_new_line_struct ();
    xarray[0] = ((struct quaddata *) (tree->data))->x_orig + x_orig;
    yarray[0] = ((struct quaddata *) (tree->data))->y_orig + y_orig;
    xarray[1] = xarray[0];
    yarray[3] = yarray[0];
    xarray[3] = ((struct quaddata *) (tree->data))->xmax + x_orig;
    yarray[1] = ((struct quaddata *) (tree->data))->ymax + y_orig;
    yarray[2] = yarray[1];
    xarray[2] = xarray[3];
    yarray[4] = yarray[0];
    xarray[4] = xarray[0];
    if (0 > Vect_copy_xy_to_pnts (Points, xarray, yarray, 5))
      G_fatal_error ("Out of memory");
    Vect_write_line (Map, (unsigned int) type, Points);

    free (Points);
  }
  return 1;
}
