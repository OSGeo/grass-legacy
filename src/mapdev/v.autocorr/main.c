/*-v.autocorr
**
** Author: James Darrell McCauley (mccauley@ecn.purdue.edu)
**         USDA Fellow
**         Department of Agricultural Engineering
**         Purdue University
**         West Lafayette, Indiana 47907-1146 USA
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted. This
** software is provided "as is" without express or implied warranty.
**
** Reference: Griffith, Daniel A.  1987. Spatial Autcorrelation---A Primer.
** 1710 Sixteenth St NW, Washington, DC: Assoc of Am Geographers.
**
** Modification History:
** 15 Apr 92 - Created by James Darrell McCauley <mccauley@ecn.purdue.edu>
** 20 May 92 - Added capability to calculate means and std errs
**                                               <mccauley@ecn.purdue.edu>
** 27 May 92 - Added chatter and -h flag <mccauley@ecn.purdue.edu>
**/

#include <math.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "Vect.h"
#include "local_proto.h"

struct Cell_head window;
struct Categories cats;
int **c;
int quiet;

int main (int argc, char **argv)
{
  char *mapset;
  char name[20];
  int n, i;
  extern int **c, quiet;
  char gisbase[256], dig_cat_file[128];
  char err_msg[128];
  double gr, mc;		/* Geary Ratio and Moran Coefficient */
  double siggrr, sigmcr, siggrn, sigmcn;	/* Standard Errors */
  double mgrr, mmcr, mgrn, mmcn;/* Means Errors */
  double *catarray;
  FILE *fp_plus, *out;
  FILE *fp_att;
  struct Map_info *Map;

  struct
  {
    struct Flag *wmatrix, *cmatrix, *stats, *q, *h;
  } flag;
  struct
  {
    struct Option *input, *mapset, *output;
  } parm;


  /* Define the different options */

  parm.input = G_define_option ();
  parm.input->key = "vect";
  parm.input->type = TYPE_STRING;
  parm.input->required = YES;
  parm.input->description = "input vector filename";

  parm.mapset = G_define_option ();
  parm.mapset->key = "mapset";
  parm.mapset->type = TYPE_STRING;
  parm.mapset->required = NO;
  parm.mapset->description = "input mapset containing vector map[current]";
  parm.mapset->answer = "";

  parm.output = G_define_option ();
  parm.output->key = "output";
  parm.output->type = TYPE_STRING;
  parm.output->required = NO;
  parm.output->description = "output filename";

  flag.wmatrix = G_define_flag ();
  flag.wmatrix->key = 'w';
  flag.wmatrix->description = "Print Weight Matrix";

  flag.cmatrix = G_define_flag ();
  flag.cmatrix->key = 'c';
  flag.cmatrix->description = "Print Connectivity Matrix";

  flag.stats = G_define_flag ();
  flag.stats->key = 'n';
  flag.stats->description = "Suppress calculation of stats";

  flag.h = G_define_flag ();
  flag.h->key = 'h';
  flag.h->description = "Do hypothesis testing";

  flag.q = G_define_flag ();
  flag.q->key = 'q';
  flag.q->description = "Quiet";

  /* Initialize the GIS calls */
  G_gisinit (argv[0]);

  if (G_parser (argc, argv) < 0)
    exit (-1);

  strcpy (name, parm.input->answer);
  if (parm.output->answer != NULL)
  {
    if ((out = fopen (parm.output->answer, "w")) == NULL)
    {
      G_warning ("couldn't open output file... using stdout");
      out = stdout;
    }
  }
  else
    out = stdout;

  if( strcmp(parm.mapset->answer, "") == 0)
    mapset = G_mapset();
  else
    mapset = parm.mapset->answer;

  quiet=(flag.q->answer);
  /* init_plus_struct (&Plus);
  init_map_struct (&Map);
  */

  Map = (struct Map_info *)malloc( sizeof(struct Map_info));
  memset( Map, 0, sizeof(struct Map_info));

  if (!quiet)
    fprintf(stdout, "Opening files...\n");
  /* Determine the "mapset" based on your order of mapsets */
   if (!(mapset = G_find_vector2 (name, mapset)))
  {
    sprintf (err_msg, "\nVector file name:  '%s' NOT found in mapset '%s'.\n\n", name, mapset);
    G_fatal_error (err_msg);
  }

   /* Open vector file */
   if( Vect_open_old( Map, name, mapset) < 2 ) {
     strcpy( err_msg, "Could not open map '%s' with appropriate access level.\n" );
     G_fatal_error( err_msg );
   }

  /* open dig att and dig plus file  */
   /*if (open_dig_files (parm.input->answer, &fp_plus, &Map, &Plus))
    exit (-1);
   */

  /* if (fp_plus==NULL) G_fatal_error("null pointer"); */
  /*
   * if ((fp_plus = fopen (Map.plus_file, "r")) == NULL) G_fatal_error
   * ("Can't open dig_plus_file");
   */

  /* need to read cats... see src/libes/gis/cats.c */
  if (G_read_vector_cats (name, mapset, &cats) < 0)
  {
    sprintf (err_msg, "%s: %s in %s - can't read category file\n",
	     G_program_name (), name, mapset);
    G_fatal_error (err_msg);
  }

  /* dig_load_plus (&Map, fp_plus, 0); */
  if (Map->n_atts <= 0)
    G_fatal_error ("Map not labeled");

  /* Read category values into an ordinary array */
  catarray = (double *) G_malloc ((Map->n_atts + 1) * sizeof (double));
  if (catarray == NULL)
    G_fatal_error ("Memory allocation error for temporary array (main)");
  for (i = 1; i <= Map->n_atts; ++i)
    catarray[i] = att2cat (Map, i);

  /* Compute connectivity matrix */
  if (!quiet)
    fprintf(stdout, "Computing connectivity matrix...\n");
  n = c_matrix (Map);

  if (!flag.stats->answer)
  {
    /* Calculate indices */
  if (!quiet)
    fprintf(stdout, "Calculating indices...\n");
    geary_moran (catarray, &gr, &mc, n);

    if (flag.h->answer)
    {
      /* Calculate population means (Monte Carlo simulation) */
      if (!quiet)
        fprintf(stdout, "Calculating population means...\n");
      mean (catarray, &mgrr, &mmcr, &mgrn, &mmcn, n);
   
      /* Calculate standard errors */
      if (!quiet)
        fprintf(stdout, "Calculating standard errors...\n");
      std_err (catarray, &siggrr, &sigmcr, &siggrn, &sigmcn, n);
    }
  }
  /* Print Results */
  fprintf (out, "SPATIAL AUTOCORRELATION REPORT\n");
  fprintf (out, "\tLocation: [%s]\n\tMapset: [%s]\n", G_location (), mapset);
  fprintf (out, "\tn=%d labeled polygons in [%s]\n\n", Map->n_atts, name);
  if (!flag.stats->answer)
  {
    fprintf (out, "\tGeary Ratio=%-10g\t\tMoran Coefficient=%-10g\n\n",
	     gr, mc);
    if (flag.h->answer)
    {
      fprintf (out, "Sampling Distributions under different assumptions\n");
      fprintf (out, "\tRandomization: Mean(GR)=%-10g\tMean(MC)=%-10g\n",
	       mgrr, mmcr);
      fprintf (out, "\t               Var(GR)=%-10g\tVar(MC)=%-10g\n",
	       siggrr, sigmcr);
      fprintf (out, "\tNormalization: Mean(GR)=%-10g\tMean(MC)=%-10g\n",
	       mgrn, mmcn);
      fprintf (out, "\t               Var(GR)=%-10g\tVar(MC)=%-10g\n\n",
	       siggrn, sigmcn);
      fprintf (out, "Significance Tests\n");
      fprintf (out, "\tHo: spatial autocorrelation is not present\n");
      fprintf (out, "\tHo: Mean(MC) = -1(n-1)\t\tHo: Mean(GR) = 1\n");
      fprintf (out, "\tHa: Mean(MC) != -1(n-1)\t\tHa: Mean(GR) != 1\n\n");
      fprintf (out, "\tRandomization: Z(MC)=(%g+%g)/%g\n",
	       mc, 1. / (Map->n_atts - 1), sqrt (sigmcr));
      fprintf (out, "\t               Z(MC)=%g\n",
	       (mc + 1. / (Map->n_atts - 1)) / sqrt (sigmcr));
      fprintf (out, "\tRandomization: Z(GR)=(%g-1)/%g\n", gr, sqrt (siggrr));
      fprintf (out, "\t               Z(GR)=%g\n", (gr - 1) / sqrt (siggrr));
      fprintf (out, "\tNormalization: Z(MC)=(%g+%g)/%g\n",
	       mc, 1. / (Map->n_atts - 1), sqrt (sigmcn));
      fprintf (out, "\t               Z(MC)=%g\n",
	       (mc + 1. / (Map->n_atts - 1)) / sqrt (sigmcn));
      fprintf (out, "\tNormalization: Z(GR)=(%g-1)/%g\n", gr, sqrt (siggrn));
      fprintf (out, "\t               Z(GR)=%g\n", (gr - 1) / sqrt (siggrn));
    }
  }
  if (flag.cmatrix->answer)
  {
    printmatrix (out, Map->n_atts);
  }
  exit (1);
}
