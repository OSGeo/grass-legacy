/*-i.texture- calculate textural features on a GRASS raster map
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
** Modification History:
** Early 91  - James Darrell McCauley <jdm5548@diamond.tamu.edu> did
**             original coding.
** 24 Jun 91 - J. Michael Carstensen <jmc@imsor.dth.dk> supplied fix for
**             correlation function.
** 28 Mar 92 - James Darrell McCauley <mccauley@ecn.purdue.edu> modified
**             for use with GRASS.
**
** Code taken from pgmtexture.c in the PBMPLUS package by James Darrell
** McCauley and Jef Poskanser, whose copyright notice is included below.
**
** Algorithms for calculating features (and some explanatory comments) are
** taken from:
**
**   Haralick, R.M., K. Shanmugam, and I. Dinstein. 1973. Textural features
**   for image classification.  IEEE Transactions on Systems, Man, and
**   Cybertinetics, SMC-3(6):610-621.
**
***************************************************************************
** pgmtexture notice:
**
** Copyright (C) 1991 Texas Agricultural Experiment Station, employer for
** hire of James Darrell McCauley
**
** Permission to use, copy, modify, and distribute this software and its
** documentation for any purpose and without fee is hereby granted, provided
** that the above copyright notice appear in all copies and that both that
** copyright notice and this permission notice appear in supporting
** documentation.  This software is provided "as is" without express or
** implied warranty.
**
** THE TEXAS AGRICULTURAL EXPERIMENT STATION (TAES) AND THE TEXAS A&M
** UNIVERSITY SYSTEM (TAMUS) MAKE NO EXPRESS OR IMPLIED WARRANTIES
** (INCLUDING BY WAY OF EXAMPLE, MERCHANTABILITY) WITH RESPECT TO ANY
** ITEM, AND SHALL NOT BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL
** OR CONSEQUENTAL DAMAGES ARISING OUT OF THE POSSESSION OR USE OF
** ANY SUCH ITEM. LICENSEE AND/OR USER AGREES TO INDEMNIFY AND HOLD
** TAES AND TAMUS HARMLESS FROM ANY CLAIMS ARISING OUT OF THE USE OR
** POSSESSION OF SUCH ITEMS.
**
*/

#define MAIN

#include "gis.h"
#include <math.h>

main (argc, argv)
  int argc;
  char *argv[];
{
  /* Global variable & function declarations */

  int inputfd;			/* the input file descriptors */
  char *inmapset;		/* the input mapset name */
  CELL *cell_row;
  char Cellmap_orig[50];
  int i, j;			/* Loop control variables */
  int rows, cols;		/* Original dimensions of image */
  int **data;			/* Data structure containing image */
  struct Option *op1;
  char *me;
  void texture ();

  G_gisinit (argv[0]);
  me = G_program_name ();

  /* define options */
  op1 = G_define_option ();
  op1->key = "rast";
  op1->type = TYPE_STRING;
  op1->required = YES;
  op1->multiple = NO;
  op1->gisprompt = "old,cell,raster";
  op1->description = "input raster file";

  /* call parser */
  if (G_parser (argc, argv))
    exit (-1);

  strcpy (Cellmap_orig, op1->answer);

  /* open input cell map */
  if ((inmapset = G_find_cell (Cellmap_orig, "")) == NULL)
  {
    fprintf (stderr, "%s: %s - Unable to open the input raster map\n",
	     me, Cellmap_orig);
    exit (1);
  }
  inputfd = G_open_cell_old (Cellmap_orig, inmapset);
  if (inputfd < 0)
    exit (1);

  /* get the rows and columns in the current window */
  rows = G_window_rows ();
  cols = G_window_cols ();

  /* allocate the space for one row of cell map data */
  cell_row = G_allocate_cell_buf ();

  /* Allocate appropriate memory for the structure containing the image */
  data = (int **) G_malloc (rows * sizeof (int *));
  for (i = 0; i <= rows; i++)
  {
    data[i] = (int *) G_malloc (cols * sizeof (int));
    if (data[i] == NULL)
      G_fatal_error ("Insufficent memory for allocation of data structure");
  }
  /* Read in cell map values */
  fprintf (stderr, "Reading the raster map...");
  for (j = 0; j < rows; j++)
  {
    G_get_map_row (inputfd, cell_row, j);
    for (i = 0; i < cols; i++)
      data[j][i] = (int) cell_row[i];
  }
  /* close input cell map and release the row buffer */
  G_close_cell (inputfd);
  free (cell_row);
  fprintf (stderr, "done\n");
  texture (data, rows, cols);

  for (i = 0; i < rows; i++)
  {
    for (j = 0; j < cols; j++)
      fprintf (stdout, "%d ", data[i][j]);
    fprintf (stdout, "\n");
  }
  /* Release memory resources */
  free (data);
  fprintf (stderr, "Calculations complete\n");

  exit (0);
}
