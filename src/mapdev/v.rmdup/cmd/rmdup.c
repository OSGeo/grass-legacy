/*
 * $Id$
 *-v.rmdup
 *
 * Author: James Darrell McCauley (mccauley@ecn.purdue.edu)
 *         USDA Fellow
 *         Department of Agricultural Engineering
 *         Purdue University
 *         West Lafayette, Indiana 47907-1146 USA
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose and without fee is hereby granted. This
 * software is provided "as is" without express or implied warranty.
 *
 * Removes duplicate arcs in a vector file. Modified from Vclean.c.
 *
 * Modification History:
 * 09 Jan 93 - Created by James Darrell McCauley <mccauley@ecn.purdue.edu>
 * 11 Mar 01 - Fixed segmentation faults from glibc2.2, fixed an infinite
 *             loop, added the threshold, added the output file, removed
 *             some flotsam (Roger S. Miller <rgrmill@rt66.com>)
 */

#include <stdlib.h>
#include <stdio.h>
#include <limits.h>
#include <math.h>

#include    "gis.h"
#include    "Vect.h"

#define MAIN

int main (argc, argv)
  int argc;
  char **argv;
{
  char *mapset;
  char *digin_name;
  char *digout_name;
  char buf[200];
  double thresh;
  char ans;

	struct GModule *module;

  /* check args and set flags  */
  struct Option *input;
  struct Option *output;
  struct Option *threshold;

  module = G_define_module();
  module->description =
	"Remove duplicate items GRASS vector file.";

  input = G_define_option ();
  input->key = "input";
  input->type = TYPE_STRING;
  input->required = YES;
  input->multiple = NO;
  input->description = "Vector file with duplicate arcs";

  output = G_define_option ();
  output->key = "output";
  output->type = TYPE_STRING;
  output->required = YES;
  output->multiple = NO;
  output->description = "Vector file to receive unique arcs";

  threshold = G_define_option ();
  threshold->key = "threshold";
  threshold->type = TYPE_DOUBLE;
  threshold->required = NO;
  threshold->multiple = NO;
  threshold->description = "Distance within which locations will be considered identical";
  threshold->answer=NULL;

  if (G_parser (argc, argv))
    exit (-1);

  digin_name = input->answer;
  digout_name = output->answer;

  thresh=-1.;
  if(threshold->answer != NULL)thresh=strtod(threshold->answer,NULL);

  G_gisinit (argv[0]);

  if ((mapset = G_find_file2 ("dig", digin_name, "")) == NULL)
  {
    sprintf (buf, "Could not find DIG file %s\n", digin_name);
    G_fatal_error (buf);
  }

  if (strcmp (mapset, G_mapset ()))
    G_fatal_error ("Can only modify files in your mapset");

  if (G_find_file2 ("dig", digout_name, "") != NULL)
  {
    fprintf (stderr, "Your output file %s already exists.\n",digout_name);
    fprintf (stderr, "Do you want to overwrite that file [y/n]? ");
    ans=tolower(fgetc(stdin));
    if(ans=='n')
    {
       sprintf(buf,"Specify an output file that doesn't already exist.\n");
       G_fatal_error (buf);
    }
  }

/* Do some file manipulations, then start the work */
  exit (export (digin_name, thresh, digout_name, mapset));
}


/*
 * Can't copy in place, cuz I'm not sure if I can depend on the *
 * ftruncate() call being available.  So will copy dig file *   to a temp
 * file, then clean from it into original, truncating *   it as I do so. *
 * 
 */

/* coming in, mapset is guaranteed to be the users own mapset */
export (digin_name, thresh, digout_name, mapset)
  char *digin_name;
  double thresh;
  char *digout_name;
  char *mapset;
{
  struct Map_info Map;
  struct Map_info NewMap;

  if (!mapset)
    G_fatal_error ("No mapset specified.");

  Vect_set_open_level (1);

/* Open a vector file for input */
  if (0 > Vect_open_old (&Map, digin_name, mapset))
    G_fatal_error ("Can't open existing input vector file");

/* open a vector file for output */
  if (0 > Vect_open_new (&NewMap, digout_name))
    G_fatal_error ("Can't open new output vector file");

/* Use the snap threshold from the old header if one wasn't provided, or 0 */
  if(thresh < 0. && Map.snap_thresh > 0.)
  {
     thresh=Map.snap_thresh;
     fprintf(stderr,"Using a threshold of %f from map header.\n",thresh);
  }
  else thresh=0.;

/* Copy the header from the old file to the new file */
  Vect_copy_head_data (&(Map.head), &(NewMap.head));

  printf("\n\n v.rmdup:  Please wait.\n");

  fprintf (stderr, "\n Stored %d unique arcs.\n", killdups (&Map, thresh, &NewMap));

  Vect_close (&NewMap);
  Vect_close (&Map);

  return (0);
}

killdups (Closet, thresh, Suitcase)
  struct Map_info *Closet;	/* funny names explained below */
  double thresh;
  struct Map_info *Suitcase;
{

/*  register int line, type; */
  int line=0, type=1, old_type;
  long old_offset, new_offset;
  int i, match, matches=0;
  struct line_pnts *Points_a, *Points_b;

  Points_a = Vect_new_line_struct ();
  Points_b = Vect_new_line_struct ();

  /*- Packing for a long trip, you may want to take one of every item
   *  of clothing that you have. So,
   *
   *          --->  take items off the rack sequentially and
   *         /      compare to all remaining items in closet.
   *        /       if you don't find something identical, put
   *       /        the item in the suitcase --+
   *      /                                    |
   *  |  /   |                              |  v   |
   *  |      |                              |      |
   *  |      |                              |      |
   *  |______|                              |______|
   *   Closet                               Suitcase
   *
   *  If we find something identical left in the closet, throw it
   *  on the floor and take the next item off the rack.
   */

  /* Make sure reads start at beginning */
  Vect_rewind (Closet);
  old_offset = ftell (Closet->dig_fp);
  while (0 < (old_type = V1_read_line (Closet, Points_a, old_offset)))
  {
    line++;
    match=0;
    old_offset = new_offset = ftell (Closet->dig_fp);

    while (match ==0 && type  > 0) /* inserted by RM to end loop at EOF */
    {
      if (0 < (type = V1_read_line (Closet, Points_b, new_offset)))
      {

	/*
	 * As soon as we find a match, we quit comparing items to item "a"
	 * (i.e., get out of this inner while loop).
	 */
	if (type < 16 && Points_a->n_points == Points_b->n_points)
	{
	  /*
	   * If the shirt is the right type for the trip (i.e., ALIVE) &&
	   * if number of buttons on shirts "a" and "b" are the same...
	   */
	  for (i = 0; i < Points_a->n_points; i+=1)
	  {
            /* RM added the threshold */
            if( fabs(Points_a->x[i] - Points_b->x[i]) > thresh ||
                fabs(Points_a->y[i] - Points_b->y[i]) > thresh)break;
	  }
          if(i==Points_a->n_points)match=1;
	}
      }

      /* point to the next item left in the closet */
      new_offset = ftell (Closet->dig_fp);
    }
    /* check the reasons for leaving the inner while loop */
    if (type == -1)		/* Item of clothing cannot be seen */
    {
      fprintf (stderr, "Out of memory on line %d\n", line);
      return (-1);
    }
    else if (!match)		/* if a match is not found, put item in
				 * Suitcase */
    {
      Vect_write_line (Suitcase, old_type, Points_a);
      matches++;
    }
/*
    fprintf(stderr, "%d line%c checked, %d line%c written\n",
                    line, ((line > 1) ? 's' : ' '),
                    matches, ((matches > 1) ? 's' : ' '));
*/

  }
  /* check the reasons for leaving the outer while loop */
  if (old_type == -1)		/* Item of clothing is too heavy to pick
				 * up */
  {
    fprintf (stderr, "Out of memory on line %d\n", line);
    return (-1);
  }
  else				/* tell me how many items we're taking on
				 * the trip */
    return matches;
}				/* NOTREACHED */
