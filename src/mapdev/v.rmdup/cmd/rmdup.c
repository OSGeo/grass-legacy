/*
 * $Id$
**-v.rmdup
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
** Removes duplicate arcs in a vector file. Modified from Vclean.c.
**
** Modification History:
** 09 Jan 93 - Created by James Darrell McCauley <mccauley@ecn.purdue.edu>
**/

#include <stdio.h>
#include <limits.h>
#include <math.h>

#include    "gis.h"
#include    "Vect.h"

#define MAIN

struct Map_info Map;
struct Map_info Outmap;
struct dig_head Head;

long ftell ();
double atof ();

int main (argc, argv)
  int argc;
  char **argv;
{
  int ret;
  char *mapset;
  char *dig_name;
  char buf[200];

	struct GModule *module;

  /* check args and set flags  */
  struct Option *map;

  module = G_define_module();
  module->description =
	"Remove duplicate items GRASS vector file.";

  map = G_define_option ();
  map->key = "map";
  map->type = TYPE_STRING;
  map->required = YES;
  map->multiple = NO;
  map->description = "Vector file to be cleaned from duplicate arcs";

  if (G_parser (argc, argv))
    exit (-1);

  dig_name = map->answer;

  G_gisinit (argv[0]);

  printf ("\n\n   v.rmdup:\n\n");

  if ((mapset = G_find_file2 ("dig", dig_name, "")) == NULL)
  {
    sprintf (buf, "Could not find DIG file %s\n", dig_name);
    G_fatal_error (buf);
  }

  if (strcmp (mapset, G_mapset ()))
    G_fatal_error ("Can only modify files in your mapset");

  exit (export (dig_name, mapset));
}


/*
 * Can't copy in place, cuz I'm not sure if I can depend on the *
 * ftruncate() call being available.  So will copy dig file *   to a temp
 * file, then clean from it into original, truncating *   it as I do so. *
 * 
 */

/* coming in, mapset is guaranteed to be the users own mapset */
export (dig_name, mapset)
  char *dig_name, *mapset;
{
  FILE *Out;
  FILE *In;
  char buf[1024];
  char *tmpfile;
  struct Map_info Map;
  int level;

  if (!mapset)
    G_fatal_error ("No mapset specified.");

  /* Copy orig  dig file  to .tmp */
  Vect_set_open_level (1);
  if (0 > Vect_open_old (&Map, dig_name, mapset))
    G_fatal_error ("Can't open vector file");

  tmpfile = G_tempfile ();
  Out = fopen (tmpfile, "w");

  if (0 > cp_filep (Map.dig_fp, Out))
    G_fatal_error ("File copy failed.  Cannot Proceed.");

  fclose (Out);
  Vect_close (&Map);

  if (0 > (level = Vect__open_update_1 (&Map, dig_name)))
    G_fatal_error ("Failed to open update");
  else
    fclose (Map.dig_fp);	/* close pointer to old digit file */

  if (NULL == (In = fopen (tmpfile, "r")))
  {
    unlink (tmpfile);
    G_fatal_error ("Failed opening temp file");
  }
  if (0 >= Vect_open_new (&Outmap, dig_name))
  {
    G_fatal_error ("Failed opening new file");
    exit (0);
  }

  Map.dig_fp = In;

  Vect_copy_head_data (&(Map.head), &(Outmap.head));

  fprintf (stderr, "killdups returns %d\n", killdups (&Map, &Outmap));

  Vect_close (&Outmap);
  Vect_close (&Map);

  unlink (tmpfile);

  return (0);
}

killdups (Closet, Suitcase)
  struct Map_info *Closet;	/* funny names explained below */
  struct Map_info *Suitcase;
{

  register int line, type;
  long old_offset, new_offset;
  int i, j, match, matches;
  struct line_pnts *Points_a, *Points_b;

  j = line = matches = 0;
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
  while (0 < (type = V1_read_line (Closet, Points_a, old_offset)))
  {
    line++;
    old_offset = new_offset = ftell (Closet->dig_fp);
    j=match = 0;			/* Assume we have no match */

/* commented following "while line" 5/2000:
 *   dig_fp->_cnt was once defined in _IO_FILE_ /usr/include/libio.h 
 *   int _cnt;    "number of characters in the buffer"
 *
 * -> but today?
 */
#if defined(sgi) || defined(CRAY) || defined(sparc)
    while (match == 0 && Closet->dig_fp->_cnt) 
#elif defined(__CYGWIN__) || defined(__FreeBSD__) || defined(__MAC_OS_X__)
    while (match == 0 && Closet->dig_fp->_bf._size)
#else
    while (match == 0 && Closet->dig_fp->_shortbuf[1])  /* try to upgrade in 5/2000 */
#endif
    {
      if (0 < (type = V1_read_line (Closet, Points_b, new_offset)))
      {
        j++;
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
	  for (i = 0; i < Points_a->n_points; ++i)
	  {
	    if (iszero (Points_a->x[i] - Points_b->x[i]) &&
		iszero (Points_a->y[i] - Points_b->y[i]))
	      match++;
	  }
          match = (match == Points_a->n_points) ? 1 : 0;
	}
      }
      /* prepare to look at the next item left in the closet */
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
      Vect_write_line (Suitcase, type, Points_a);
      matches++;
    }
   fprintf(stderr, "%d line%c chked, %d line%c writ. match=%d j=%d xa1=%f xb1=%f\n",
                    line, ((line > 1) ? 's' : NULL),
                    matches, ((matches > 1) ? 's' : NULL),
                    match ,j, Points_a->x[0],Points_b->x[0]);
  }
  /* check the reasons for leaving the outer while loop */
  if (type == -1)		/* Item of clothing is too heavy to pick
				 * up */
  {
    fprintf (stderr, "Out of memory on line %d\n", line);
    return (-1);
  }
  else				/* tell me how many items we're taking on
				 * the trip */
    return matches;
}				/* NOTREACHED */

cp_filep (in, out)
  FILE *in, *out;
{
  char buf[BUFSIZ];
  int red, ret;
  int no_file = 0;
  int err = 0;

  fseek (in, 0L, 0);
  {
    while (red = fread (buf, 1, BUFSIZ, in))
    {
      if (!(ret = fwrite (buf, 1, red, out)))
      {
	err++;
	break;
      }
    }
    fclose (in);
  }
  if (0 != fclose (out))
    err++;

  return (err);
}


int make_dead (type)
  int type;
{
  char buf[128];
  int newtype;

  switch (type)
  {
  case AREA:
    newtype = DEAD_AREA;
    break;
  case LINE:
    newtype = DEAD_LINE;
    break;
  case DOT:
    newtype = DEAD_DOT;
    break;
  default:
    sprintf (buf, "got type %d unexpectedly\n", (int) type);
    G_fatal_error (buf);
    break;
  }

  return newtype;
}

/* added 5/2000 */
int iszero(int x)
{ 
 return x != x; 
}
