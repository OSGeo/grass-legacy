/*
 * Line thinning program
 *   Thinning algorithm
 *
 * Jean Ezell
 * US Army Corps of Engineers
 * CERL-EN Modelling and Simulation
 * Champaign, IL  61820
 * January - March, 1988
 *
 * References: B. Zvolanek and C. C. Lee, "Image Skeletonization for
 *             Object Position Measurement", SPIE Proceedings, Vol. 359,
 *             Applications of Digital Image Processing IV, A. G. Tescher,
 *             ed., San Diego, CA, August 82. (621.361AP58)
 *
 *             C. C. Lee, "Modified Distance Transform and Linking
 *             Algorithm for Image Skeletonization", SPIE Proceedings,
 *             Vol. 415, Coherent Infrared Radar Systems and Applications
 *             II, R. C. Harney, ed., Arlington, VA, April 83. (621.38489C66)
 *
 * Note:  Condition for stated for skeletonization (> 2) was probably
 *        intended to be "< -2".  "<= -2" actually looks better.
 *
 *             C. C. Lee, "A Sequential Thinning Algorithm for Image
 *             Skeletonization", SPIE Proceedings, Vol. 434, Applications
 *             of Digital Image Processing VI, San Diego, August 1983.
 *             (621.38AP583)
 *
 * The second pass of the distance transform has been combined with the
 * skeletonization algorithm to reduce the number of disk reads.  The
 * thinning algorithm moves alternately left and right along rows instead
 * of along diagonals as described in the papers.  This speeds up execution
 * by reducing i/o and gives a more uniform thinning of diagonal lines,
 * but it also produces wiggly vertical lines:
 *
 *            x
 *             x
 *            x
 *             x
 *            x
 *
 * These are straightened by the routine straight().
 *
 * Global variables:
 *   n_rows, n_cols  number of rows and columns in work file, including pads
 *   pad_size        size of pad on each edge of work file
 *   box_right,      edges of bounding box which will just cover area where
 *   box_left, etc   there are non-zero cells
 */

#include <stdio.h>
#include "gis.h"

#define LEFT 1
#define RIGHT 2

extern char *error_prefix;
static int n_rows, n_cols, pad_size;
static int box_right, box_left, box_top, box_bottom;

thin_lines()
{
  map_size(&n_rows,&n_cols,&pad_size);
  if (!distance())
  {
    fprintf(stdout,"Distance transform and skeletonization completed successfully\n");
    if (!link())
    {
      fprintf(stdout,"Linking completed successfully\n");
      if (!thin())
      {
        fprintf(stdout,"Thinning completed successfully\n");
        if (!straight())
          fprintf(stdout,"Straightening completed successfully\n");
      }
    }
  }
}

/*
 * distance - distance transform and skeletonization
 *
 * forward distance (destructive):
 *
 *   +---+---+---+---+---+---+---+
 *   |   |   |   | d |   |   |   |   row - 1 (top)
 *   +---+---+---+---+---+---+---+
 *   |   |   | d | D |   |   |   |   row     (bottom)
 *   +---+---+---+---+---+---+---+
 *                col
 *
 * reverse distance (destructive) and skeletonization (non-destructive):
 *
 *   +---+---+---+---+---+---+---+
 *   |   |   |   |D,s| d |   |   |   row - 2 (top)
 *   +---+---+---+---+---+---+---+
 *   |   |   |   | d |   |   |   |   row - 1 (second)
 *   +---+---+---+---+---+---+---+
 *   |   | s |   | S |   | s |   |   row     (middle)
 *   +---+---+---+---+---+---+---+
 *   |   |   |   |   |   |   |   |   row + 1
 *   +---+---+---+---+---+---+---+
 *   |   |   |   | s |   |   |   |   row + 2 (bottom)
 *   +---+---+---+---+---+---+---+
 *                col
 */

static distance()
{
  int row, col;
  CELL old;
  CELL *top, *middle, *second, *bottom, *get_a_row();
  int no_skel, no_skel_write;
  CELL *tmp[4];
  int i, result;

  box_right = box_bottom = 0;
  box_left = n_cols;
  box_top = n_rows;
  bottom = get_a_row(pad_size - 1);
  for (row = pad_size; row < n_rows - pad_size; row++)
  {					/* forward transform */
    top = bottom;			/* line above the one we're changing */
    bottom = get_a_row(row);		/* line we're working on now */
    old = bottom[pad_size - 1];
    for (col = pad_size; col < n_cols - pad_size; col++)
    {
      if (bottom[col])			/* not background pixel */
      {
        if (col < box_left)		/* find bounding box which will */
          box_left = col;		/*   cover part of cell file which */
        if (col > box_right)		/*   has lines in it */
          box_right = col;
        if (row < box_top)
          box_top = row;
        if (row > box_bottom)
          box_bottom = row;
        bottom[col] = (CELL) min((int) top[col],(int) old) + 1;
      }
      old = bottom[col];
    }					/* col-loop, forward transform */
    put_a_row(row,bottom);
  }					/* row-loop, forward transform */
  if (box_right < box_left || box_bottom < box_top)
  {
    fprintf(stderr,"%s:  distance:  could not find bounding box for lines\n",error_prefix);
    return(-1);				/* no bounding box found */
  }
  fprintf(stdout,"Bounding box:  l = %d, r = %d, t = %d, b = %d\n",box_left,box_right,box_top,box_bottom);
  no_skel = 2;				/* combine 2nd pass of distance with */
  no_skel_write = 4;			/*   skeletonization for speed */
  tmp[0] = (CELL *) G_malloc(n_cols * sizeof(CELL));
  tmp[1] = (CELL *) G_malloc(n_cols * sizeof(CELL));
  tmp[2] = (CELL *) G_malloc(n_cols * sizeof(CELL));
  for (row = box_bottom; row >= box_top - 2; row--)
  {
    top = get_a_row(row);
    second = get_a_row(row + 1);
    middle = get_a_row(row + 2);
    bottom = get_a_row(row + 4);
    old = top[box_right + 1];
    for (i = 0; i < n_cols; i++)
      tmp[0][i] = 0;
    for (col = box_right; col >= box_left; col--)
    {
      old = top[col] = (CELL) min((int) top[col],min((int) old,(int) second[col]) + 1);
      if (!no_skel)
      {
        if (middle[col])
        {
          result = (int) middle[col - 2] + (int) middle[col + 2] + (int) top[col] + (int) bottom[col] - (((int) middle[col]) << 2);
          if (result <= -2)
            tmp[0][col] = middle[col];
        }
      }
      else
        no_skel--;
    }					/* col loop */
    put_a_row(row,top);
    if (!no_skel_write)
      put_a_row(row + 4,tmp[2]);
    else
      no_skel_write--;
    tmp[3] = tmp[2];
    tmp[2] = tmp[1];
    tmp[1] = tmp[0];
    tmp[0] = tmp[3];
  }					/* row loop */
  free(tmp[0]);
  free(tmp[1]);
  free(tmp[2]);
  return(0);
}

/*
 * link - apply linking algorithm to skeleton
 *
 * five regions considered:  North, NorthEast, East, SouthEast, South
 * (destructive)
 *
 *   +---+---+---+---+---+---+---+
 *   |   |   |   |   |   |   |   |
 *   +---+---+---+---+---+---+---+
 *   |   | N |NE |NE |NE |   |   |   row - 3 (map[0])
 *   +---+---+---+---+---+---+---+
 *   |   | N |NE |NE |NE |   |   |   row - 2 (map[1])
 *   +---+---+---+---+---+---+---+
 *   |   | N |NE |NE |NE |   |   |   row - 1 (map[2])
 *   +---+---+---+---+---+---+---+
 *   |   |   | E | E | E |   |   |   row     (map[3])
 *   +---+---+---+---+---+---+---+
 *   |   | S |SE |SE |SE |   |   |   row + 1 (map[4])
 *   +---+---+---+---+---+---+---+
 *   |   | S |SE |SE |SE |   |   |   row + 2 (map[5])
 *   +---+---+---+---+---+---+---+
 *   |   | S |SE |SE |SE |   |   |   row + 3 (map[6])
 *   +---+---+---+---+---+---+---+
 *   |   |   |   |   |   |   |   |
 *   +---+---+---+---+---+---+---+
 *        col
*/

static link()
{
  int row, col;
  CELL *map[7];

  for (row = box_top + 3; row <= box_bottom - 3; row++)
  {
    map[0] = get_a_row(row - 3);
    map[1] = get_a_row(row - 2);
    map[2] = get_a_row(row - 1);
    map[3] = get_a_row(row);
    map[4] = get_a_row(row + 1);
    map[5] = get_a_row(row + 2);
    map[6] = get_a_row(row + 3);
    for (col = box_left; col <= box_right - 3; col++)
    {
      if (map[3][col])
      {
        if (!linked(col,map[2],map[3],map[4]))
        {
          if (!North(col,map))		/* if just doing North region is */
          {				/*   not enough, */
            if (!NEast(col,map))	/*   then try the northeast.  and if */
            {				/*   that's not enough, */
              if (!East(col,map))	/*   try east, etc. */
              {
                if (!SEast(col,map))
                {
                  if (South(col,map))
                    fprintf(stdout,"South (%d,%d)\n",row,col);
                }
                else
                  fprintf(stdout,"SouthEast (%d,%d)\n",row,col);
              }
              else
                fprintf(stdout,"East (%d,%d)\n",row,col);
            }
            else
              fprintf(stdout,"NorthEast (%d,%d)\n",row,col);
          }
          else
            fprintf(stdout,"North (%d,%d)\n",row,col);
        }					/* linking complete for (row,col) */
      }
    }					/* j-loop - columns of image */
    put_a_row(row - 3,map[0]);
    put_a_row(row - 2,map[1]);
    put_a_row(row - 1,map[2]);
    put_a_row(row,map[3]);
    put_a_row(row + 1,map[4]);
    put_a_row(row + 2,map[5]);
    put_a_row(row + 3,map[6]);
  }					/* i-loop - rows of image */
  return(0);
}

/* linked - test whether or not linking needs to be performed */

static linked(col,top,middle,bottom)
int col;
CELL *top, *middle, *bottom;
{
  int l, r;

  r = col + 1;
  l = col - 1;
  if ((middle[l] && middle[r]) || (top[col] && bottom[col]))
    return(1);				/* -- or | */
  if ((top[r] && bottom[l]) || (top[l] && bottom[r]))
    return(1);				/* test for / or \ */
  if ((top[r] || bottom[l]) && (top[l] || bottom[r]))
    return(1);				/* test for <, >, ^ or V */
  return(0);
}

/* North, NEast, etc. - perform linking in named region, if necessary */

static North(col,map)
int col;
CELL **map;
{
  if (map[2][col] == 0)
  {
    if (map[1][col] || map[0][col])
    {
      map[2][col] = map[1][col] = 12;
      return(1);
    }
  }
  return(0);
}

static NEast(col,map)
int col;
CELL **map;
{
  if (!(map[2][col] || map[2][col+1] || map[3][col+1]))
  {
    if (map[0][col+1])
    {
      map[2][col+1] = map[1][col+1] = 12;
      return(1);
    }
    if (map[0][col+2] || map[0][col+3] || map[1][col+3])
    {
      map[2][col+1] = map[1][col+2] = 12;
      return(1);
    }
    if (map[2][col+3])
    {
      map[2][col+1] = map[2][col+2] = 12;
      return(1);
    }
  }
  return(0);
}

static East(col,map)
int col;
CELL **map;
{
  if (map[3][col+1] == 0)
  {
    if (map[3][col+2] || map[3][col+3])
    {
      map[3][col+1] = map[3][col+2] = 12;
      return(1);
    }
  }
  return(0);
}

static SEast(col,map)
int col;
CELL **map;
{
  if (!(map[3][col+1] || map[4][col+1] || map[4][col] || map[5][col] || map[6][col]))
  {
    if (map[4][col+3] || map[5][col+3] || map[6][col+3] || map[6][col+2] ||  map[6][col+1])
    {
      map[4][col+1] = 12;
      return(1);
    }
  }
  return(0);
}

static South(col,map)
int col;
CELL **map;
{
  if (!(map[4][col+1] || map[4][col]))
  {
    if (map[6][col])
    {
      map[4][col] = 12;
      return(1);
    }
  }
  return(0);
}

/*
 * thin - thin lines to a single pixel width
 *
 * (destructive)
 *
 *   +---+---+---+---+---+---+---+
 *   |   |   |   |   |   |   |   |
 *   +---+---+---+---+---+---+---+
 *   |   |   | t | t | t |   |   |   row - 1
 *   +---+---+---+---+---+---+---+
 *   |   |   | t | T | t |   |   |   row
 *   +---+---+---+---+---+---+---+
 *   |   |   | t | t | t |   |   |   row + 1
 *   +---+---+---+---+---+---+---+
 *   |   |   |   |   |   |   |   |
 *   +---+---+---+---+---+---+---+
 *                col
 */

static thin()
{
  int T;
  int row, col;
  CELL *top, *second, *third, *bottom;

  for (row = box_top; row <= box_bottom; row++)
  {
    top = get_a_row(row - 1);
    second = get_a_row(row);
    third = get_a_row(row + 1);
    bottom = get_a_row(row + 2);
    if (row == box_top)
      make_links(top,second,third);
    make_links(second,third,bottom);
    if (row & 1)
    {					/* odd row; go forward */
      for (col = box_left; col <= box_right; col++)
      {
        if (T = second[col])		/* if pixel is not blank */
        {
          if (keep(T))
            second[col] = 1;
          else
          {
            second[col] = 0;
            second[col + 1] &= ~0100;
            third[col + 1] &= ~0200;
            third[col] &= ~01;
            third[col - 1] &= ~02;
          }
        }				/* end non-blank cell */
      }					/* end col-loop */
    }					/* end odd row */
    else				/* even row; go backwards */
    {
      for (col = box_right; col >= box_left; col--)
      {
        if (T = second[col])		/* if pixel is not blank */
        {
          if (keep(T))
            second[col] = 1;
          else
          {
            second[col] = 0;
            third[col + 1] &= ~0200;
            third[col] &= ~01;
            third[col - 1] &= ~02;
            second[col - 1] &= ~04;
          }
        }				/* end non-blank cell */
      }					/* end col-loop */
    }					/* end even rows */
    put_a_row(row,second);
    put_a_row(row + 1,third);
  }					/* row-loop */
  return(0);
}

/* make_links - make cell values contain information about their */
/* neighbors */

static make_links(top,middle,bottom)
CELL *top, *middle, *bottom;
{
  int col;

  for (col = box_left; col <= box_right; col++)
  {
    if (middle[col])
      middle[col] = (top[col] != 0) | ((top[col+1] != 0) << 1) | ((middle[col+1] != 0) << 2) | ((bottom[col+1] != 0) << 3) | ((bottom[col] != 0) << 4) | ((bottom[col-1] != 0) << 5) | ((middle[col-1] != 0) << 6) | ((top[col-1] != 0) << 7);
  }
}

/* keep - test conditions for deleting a point in the thinning algorithm */

static keep(T)
int T;
{
  int Tstar, test10, change, total, k;

  Tstar = ((T & 0x55) << 1) | T;
  test10 = Tstar << 1;			/* count 1 -> 0 transitions in Tstar */
  if (Tstar & 0x80)			/*   by counting the number of 1s in */
    test10 |= 0x1;			/*   ~Tstar & (Tstar rotated left) */
  test10 &= (~Tstar & 0xff);
  change = total = 0;
  for (k = 0; k < 8; k++)		/* count total number of ones */
  {					/*  and 1 -> 0 transitions in Tstar */
    if (Tstar & 0x1)
      ++total;
    if (test10 & 0x1)
      ++change;
    Tstar >>= 1;
    test10 >>= 1;
  }					/* k-loop - count 1s and (1->0)s */
  return(change > 1 || (change == 1 && total < 2));
}

/*
 * straight - straighten wiggly vertical lines produced by the thinning
 * algorithm
 *
 * (destructive)
 *
 *   +---+---+---+---+---+---+---+
 *   |   |   |   |   |   |   |   |
 *   +---+---+---+---+---+---+---+
 *   |   | s | s | s | s |   |   |   row - 1 (top)
 *   +---+---+---+---+---+---+---+
 *   |   | s | S | S | s |   |   |   row     (middle)
 *   +---+---+---+---+---+---+---+
 *   |   | s | s | s | s |   |   |   row + 1 (bottom)
 *   +---+---+---+---+---+---+---+
 *   |   |   |   |   |   |   |   |
 *   +---+---+---+---+---+---+---+
 *        col
 */

static straight()
{
  int row, col;
  CELL *top, *middle, *bottom;

  for (row = box_top + 1; row <= box_bottom - 1; row++)
  {
    top = get_a_row(row - 1);
    middle = get_a_row(row);
    bottom = get_a_row(row + 1);
    if (row & 1)
    {
      col = box_left;
      while (col <= box_right - 3)
      {
        if (RIGHT & skip(top + col,middle + col,bottom + col))
          col += 3;
        else
          col++;
      }
    }
    else
    {
      col = box_right - 3;
      while (col >= box_left)
      {
        if (LEFT & skip(top + col,middle + col,bottom + col))
          col -= 3;
        else
          col--;
      }
    }
    put_a_row(row,middle);
  }
  return(0);
}

/* skip - straighten line vertical (if there is one) and let straight() */
/* know if we can skip the next 2 window positions */

static skip(top,middle,bottom)
CELL *top, *middle, *bottom;
{
  if (!(top[3] || middle[3] || bottom[3]))
  {					/* right side zero */
    if (!(top[0] || middle[0] || bottom[0]))
    {					/* left side zero */
      if (top[1] + top[2] + middle[1] + middle[2] + bottom[1] + bottom[2] == 3)
      {
        if (top[1] && bottom[1] && middle[2])
        {
          middle[1] = 1;
          middle[2] = 0;
        }
        else
        {
          if (top[2] && bottom[2] && middle[1])
          {
            middle[2] = 1;
            middle[1] = 0;
          }
        }
      }
      return(RIGHT | LEFT);
    }
    return(RIGHT);
  }
  else
    return(0);
}

static min(i,j)
int i, j;
{
  if (i < j)
    return(i);
  else
    return(j);
}
