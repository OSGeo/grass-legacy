/*
 * Line thinning program
 *   Thinning algorithm
 *
 * Mike Baba 
 * DBA Systems 
 * Fairfax, Va 
 * Jan 1990 
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
 *             E.S. Deutsch, "Thinning Algorithms on Rectanglur, Hexogonal,
 *             and Triangular Arrays", Communications of the AMC, Vol.15,
 *             No. 9, September, 1972.
 *
 *             J.D. Greenlee, "Raster and Vector Processing for Scanned 
 *             Linework", Photogrammmetric Engineering and Remote Sensing,
 *             No. 10,  October, 1987, pp. 1383-1387.
 *
 * The second pass of the distance transform has been combined with the
 * skeletonization algorithm to reduce the number of disk reads.  
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

#define true 1
#define false 0
#define MAX_PASSES    10
#define DELETED_PIX   9999

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
			if (!thindba())
				fprintf(stdout,"Thinning completed successfully\n");
		}
	}
}

/*
 * distance- distance transform and skeletonization
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
	int no_skel_write;
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
		for  (col = pad_size; col < n_cols - pad_size; col++)
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
	/* 2nd pass reverse */
	for (row = box_bottom; row >= box_top - 2; row--)
	{
		top = get_a_row(row);
		second = get_a_row(row + 1);
		old = top[box_right + 1];
		for (col = box_right; col >= box_left; col--)
		{
			old = top[col] = (CELL) min((int) top[col],min((int) old,(int) second[col]) + 1);
		}					/* col loop */
		put_a_row(row,top);
	}					/* row loop */



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
			if (middle[col])
			{
				result = (int) middle[col - 2] + (int) middle[col + 2] + (int) top[col] + (int) bottom[col]
				    - (((int) middle[col]) << 2);
				if (result <= -2)
					tmp[0][col] = middle[col];
			}
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
				if (1)        /* link applied to all non-zero pixels */
				{
					if (!North(col,map))		/* if just doing North region is */
					{				/*   not enough, */
						if (!NEast(col,map))	/*   then try the northeast.  and if */
						{				/*   that's not enough, */
							if (!East(col,map))	/*   try east, etc. */
							{
								if (!SEast(col,map))
								{
/* code folded from here */
	/* code folded from here */
	if (South(col,map))
	{
#ifdef DEBUG
		fprintf (stdout,"South (%d,%d)\n",row,col);
#endif
	}
	/* unfolding */
/* unfolding */
								}
								else
								{
#ifdef DEBUG
/* code folded from here */
	/* code folded from here */
	fprintf (stdout,"SouthEast (%d,%d)\n",row,col);
#endif
	/* unfolding */
/* unfolding */
								}
							}
							else
							{
#ifdef DEBUG
								fprintf(stdout,"East (%d,%d)\n",row,col);
#endif
							}
						}
						else
						{
#ifdef DEBUG
							fprintf(stdout,"NorthEast (%d,%d)\n",row,col);
#endif
						}
					}
					else
					{
#ifdef DEBUG
						fprintf(stdout,"North (%d,%d)\n",row,col);
#endif
					}
				}		       		/* linking complete for (row,col) */
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
			map[2][col] = map[1][col] = map[2][col+1] = map[1][col+1] = 12;
			return(1);
		}
		if (map[0][col+2] || map[0][col+3] || map[1][col+3])
		{
			map[2][col+1] = map[2][col+2] = map[1][col+2] = map[1][col+1] = 12;
			return(1);
		}
		if (map[2][col+3])
		{
			map[2][col+1] = map[2][col+2] = map[3][col+1] = map[3][col+2]= 12;
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
	if (!(map[4][col] || map[4][col+1] || map[3][col+1] ))
	{
		if (map[6][col+1])
		{
			map[4][col+1] = map[5][col+1] = map[4][col] = map[5][col]= 12;
			return(1);
		}
		if (map[5][col+3] || map[6][col+3] || map[6][col+2])
		{
			map[4][col+1] = map[4][col+2] = map[5][col+2] = map[5][col+1] = 12;
			return(1);
		}
		if (map[4][col+3])
		{
			map[4][col+1] = map[4][col+2] = map[3][col+1] = map[3][col+2]= 12;
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

static thindba()
{
	int T;
	int row, col;
	int pass, cycle, deleted1, deleted2, deleted;
	CELL *top, *second, *third, *bottom;

	for (pass = 0; pass <= MAX_PASSES; pass++)
	{
		printf ("PASS = %d \n", pass);
		if  ((deleted1 == 0) && (deleted2 == 0) && (pass > 0)) break;
		deleted1 = deleted2 = 0;
		for (cycle = 1; cycle <=2; cycle++)
		{
			printf ("  Cycle = %d \n",cycle);
			for (row = box_top; row <= box_bottom; row++)
			{
				top = get_a_row(row - 1);
				second = get_a_row(row);
				third = get_a_row(row + 1);
				bottom = get_a_row(row + 2);
				for (col = box_left; col <= box_right; col++)
				{
					if (second[col])		/* if pixel is not blank */
					{
						T = make_one_link(top,second,third,col);
						if (cycle == 1)
						{
							if (ThinStep1(T))
							{
								second[col] = DELETED_PIX;  /* pixel is to be deleted */
								deleted1++;                 /* but include in make_one_link */
							}
							else
								second[col] = 1;
						}
						if (cycle == 2)
						{
							if (ThinStep2(T))
							{
								second[col] = DELETED_PIX;
								deleted2++;
							}
							else
								second[col] = 1;
						}
					}     			/* end blank pixel */
					if (top[col-1] == DELETED_PIX) top[col-1]=0;
				}			        /* end col loop */
				if (top[col-1] == DELETED_PIX) top[col-1]=0;
				put_a_row(row-1,top);
				put_a_row(row,second);
			}                                 /* end row loop */
			/* deleted pixels become zero */
			for (col = box_left; col <= box_right; col++)
				if (second[col] == DELETED_PIX) second[col]=0;
			if (cycle == 1) deleted = deleted1;
			else deleted = deleted2;
			printf ("     Deleted %d  pixels \n", deleted);
		}                                   /* end cycle loop */
	}                                     /* end pass loop */
	return(0);
}


/* make_one_link - return neighborhood information for pixel at (middle,col) */

make_one_link(top,middle,bottom,col)
CELL *top, *middle, *bottom;
int col;
{
	int T;

	T = 0;
	if (middle[col])
		T = ((middle[col+1] != 0) | ((top[col+1] != 0) << 1) | ((top[col] != 0) << 2) | ((top[col-1] != 0) << 3) |
		    ((middle[col-1] != 0) << 4) | ((bottom[col-1] != 0) << 5) | ((bottom[col] != 0) << 6) | ((bottom[col+1] != 0) << 7));

	return (T);
}

/*ThinStep1 */
/*-----------------------------------------------------------------------
     Eliminates pixel with neighborhood values below, on first cycle only.
  ----------------------------------------------------------------------*/
ThinStep1(val1)
int val1;  /* IN */

{
	static int      STEP1 [50] = {
		0,3,6,7,12,13,14,15,24,28,30,48,
		56,60,62,65,67,80,96,97,99,112,120,124,126,129,131,133,135,141,
		143,192,193,195,224,225,227,240,241,243,248,249,251,252,254		};
	int i;

	for (i=0; i<50; i++)
	{
		if (val1 == STEP1[i])
			return (true);
	}
	return (false);
}

/*ThinStep2*/
/*----------------------------------------------------------------
   Eliminates pixel with neighborhood values below, on second cycle only.
  ---------------------------------------------------------------*/
ThinStep2(val2)
int val2;  /* IN */

{
	static int    STEP2 [51] = {
		0,3,5,6,7,12,14,15,20,22,24,28,30,31,48, 
		52,54,56,60,62,63,65,80,88,96,112,120,129,131,135,143,159,191,
		192,193,195,199,207,208,216,224,225,227,231,239,240,248		};

	int i;

	for (i=0; i<51; i++)
	{
		if (val2 == STEP2[i])
			return (true);
	}
	return (false);
}

static min(i,j)
int i, j;
{
	if (i < j)
		return(i);
	else
		return(j);
}
