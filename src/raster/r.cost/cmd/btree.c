/* These routines manage the list of grid-cell candidates for 
 * visiting to calculate distances to surrounding cells.
 * A binary tree (btree) approach is used.  Components are
 * sorted by distance.
 *
 * insert ()
 *   inserts a new row-col with its distance value into the btree
 *
 * delete()
 *   deletes (if possible) a row-col entry in the tree
 *
 * get_lowest()
 *   retrieves the entry with the smallest distance value
 */


#include "gis.h"
#include "local_proto.h"
#include "memory.h"

static struct cost *start_cell = NULL ;
/*  static int show(struct cost *); */
static int do_quit(double,int,int);

struct cost *insert(double min_cost,int row,int col)
{
	struct cost *new_cell, *next_cell ;

/*  	new_cell = (struct cost *)(G_malloc(sizeof(struct cost))); */
	new_cell = get();
	if (new_cell == NULL) {
			fprintf(stderr,"new_cell is NULL\n");
}
	new_cell->min_cost = min_cost;
	new_cell->row = row;
	new_cell->col = col;
	new_cell->above = NULL ;
	new_cell->higher = NULL ;
	new_cell->lower = NULL ;

	if (start_cell == NULL)
	{
		start_cell = new_cell;
		return(new_cell);
	}

	for(next_cell=start_cell;;)
	{
		if (min_cost < next_cell->min_cost)
		{
			if (next_cell->lower != NULL)
			{
				next_cell = next_cell->lower ;
				continue ;
			}
			new_cell->above = next_cell ;
			next_cell->lower = new_cell ;
			return(new_cell) ;
		}
		if (min_cost > next_cell->min_cost)
		{
			if (next_cell->higher != NULL)
			{
				next_cell = next_cell->higher ;
				continue ;
			}
			new_cell->above = next_cell ;
			next_cell->higher = new_cell ;
			return(new_cell) ;
		}

		/* If we find a value that is exactly equal to new value */
		if (next_cell->lower != NULL)
			next_cell->lower->above = new_cell ;
		new_cell->lower = next_cell->lower ;
		new_cell->above = next_cell ;
		next_cell->lower = new_cell ;

		return(new_cell) ;
	}
}


struct cost *find(double min_cost,int row,int col)
{
	struct cost *next_cell ;

	for(next_cell=start_cell;;)
	{
		if (min_cost <= next_cell->min_cost)
		{
			if (next_cell->row == row && next_cell->col == col)
				return(next_cell) ;

			if (next_cell->lower != NULL)
			{
				next_cell = next_cell->lower ;
				continue ;
			}
			fprintf(stderr, "1 ");
			return NULL;
			do_quit(min_cost, row, col) ;
		}
		else
		{
			if (next_cell->higher != NULL)
			{
				next_cell = next_cell->higher ;
				continue ;
			}
			fprintf(stderr, "2 ");
			return NULL;
			do_quit(min_cost, row, col) ;
		}
	}
}

static int do_quit(double min_cost,int row,int col)
{
	fprintf(stderr,"Can't find %d,%d:%f\n", row,col,min_cost) ;
	show_all() ;
	exit(-1) ;
}

struct cost *
get_lowest (void)
{
	struct cost *next_cell ;

	if(start_cell == NULL)
		return(NULL) ;

	for(next_cell=start_cell;
		next_cell->lower != NULL;
		next_cell = next_cell->lower) ;

	if(next_cell->row == -1)
	{
/*
fprintf(stderr, "Deleting %d\n", next_cell) ;
show_all() ;
*/
		delete(next_cell) ;
/*
fprintf(stderr, "Deleted %d\n", next_cell) ;
show_all() ;
*/
		return(get_lowest()) ;
	}

	return(next_cell) ;
}

int delete(struct cost *delete_cell)
{
	if (delete_cell == NULL)
	{
		fprintf(stderr,"Illegal delete request\n") ;
		return 0;
	}
/*
       \      \      \      \   
     1  X   4  X   7  X   10 X  
       N N    / N    N \    / \ 

         /      /      /      / 
     2  X   5  X   8  X   11 X  
       N N    / N    N \    / \
						   
        N      N      N      N  
     3  X   6  X   9  X   12 X  
       N N    / N    N \    / \
*/

	if (delete_cell->higher == NULL)        /* 123456       */
	{
		if (delete_cell->lower == NULL)     /* 123          */
		{
			if (delete_cell->above == NULL) /*   3          */
			{
				start_cell = NULL ;
				give(delete_cell) ;
				return 0;
			}
			if (delete_cell->above->higher == delete_cell)
			{                               /* 1            */
				delete_cell->above->higher = NULL ;
				give(delete_cell) ;
				return 0;
			}
			else
			{                               /*  2           */
				delete_cell->above->lower = NULL ;
				give(delete_cell) ;
				return 0;
			}
		}
		else                                /*    456       */
		{
			if (delete_cell->above == NULL) /*      6       */
			{
				start_cell = delete_cell->lower ;
				delete_cell->lower->above = NULL ;
				give(delete_cell) ;
				return 0;
			}
			if (delete_cell->above->higher == delete_cell)
			{                               /*    4         */
				delete_cell->above->higher = delete_cell->lower ;
				delete_cell->lower->above = delete_cell->above ;
				give(delete_cell) ;
				return 0;
			}
			else
			{                               /*     5        */
				delete_cell->above->lower = delete_cell->lower ;
				delete_cell->lower->above = delete_cell->above ;
				give(delete_cell) ;
				return 0;
			}
		}
	}
	if (delete_cell->lower == NULL)         /*       789    */
	{
		if (delete_cell->above == NULL)     /*         9    */
		{
			start_cell = delete_cell->higher ;
			delete_cell->higher->above = NULL ;
			give(delete_cell) ;
			return 0;
		}
		if (delete_cell->above->higher == delete_cell)
		{                                   /*       7      */
			delete_cell->above->higher = delete_cell->higher ;
			delete_cell->higher->above = delete_cell->above ;
			give(delete_cell) ;
			return 0;
		}
		else
		{                                   /*        8     */
			delete_cell->above->lower = delete_cell->higher ;
			delete_cell->higher->above = delete_cell->above ;
			give(delete_cell) ;
			return 0;
		}
	}
/*
 * At this point we are left with 10,11,12 which can be expanded
 *    \   
 *  10 X         X          X   
 *    / \       / \        / \  
 *          A  O   -   B  -   O     C ALL OTHERS
 *      /     / N            N \
 *  11 X  
 *    / \
 *
 *     N  
 *  12 X  
 *    / \
 */

	if (delete_cell->lower->higher == NULL) /* A */
	{
		if (delete_cell == start_cell)      /* 12A */
		{
			delete_cell->lower->higher = delete_cell->higher ;
			delete_cell->higher->above = delete_cell->lower ;
			start_cell = delete_cell->lower ;
			delete_cell->lower->above = NULL ;
			give(delete_cell) ;
			return 0;
		}
		if (delete_cell->above->higher == delete_cell)
		{                                   /* 10A */
			delete_cell->lower->higher = delete_cell->higher ;
			delete_cell->higher->above = delete_cell->lower ;
			delete_cell->above->higher = delete_cell->lower ;
			delete_cell->lower->above = delete_cell->above ;
			give(delete_cell) ;
			return 0;
		}
		else
		{                                   /* 11A */
			delete_cell->lower->higher = delete_cell->higher ;
			delete_cell->higher->above = delete_cell->lower ;
			delete_cell->above->lower = delete_cell->lower ;
			delete_cell->lower->above = delete_cell->above ;
			give(delete_cell) ;
			return 0;
		}
	}
	if (delete_cell->higher->lower == NULL) /* A */
	{
		if (delete_cell == start_cell)      /* 12B */
		{
			delete_cell->higher->lower = delete_cell->lower ;
			delete_cell->lower->above = delete_cell->higher ;
			start_cell = delete_cell->higher ;
			delete_cell->higher->above = NULL ;
			give(delete_cell) ;
			return 0;
		}
		if (delete_cell->above->lower == delete_cell)
		{                                   /* 11B */
			delete_cell->higher->lower = delete_cell->lower ;
			delete_cell->lower->above = delete_cell->higher ;
			delete_cell->above->lower = delete_cell->higher ;
			delete_cell->higher->above = delete_cell->above ;
			give(delete_cell) ;
			return 0;
		}
		else
		{                                   /* 10B */
			delete_cell->higher->lower = delete_cell->lower ;
			delete_cell->lower->above = delete_cell->higher ;
			delete_cell->above->higher = delete_cell->higher ;
			delete_cell->higher->above = delete_cell->above ;
			give(delete_cell) ;
			return 0;
		}
	}
	/* If we get this far, the node cannot be safely removed.  Just leave
	 * an internal mark noting that it is no longer good.
	 */
	delete_cell->row = -1 ;
	return 0;
}

int show_all(void)
{
	if (start_cell == NULL)
	{
		fprintf(stderr, "Nothing to show\n") ;
		return 1;
	}
	fprintf(stderr, "start: %p %d,%d,%f %p %p\n",
		start_cell,
		start_cell->row,
		start_cell->col,
		start_cell->min_cost,
		start_cell->lower,
		start_cell->higher) ;
	show(start_cell->lower) ;
	show(start_cell->higher) ;

	return 0;
}

int show(struct cost *next)
{
	if(next == NULL)
		return 0;
	fprintf(stderr, "%p %d,%d,%f %p %p %p\n",
		next,
		next->row,
		next->col,
		next->min_cost,
		next->lower,
		next->higher,
		next->above) ;
	show(next->lower) ;
	show(next->higher) ;

	return 0;
}

int check_all(char *str)
{
	fprintf(stderr,"\n") ;
	if(start_cell->above != NULL)
	{
		fprintf(stderr,"Bad Start Cell\n") ;
		exit(-1) ;
	}
	check(str, start_cell) ;

	return 0;
}

int check(char *str,struct cost *start)
{
	if (start == NULL)
		return 0;

	if (start->lower != NULL)
	{
		if (start->min_cost < start->lower->min_cost)
		{
			fprintf(stderr,"%s %f-%f lower cost higher or equal\n", str,
				start->min_cost, start->lower->min_cost) ;
			show_all() ;
			exit(-1) ;
		}
		if (start->lower->above != start)
		{
			fprintf(stderr,"%s lower above pointer wrong\n", str) ;
			show_all() ;
			exit(-1) ;
		}
	}
	if (start->higher != NULL)
	{
		if (start->min_cost >= start->higher->min_cost)
		{
			fprintf(stderr,"%s %f-%f higher cost lower\n", str,
				start->min_cost, start->higher->min_cost) ;
			show_all() ;
			exit(-1) ;
		}
		if (start->higher->above != start)
		{
			fprintf(stderr,"%s higher above pointer wrong\n", str) ;
			show_all() ;
			exit(-1) ;
		}
	}
	check(str, start->lower) ;
	check(str, start->higher) ;

	return 0;
}
