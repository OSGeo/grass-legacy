#include <stdio.h>
#include <string.h>
#include <sys/wait.h>
#include "gis.h"
#include "dbmi.h"
#include "Vect.h"
#include "global.h"
#include "lines.h"

/* move - move to next point in line */

struct COOR *move(struct COOR *point)
{
	if (direction == FORWARD)
	{
		if (point->fptr == NULL)		/* at open end of line */
			return(NULL);
		if (point->fptr->fptr == point)	/* direction change coming up */
			direction = BACKWARD;
		return(point->fptr);
	}
	else
	{
		if (point->bptr == NULL)
			return(NULL);
		if (point->bptr->bptr == point)
			direction = FORWARD;
		return(point->bptr);
	}
}

/* find_end - search for end of line, starting at a given point and */
/* moving in a given direction */

struct COOR *find_end(struct COOR *seed,int dir,int *result,int *n)
{
	struct COOR *start;

	start = seed;
	direction = dir;
	*result = *n = 0;
	while (!*result)
	{
		seed = move(seed);
		(*n)++;
		if (seed == start)
			*result = LOOP;
		else
		{
			if (seed == NULL)
				*result = OPEN;
			else
			{
				if (at_end(seed))
					*result = END;
			}
		}
	}
	return(seed);
}

/* at_end - test whether a point is at the end of a line;  if so, give */
/* the direction in which to move to go away from that end */

int at_end(struct COOR *ptr)
{
	if (ptr->fptr == ptr)
		return(BACKWARD);
	if (ptr->bptr == ptr)
		return(FORWARD);
	return(0);
}

int read_row (void *buf)
{
	void* p;
	if (last_read)
		return(0);
	if (first_read)
	{
		blank_line(buf);
		first_read = 0;
	}
	else
	{
		if (row_count >= n_rows)
		{
			last_read = 1;
			blank_line(buf);
		}
		else
		{
			/* The buf variable is a void pointer and thus */
			/* points to anything. Therefore, it's size is */
			/* unknown and thus, it cannot be used for pointer */
			/* arithmetic (some compilers treat this as an error */
			/* - SGI MIPSPro compiler for one). Make the */
			/* assumption that data_size is the proper number of */
			/* bytes and cast the buf variable to char * before */
			/* incrementing */
			p = ((char *) buf) + data_size;
			G_get_raster_row(input_fd,p,row_count++,data_type);
			p = buf;
			G_set_null_value(p,1,data_type);
			
			/* Again we need to cast p to char * under the */
			/* assumption that the increment is the proper */
			/* number of bytes. */
			p = ((char *) p) + (row_length + 1)*data_size;
			G_set_null_value(p,1,data_type);
		}
	}
	return(row_length + 2);
}

int blank_line(void *buf)
{
	G_set_null_value(buf, row_length + 2,data_type);
	
	return 0;
}

void *xmalloc(int size,char *label)
{
	char *addr;

	addr = G_malloc(size);
	return(addr);
}

int xfree(void *addr,char *label)
{
	/* free(addr);*/
	G_free(addr);

	return 0;
}

void *xrealloc(char *addr, int size, char *label)
{
	char *addr2;

	addr2 = G_realloc(addr,size);
	return(addr2);
}
