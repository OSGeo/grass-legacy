
 /***********************************************************************
 *  #include "atts.h"
 *
 *  atts_init (fp, atts_index)
 *      FILE *fp ;
 *      struct atts_index *atts_index;
 *
 * This routine reads through a digit attribute file accumulating addresses
 *  about areas, lines, and points.   These are stored in arrays within
 * the 'atts_index' structure.
 *
 *   returns:   0  on a successful read of the attribute file
 *             -1  on error
 *
 *   NOTICE: that the offsets start at 1 (area_off[1]) not 0 (area_off[0]) ;
 *
 **********************************************************************/

#include "gis.h"
#include "dig_atts.h"

#define ALLOC_AMT	512

atts_init(fp, atts_ptr)
	FILE *fp ;
	struct atts_index *atts_ptr ;
{
	int stat ;
	int max_areas ;
	int max_lines ;
	int max_points ;

	struct attribute att ;

	long ftell() ;
	char *check_alloc() ;
	int  read_att_struct() ;

	rewind (fp) ;

/* Make initial memory allocations */
/* line and area offsets */
	atts_ptr->area_alloc = ALLOC_AMT ;
	atts_ptr->area_off = (long *) G_calloc (ALLOC_AMT, sizeof (long));
	atts_ptr->line_alloc = ALLOC_AMT ;
	atts_ptr->line_off = (long *) G_calloc (ALLOC_AMT, sizeof (long));
	atts_ptr->point_alloc = ALLOC_AMT ;
	atts_ptr->point_off = (long *) G_calloc (ALLOC_AMT, sizeof (long));


	max_areas = 0 ;
	max_lines = 0 ;
	max_points = 0 ;


	while( (stat =  read_att_struct( fp, &att)) == 0 ) 
	{

		switch(att.type)
		{

		case 'A':
			++max_areas ;

			atts_ptr->area_off = (long *)check_alloc(
				max_areas,
				&atts_ptr->area_alloc,
				(char *)atts_ptr->area_off,
				sizeof(long)) ;

			atts_ptr->area_off[max_areas] = att.offset ;
			break ;

		case 'L':
			++max_lines ;

			atts_ptr->line_off = (long *)check_alloc(
				max_lines,
				&atts_ptr->line_alloc,
				(char *)atts_ptr->line_off,
				sizeof(long)) ;

			atts_ptr->line_off[max_lines] = att.offset ;
			break ;
			
		case 'P':
			++max_points ;

			atts_ptr->point_off = (long *)check_alloc(
				max_points,
				&atts_ptr->point_alloc,
				(char *)atts_ptr->point_off,
				sizeof(long)) ;

			atts_ptr->point_off[max_points] = att.offset ;
			break ;

		default:
			fprintf(stderr, "ERROR: Undefined type in attribute file\n") ;
			return(-1) ;
		}

		if (stat <0)
			return(-1) ;
	}


	atts_ptr->max_areas = max_areas ;
	atts_ptr->max_lines = max_lines ;
	atts_ptr->max_points = max_points ;
	atts_ptr->max_atts = max_areas + max_lines ;
	return(0) ;
}


free_atts(atts_ptr)
	struct atts_index *atts_ptr ;
{
	free(atts_ptr->area_off) ;
	free(atts_ptr->line_off) ;
	free(atts_ptr->point_off) ;
}

static
char *
check_alloc(num, n_alloc, arr, size)
	int num ;
	int *n_alloc ;
	char *arr ;
	int size ;
{
	int orig_alloc ;
	char *ptr ;
	char *calloc() ;

	if (num < *n_alloc)
		return (arr) ;

	orig_alloc = *n_alloc ;

	while (num >= *n_alloc)
		*n_alloc += ALLOC_AMT;

	ptr = calloc (*n_alloc, size) ;
	if (ptr == NULL)
	{
		fprintf(stderr,"ERROR: ran out of memory [requested (%d,%d)]\n",
			*n_alloc, size) ;
		exit(-1) ;
	}

	{
		register char *a ;
		register char *b ;
		register int n ;
		n = orig_alloc * size ;
		a = ptr ;
		b = arr ;
		while(n--)
			*a++ = *b++ ;
	}

	free(arr) ;
	return(ptr) ;
}

