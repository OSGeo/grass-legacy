

#include	<stdio.h>
#include	"dig_structs.h"


load_node_struct( ptr, angle, line_no)
	struct  P_node  *ptr ;
	double  angle ;
	int     line_no ;
{

	int  alloc_angles ;

	char *dig_falloc() ;
	char *dig_alloc_space() ;

    /*  alloc_lines represents allocation for angles also, save it  */
    /*  alloc_angles is dummy variable for alloc_space()  */
	alloc_angles = ptr->alloc_lines ;

    ptr->alive = 1;

  /*  first time thru allocate 3 lines per node,
  *   otherwise allocate only 1 if needed
  */
    if (ptr->n_lines)
    {
	++ptr->n_lines ;

	ptr->lines = (plus_t *) dig_alloc_space ( ptr->n_lines, &ptr->alloc_lines,
    		1, (char *)ptr->lines, sizeof (plus_t));

	ptr->angles = (float *) dig_alloc_space ( ptr->n_lines, &alloc_angles,
    		1, (char *)ptr->angles, sizeof (float));

    }
    else
    {
	++ptr->n_lines ;
    /* assume average 3 lines per node ?? this may be over stepping bounds */
	ptr->alloc_lines = 3 ;

	ptr->lines = (plus_t *) dig_falloc (  3,  sizeof (plus_t)) ;
	ptr->angles = (float *) dig_falloc (  3,  sizeof (float)) ;

    }
	

    ptr->lines[ptr->n_lines - 1] = line_no ;
    ptr->angles[ptr->n_lines - 1] = (float)angle ;

    return (0);
}
