#include "gis.h"
#include "Vect.h"
#include "local_dig.h"

#define FIRST  { if (First_Time) Vect_init (); }

static int First_Time = 1;

/* Internal function definitions */

int vect__get_no_line_points (struct Map_info*, long);
int vect_x__get_no_line_points (struct Map_info*, long);
#ifdef  MEMORY_IO
int vect_x__mem_get_no_line_points (long);
#endif


/***** Public function */

/* Gets number of points in any specifies line.
   This is NOT affected by constraints */

int 
v2_get_no_line_points (struct Map_info *Map, int line)
{
  if (line < 1 || line > Map->n_lines)		/* ALL DONE */
    return -2;

  return (vect__get_no_line_points (Map, Map->Line[line].offset));
}

/***** Private functions */

int 
vect__get_no_line_points (struct Map_info *Map, long offset)
{
    FIRST;
    return (vect_x__get_no_line_points (Map, offset));
}



/*  Get number of points in a polyline
**     read line info from digit file or memory 
**
**  Returns     (int)  number of points  or
**	 -2  End of file

**  if Line_In_Memory is TRUE, then offset is taken off of Mem_Line_Ptr
**  in memory.
*/

int 
vect_x__get_no_line_points (struct Map_info *Map, long offset)
{
    int n_points;
    long itype;

#ifdef MEMORY_IO
    if (Lines_In_Memory)
	return (Vect_x__mem_get_no_line_points (offset));
#endif

    /* reads must set in_head, but writes use default */
    dig__set_cur_in_head (&(Map->head));

    fseek (Map->dig_fp, offset, 0);

    /* Read and discard line type */
    if (0 >= dig__fread_port_L (&itype, 1, Map->dig_fp)) goto done;

    /* Read number of points */
    if (0 >= dig__fread_port_I (&n_points, 1, Map->dig_fp)) goto done;
    return ((int) n_points);
done:
    return (-2);
}

/* this is an experimental routine to use a memory copy of the digit file
** to maybe speed things up 
**  see  memory_io.c for the rest
*/

#ifndef  MEMORY_IO	/* not updated to 4.0 yet */

int 
vect_x__mem_get_no_line_points (long offset)
{
  G_fatal_error ("vect_x__mem_get_no_line_points() was called");
}

#else

int 
vect_x__mem_get_no_line_points (long offset)
{
  int n_points;
  long l_points;
  long itype;

    dig_mseek (0, offset, 0);

    /* Read and discard line type */
    if (0 >= dig_mread (&itype, sizeof(long), 1, 0) )
	goto done;
    
    /* Read number of points */
    if (0 >= dig_mread (&l_points, sizeof(long), 1, 0) )
	goto done;
    dig__long_convert (&l_points, &l_points, 1);
    n_points = (int) l_points;

    return (n_points);
done:
    return (-2);
}
#endif /* MEMORY_IO */

