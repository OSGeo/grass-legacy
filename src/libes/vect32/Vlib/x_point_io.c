#include "Vect.h"
#include "gis.h"

/*  Read_line ()
**     read line info from digit file into line_points structure 
**
**  Returns     (int)  type  or
**	 -2  End of file
**	-1 Out of memory

**  if Line_In_Memory is TRUE, then offset is taken off of Mem_Line_Ptr
**  in memory.
*/


int Vect_x__Read_line (
    struct Map_info *Map,
    struct line_pnts *p,
    long offset)
{
    int n_points;
    long itype;

#ifdef MEMORY_IO
    if (Lines_In_Memory)
	return (Vect_x__Mem_Read_line (p, offset));
#endif

    /* reads must set in_head, but writes use default */
    dig__set_cur_in_head (&(Map->head));

    fseek (Map->dig_fp, offset, 0);

    if (0 >= dig__fread_port_L (&itype, 1, Map->dig_fp)) goto done;
    itype = dig_old_to_new_type ((char) itype);
    if (0 >= dig__fread_port_I (&n_points, 1, Map->dig_fp)) goto done;


    if (0 > dig_alloc_points (p, (int) n_points+1))
    {
#ifdef DEBUG
fprintf (stderr, "ALLOC_POINTS Returned error\n");
fprintf (stderr, "Requested %d  Previous %d\n", (int) n_points+1, p->alloc_points);
debugf ("Offset: %ld  Type: %d  Npoints: %d\n", offset, itype, n_points);
#endif
	return (-1);
    }

    p->n_points = n_points;
    if (0 >= dig__fread_port_D (p->x, n_points, Map->dig_fp)) goto done;
    if (0 >= dig__fread_port_D (p->y, n_points, Map->dig_fp)) goto done;

    return ((int) itype);
done:
    return (-2);
}


/* write line info to DIGIT file */
/*  returns offset into file */
long Vect_x__Write_line(
    struct Map_info *Map, 
    char type,
    struct line_pnts *points)
{
    long offset;

#ifdef MEMORY_IO
    if (Lines_In_Memory)
    {
	fprintf (stderr, "CATASTROPHIC ERROR! attempt to append to memory file\n");
	return (-1);
    }
#endif
    fseek (Map->dig_fp, 0L, 2) ;		/*  end of file */
    offset = ftell (Map->dig_fp);

    Vect_x__Rewrite_line (Map, offset, type, points);

    return (offset);
}

/* write line info to DIGIT file */
/*  at the given offset */
/*  obviously the number of points must NOT have changed */
/*  from when line was read in */

int Vect_x__Rewrite_line (
    struct Map_info *Map,
    long offset,
    char type,
    struct line_pnts *points)
{
    long itype;
    FILE * dig_fp;

    dig_fp = Map->dig_fp;
    fseek (dig_fp, offset, 0);

    itype = (long) dig_new_to_old_type (type);
    if (0 >= dig__fwrite_port_L (&itype, 1, dig_fp)) return -1;

    dig__fwrite_port_I (&points->n_points, 1, dig_fp);

    if (0 >= dig__fwrite_port_D (points->x, points->n_points, dig_fp)) return -1;
    if (0 >= dig__fwrite_port_D (points->y, points->n_points, dig_fp)) return -1;

    fflush (dig_fp);

    /* 
    ** if we have a memory file loaded, have to update it
    **
    **  NOTE WELL:  this is not currently set up to expand the memory file
    **  this is only designed to be used for true Re_writes, ie 
    **    updating a type or endpoints etc. not for adding new lines
    **
    */
#ifdef MEMORY_IO	/* not upgraded to 4.0 yet */
    if (Lines_In_Memory)
    {
	dig_mseek (0, offset, 0);
	dig_mwrite(&itype, sizeof(long), 1, 0);
	dig_mwrite(&n_points, sizeof(long), 1, 0);

	dig_mwrite(Ptmp, sizeof(double), points->n_points, 0);

	Ptmp = dig__double_convert (points->x, NULL, points->n_points);
	dig_mwrite(Ptmp, sizeof(double), points->n_points, 0);
    }
#endif
    return 0;
}



/* this is an experimental routine to use a memory copy of the digit file
** to maybe speed things up 
**  see  memory_io.c for the rest
*/

#ifndef  MEMORY_IO	/* not updated to 4.0 yet */

int Vect_x__Mem_Read_line (
    struct line_pnts *p,
    long offset)
{
    G_fatal_error ("Vect_x__Mem_Read_line() was called");
}

#else

int Vect_x__Mem_Read_line (
    struct line_pnts *p,
    long offset)
{
    int n_points;
    long l_points;
    long itype;

    dig_mseek (0, offset, 0);

    if (0 >= dig_mread (&itype, sizeof(long), 1, 0) )
	goto done;
    dig__long_convert (&itype, &itype, 1);
    itype = dig_old_to_new_type ((char) itype);
    if (0 >= dig_mread (&l_points, sizeof(long), 1, 0) )
	goto done;
    dig__long_convert (&l_points, &l_points, 1);
    n_points = (int) l_points;

    if (0 > dig_alloc_points (p,  n_points+1))
	return (-1);

    p->n_points = n_points;
    if (0 >= dig_mread (p->x, sizeof(double),  n_points, 0))
	goto done;
    dig__double_convert (p->x, p->x,  n_points);
    if (0 >= dig_mread (p->y, sizeof(double),  n_points, 0))
	goto done;
    dig__double_convert (p->y, p->y,  n_points);

    return ((int) itype);
done:
    return (-2);
}
#endif /* MEMORY_IO */

