#include "Vect.h"
#include "gis.h"

/*  Read_line ()
   **     read line info from digit file into line_points structure 
   **
   **  Returns     (int)  type  or
   **    -2  End of file
   **   -1 Out of memory

   **  if Line_In_Memory is TRUE, then offset is taken off of Mem_Line_Ptr
   **  in memory.
 */


int
Vect_x__Read_line (
		    struct Map_info *Map,
		    struct line_pnts *p,
		    struct line_cats *c,
		    long offset)
{
  int n_points, size;
  GRASS_V_NCATS n_cats;
  int type;

#ifdef MEMORY_IO
  if (Lines_In_Memory)
    return (Vect_x__Mem_Read_line (p, c, offset));
#endif

  /* reads must set in_head, but writes use default */
  dig__set_cur_head (&(Map->head));

  fseek (Map->dig_fp, offset, 0);

  if (0 >= dig__fread_port_I (&type, 1, Map->dig_fp))
    goto done;
  
  if (0 >= dig__fread_port_C (&n_cats, 1, Map->dig_fp))
    goto done;

  if ( c != NULL )
    {	  
      c->n_cats = n_cats;
      if (n_cats > 0)
      {
        if (0 > dig_alloc_cats (c, (int) n_cats + 1))
	  return (-1);

        if (0 >= dig__fread_port_S (c->field, n_cats, Map->dig_fp))
	  goto done;
        if (0 >= dig__fread_port_I (c->cat, n_cats, Map->dig_fp))
	  goto done;
      }
    }
  else
    {
      size = (PORT_SHORT + PORT_LONG) * n_cats;
      fseek (Map->dig_fp, size, SEEK_CUR);
    }
  
  if (0 >= dig__fread_port_I (&n_points, 1, Map->dig_fp))
    goto done;

  if (0 > dig_alloc_points (p, (int) n_points + 1))
    {
#ifdef DEBUG
      fprintf (stderr, "ALLOC_POINTS Returned error\n");
      fprintf (stderr, "Requested %d  Previous %d\n", (int) n_points + 1, p->alloc_points);
      debugf ("Offset: %ld  Type: %d  Npoints: %d\n", offset, itype, n_points);
#endif
      return (-1);
    }

  p->n_points = n_points;
  if (0 >= dig__fread_port_D (p->x, n_points, Map->dig_fp))
    goto done;
  if (0 >= dig__fread_port_D (p->y, n_points, Map->dig_fp))
    goto done;

  if (Map->head.with_z)
    {
      if (0 >= dig__fread_port_D (p->z, n_points, Map->dig_fp))
	goto done;
    }
  
  return (type);
done:
  return (-2);
}


/* write line info to DIGIT file */
/*  returns offset into file */
long 
Vect_x__Write_line (
		     struct Map_info *Map,
		     int type,
		     struct line_pnts *points,
		     struct line_cats *cats)
{
  long offset;

#ifdef MEMORY_IO
  if (Lines_In_Memory)
    {
      fprintf (stderr, "CATASTROPHIC ERROR! attempt to append to memory file\n");
      return (-1);
    }
#endif
  fseek (Map->dig_fp, 0L, 2);	/*  end of file */
  offset = ftell (Map->dig_fp);

  Vect_x__Rewrite_line (Map, offset, type, points, cats);

  return (offset);
}

/* write line info to DIGIT file */
/*  at the given offset */
/*  obviously the number of points must NOT have changed */
/*  from when line was read in */

int 
Vect_x__Rewrite_line (
		       struct Map_info *Map,
		       long offset,
		       int type,
		       struct line_pnts *points,
		       struct line_cats *cats)
{
  int  n_points;
  FILE *dig_fp;

  dig__set_cur_head (&(Map->head));
  dig_fp = Map->dig_fp;
  fseek (dig_fp, offset, 0);

  
  if (0 >= dig__fwrite_port_I (&type, 1, dig_fp))
    return -1;

  if (0 >= dig__fwrite_port_C (&cats->n_cats, 1, dig_fp))
    return -1;

  if (cats->n_cats > 0)
    {
      if (0 >= dig__fwrite_port_S (cats->field, cats->n_cats, dig_fp))
	return -1;
      if (0 >= dig__fwrite_port_I (cats->cat, cats->n_cats, dig_fp))
	return -1;
    }
  
  if ( type & ( DOT | DEAD_DOT | CENTROID | DEAD_CENTROID ) )
    n_points = 1;
  else  
    n_points = points->n_points;	  
  
  dig__fwrite_port_I (&n_points, 1, dig_fp);

  
  if (0 >= dig__fwrite_port_D (points->x, n_points, dig_fp))
    return -1;
  if (0 >= dig__fwrite_port_D (points->y, n_points, dig_fp))
    return -1;

  if (Map->head.with_z == WITH_Z)
    {
      if (0 >= dig__fwrite_port_D (points->z, n_points, dig_fp))
          return -1;
    }
  
  fflush (dig_fp);

  /* 
     ** if we have a memory file loaded, have to update it
     **
     **  NOTE WELL:  this is not currently set up to expand the memory file
     **  this is only designed to be used for true Re_writes, ie 
     **    updating a type or endpoints etc. not for adding new lines
     **
   */
#ifdef MEMORY_IO		/* not upgraded to 4.0 yet */
  if (Lines_In_Memory)
    {
      dig_mseek (0, offset, 0);
      dig_mwrite (&itype, sizeof (long), 1, 0);
      dig_mwrite (&n_points, sizeof (long), 1, 0);

      dig_mwrite (Ptmp, sizeof (double), points->n_points, 0);

      Ptmp = dig__double_convert (points->x, NULL, points->n_points);
      dig_mwrite (Ptmp, sizeof (double), points->n_points, 0);
    }
#endif
  return 0;
}



/* this is an experimental routine to use a memory copy of the digit file
   ** to maybe speed things up 
   **  see  memory_io.c for the rest
 */

#ifndef  MEMORY_IO		/* not updated to 4.0 yet */

int 
Vect_x__Mem_Read_line (
			struct line_pnts *p,
			long offset)
{
  G_fatal_error ("Vect_x__Mem_Read_line() was called");
}

#else

int 
Vect_x__Mem_Read_line (
			struct line_pnts *p,
			long offset)
{
  int n_points;
  long l_points;
  long itype;

  dig_mseek (0, offset, 0);

  if (0 >= dig_mread (&itype, sizeof (long), 1, 0))
      goto done;
  dig__long_convert (&itype, &itype, 1);
  itype = dig_old_to_new_type ((char) itype);
  if (0 >= dig_mread (&l_points, sizeof (long), 1, 0))
      goto done;
  dig__long_convert (&l_points, &l_points, 1);
  n_points = (int) l_points;

  if (0 > dig_alloc_points (p, n_points + 1))
    return (-1);

  p->n_points = n_points;
  if (0 >= dig_mread (p->x, sizeof (double), n_points, 0))
      goto done;
  dig__double_convert (p->x, p->x, n_points);
  if (0 >= dig_mread (p->y, sizeof (double), n_points, 0))
      goto done;
  dig__double_convert (p->y, p->y, n_points);

  return ((int) itype);
done:
  return (-2);
}
#endif /* MEMORY_IO */
