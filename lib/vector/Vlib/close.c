#include "Vect.h"
#include <stdlib.h>

static int V2__release_file_mem (struct Map_info *);

/*  Close vector data file */
/*  returns 0 on success,  -1 on error  */
int 
Vect_close (struct Map_info *Map)
{

  switch (Map->level)
    {
    case 1:
      return V1_close (Map);
      /*NOTREACHED */
      break;
    case 2:
      return V2_close (Map);
      /*NOTREACHED */
      break;
    case 3:
    default:
      return -1;
    }
  /* NOTREACHED */

  return 0;
}

/* return 0 on success, Non-zero on error */
int 
V1_close (struct Map_info *Map)
{
  if (!VECT_OPEN (Map))
    return -1;

  if (Map->mode == MODE_WRITE || Map->mode == MODE_RW)
    Vect__write_head (Map);

  free (Map->name);
  free (Map->mapset);
  free (Map->digit_file);

  Map->name = NULL;
  Map->mapset = NULL;
  Map->digit_file = NULL;
  Map->open = VECT_CLOSED_CODE;

  return fclose (Map->dig_fp);
}

int 
V2_close (struct Map_info *Map)
{
  if (Map->mode & (MODE_WRITE | MODE_RW))
    {
      /* TODO update files */
      Vect__write_head (Map);
    }


  /* close files */
  fclose (Map->dig_fp);

  V2__release_file_mem (Map);	/* from dig_P_fini()  4.0 */

  free (Map->name);
  free (Map->mapset);
  free (Map->digit_file);

  Map->name = NULL;
  Map->mapset = NULL;
  Map->digit_file = NULL;

  Map->open = VECT_CLOSED_CODE;

  return 0;
}

static int			/* dunno if this should be static or not */
V2__release_file_mem (struct Map_info *Map)
{
  register int i;

  /* release all memory */
  if (Map->Line != NULL)
    {
      free (Map->Line);
    }
  if (Map->Area != NULL)
    {
      for (i = 1; i <= Map->n_areas; i++)
	if (Map->Area[i].alloc_lines > 0)
	  if (Map->Area[i].lines != NULL)
	    free (Map->Area[i].lines);
      free (Map->Area);
    }
  if (Map->Isle != NULL)
    {
      for (i = 1; i <= Map->n_isles; i++)
	if (Map->Isle[i].alloc_lines > 0)
	  if (Map->Isle[i].lines != NULL)
	    free (Map->Isle[i].lines);
      free (Map->Isle);
    }
  if (Map->Node != NULL)
    {
      for (i = 1; i <= Map->n_nodes; i++)
	if (Map->Node[i].alloc_lines > 0)
	  {
	    if (Map->Node[i].lines != NULL)
	      free (Map->Node[i].lines);
	    if (Map->Node[i].angles != NULL)
	      free (Map->Node[i].angles);
	  }
      free (Map->Node);
    }
  if (Map->Att != NULL)
    {
      free (Map->Att);
    }

/*  just leave points alloced, in case they are going to still be using them 
   free (Points.x);
   free (Points.y);
   Points.n_points = Points.alloc_points = 0;
 */

  return 0;
}

#ifdef TMP_CLOSE		/* no one was using these anyway */
int 
V2_tmp_close (int Map)
{
  fclose (Map->dig_fp);

  return 0;
}

int 
V2_tmp_open (int Map)
{
  /* BUG, to use tmp_open along with dig__P_writable(), 
     **  you must be sure that the writable flag is set to 
     **  the appropriate value each time you call tmp_open
   */
  Map->dig_fp = fopen (Map->digit_file, RW_str);

  return 0;
}
#endif
