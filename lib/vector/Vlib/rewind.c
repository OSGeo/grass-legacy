#include "Vect.h"

/*  Rewind vector data file to cause reads to start at beginning */
/* returns 0 on success, -1 on error */
int 
Vect_rewind (struct Map_info *Map)
{
  if (!VECT_OPEN (Map))
    return -1;

  switch (Map->level)
    {
    case 1:
      return V1_rewind (Map);
      /* NOTREACHED */
      break;
    case 2:
      return V2_rewind (Map);
      /* NOTREACHED */
      break;
    case 3:
    default:
      return -1;
    }
  /* NOTREACHED */
}


/* returns 0 on success,  -1 on error  */
int 
V1_rewind (struct Map_info *Map)
{
  struct dig_head dhead;

  return (fseek (Map->dig_fp, GRASS_V_DIG_HEAD_LENGTH, 0));
}

int 
V2_rewind (struct Map_info *Map)
{
  Map->next_line = 1;
  return V1_rewind (Map);	/* make sure level 1 reads are reset too */
}
