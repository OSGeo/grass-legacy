/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Original author CERL, probably Dave Gerdes or Mike Higgins.
*               Update to GRASS 5.1 Radim Blazek and David D. Gray.
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include "Vect.h"
#include <stdlib.h>

static int V2__release_file_mem_nat (struct Map_info *);

/* 
** return 0 on success
**        non-zero on error
*/
int 
V1_close_nat (struct Map_info *Map)
{
  struct Coor_info CInfo;
    
  G_debug (1, "V1_close_nat(): name = %s mapset= %s", Map->name, Map->mapset);
  if (!VECT_OPEN (Map))
    return -1;

  if (Map->mode == GV_MODE_WRITE || Map->mode == GV_MODE_RW) {
    Vect_coor_info ( Map, &CInfo);
    Map->head.size = CInfo.size;
    dig__write_head (Map);

    Vect__write_head (Map);
    Vect_write_dblinks ( Map );
  }

  free (Map->name);
  free (Map->mapset);
  free (Map->digit_file);

  Map->name = NULL;
  Map->mapset = NULL;
  Map->digit_file = NULL;
  Map->open = VECT_CLOSED_CODE;

  dig_file_free ( &(Map->dig_fp) );
  return fclose (Map->dig_fp.file);
}

/* 
** return 0 on success
**        non-zero on error
*/
int 
V2_close_nat (struct Map_info *Map)
{
  struct Coor_info CInfo;
  struct Plus_head *Plus;

  G_debug (1, "V2_close_nat(): name = %s mapset= %s", Map->name, Map->mapset);
  
  Plus = &(Map->plus); 
  
  if (Map->mode & (GV_MODE_WRITE | GV_MODE_RW)) {
      Vect_coor_info ( Map, &CInfo);
      Map->head.size = CInfo.size;
      dig__write_head (Map);
      
      Vect__write_head (Map);

      Vect_write_dblinks ( Map );
 }

  /* close coor file */
  fclose (Map->dig_fp.file);
  dig_file_free ( &(Map->dig_fp) );

  /* Save topo if necessary */
  G_debug (2, "  built = %d", Plus->built);
  if (Plus->mode & (GV_MODE_WRITE | GV_MODE_RW) && Plus->built == GV_BUILD_ALL) {
      /* Get coor file checks */
      Vect_coor_info ( Map, &CInfo);
      Plus->coor_size = CInfo.size;
      Plus->coor_mtime = CInfo.mtime;
      
      Vect_save_topo ( Map );
      Vect_save_spatial_index ( Map );

  }
      
  dig_free_plus ( Plus );

  free (Map->name);
  free (Map->mapset);
  free (Map->digit_file);

  Map->name = NULL;
  Map->mapset = NULL;
  Map->digit_file = NULL;

  Map->open = VECT_CLOSED_CODE;

  return 0;
}

/* dunno if this should be static or not */
/*
static int			
V2__release_file_mem_nat (struct Map_info *Map)
{
  register int i;

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

  return 0;
}
*/
