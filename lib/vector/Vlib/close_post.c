/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S): 
*               Written by (in alphabetic order):
*                     Di Simone Alessio                 a.disimone@inwind.it
*                     Di Sorbo  Alessandro              a.disorbo@inwind.it
*                     Ragni Domenico                    domrag@inwind.it
*                     Romano Enrico                     enr.omano@genie.it
*                     Serino Antonio                    antoseri@libero.it
*
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2002 by the authors
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include "Vect.h"
#include <stdlib.h>

#ifdef HAVE_POSTGRES

/******************************************************************************
* Function name: V1_close_post.
* Arguments    : Map.
* Return       : Status (-1 error 0 all ok), and modify Map structures.
*
* Description  :
*              Commit transaction, close connection and free Map structure
*
*******************************************************************************/

int
V1_close_post (struct Map_info *Map)
{
  G_debug (1, "V1_close_post():");

  if (!VECT_OPEN (Map)) return -1;

  if (Map->mode & (GV_MODE_WRITE | GV_MODE_RW))
    Vect__write_head (Map);

  free (Map->name);
  free (Map->mapset);
  Map->name = NULL;
  Map->mapset = NULL;
  Map->digit_file = NULL;
  Map->open = VECT_CLOSED_CODE;

  PQfinish (Map->fInfo.post.conn);
  return 0;
}

/* 
** return 0 on success
**        non-zero on error
*/
int
V2_close_post (struct Map_info *Map)
{
  struct Coor_info CInfo;
  struct Plus_head *Plus;

  G_debug (1, "V2_close_post(): name = %s mapset= %s", Map->name,
	   Map->mapset);

  Plus = &(Map->plus);

  /* Save topo if necessary */
  if (Plus->mode & (GV_MODE_WRITE | GV_MODE_RW)) {
      Vect_coor_info ( Map, &CInfo); /* Size and time is 0L for PostGIS) */
      Plus->coor_size = CInfo.size;
      Plus->coor_mtime = CInfo.mtime;
      
      Vect_save_topo (Map);
      Vect_save_spatial_index ( Map );
      dig_free_plus (Plus);
  }

  return (V1_close_post (Map));

}

#endif
