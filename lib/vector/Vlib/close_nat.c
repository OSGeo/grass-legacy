/*
* $Id$
*
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

/* 
** return 0 on success
**        non-zero on error
*/
int 
V2_close_nat (struct Map_info *Map)
{
  if (Map->mode & (MODE_WRITE | MODE_RW))
    {
      /* TODO update files */
      Vect__write_head (Map);
    }


  /* close files */
  fclose (Map->dig_fp);

  //V2__release_file_mem_nat (Map);	/* from dig_P_fini()  4.0 */

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
