/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S): 
*               Written by (in alphabetic order):
*                     Di Simone Alessio                      a.disimone@inwind.it
*                     Di Sorbo  Alessandro                  a.disorbo@inwind.it
*                     Ragni Domenico                         domrag@inwind.it
*                     Romano Enrico                         enr.omano@genie.it
*                     Serino Antonio                         antoseri@libero.it
* PURPOSE:      Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2002 by the authors
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include "Vect.h"

#ifdef HAVE_POSTGRES


/****************************************************************************************
* Function name: V1_rewind_post.
* Arguments    : Map.
* Return       : Status (-1 error 0 all ok), and modify Map structures.
*
* Description  :
*             Move cursor to first row, and set nextRow to 0
*             Rewind vector data file to cause reads to start at beginning.
***************************************************************************************/
int
V1_rewind_post (struct Map_info *Map)
{
  Map->fInfo.post.lastRead = 0;
  return 0;
}

int
V2_rewind_post (struct Map_info *Map)
{
  Map->fInfo.post.lastRead = 0;
  Map->next_line = 1;
  return 0;
}

#endif
