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
  char *query = (char *) calloc (65536, sizeof (char));

  sprintf (query, "MOVE  BACKWARD %d IN g_cursor", Map->fInfo.post.nextRow);
  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
   /**********************************************************************/
  if (!Map->fInfo.post.geomRes
      || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_COMMAND_OK)
    {
      PQclear (Map->fInfo.post.geomRes);
      Map->fInfo.post.geomRes = NULL;
      free (query);
      return (-1);
    }
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
/***************************************************/
  sprintf (query, "SELECT  min(%s) from %s",
	   Map->fInfo.post.geom_id, Map->fInfo.post.geom_table);
  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
  if (!Map->fInfo.post.geomRes
      || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_TUPLES_OK)
    {
      PQclear (Map->fInfo.post.geomRes);
      Map->fInfo.post.geomRes = NULL;
      PQfinish (Map->fInfo.post.conn);
      free (query);
      return (-1);
    }

  Map->fInfo.post.nextRow = atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 0));
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
  free (query);
/***************************************************/
  return 0;
}

int
V2_rewind_post (struct Map_info *Map)
{
  Map->next_line = 1;
  return V1_rewind_post (Map);	/* make sure level 1 reads are reset too */
}

#endif
