/*****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):  
*               Written by (in alphabetic order):
*                     Di Simone Alessio                   a.disimone@inwind.it
*                     Di Sorbo  Alessandro                a.disorbo@inwind.it
*                     Ragni Domenico                      domrag@inwind.it
*                     Romano Enrico                       enr.omano@genie.it
*                     Serino Antonio                      antoseri@libero.it
*
* PURPOSE:     Higher level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2002 by the authors
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

#ifdef HAVE_POSTGRES
#include "libpq-fe.h"
#include <time.h>
#include <string.h>
#include <stdio.h>
int connectionEstablished = 0;	/* No Connection Made */

int setup (struct Map_info *Map);
/************************************************************************************ 
* Function name: setup.
* Arguments    : Map.
* Return       : Status (-1 error 0 all ok), and modify Map structures.
*
**************************************************************************************/

int
setup (struct Map_info *Map)
{
  char GEOM_ID[20] = "GEOM_ID";
  char GEOM_TYPE[20] = "GEOM_TYPE";
  char GEOM_GEOM[20] = "GEOM_GEOM";
  char CAT_ID[20] = "CAT_ID";
  char CAT_FIELD[20] = "CAT_FIELD";
  char CAT_CAT[20] = "CAT_CAT";
  fprintf (stderr, "setup\n");
/* If some option parameter are not defined..define here (be quite and drive)*/
  if ((Map->fInfo.post.geom_id == NULL)
      || (strlen (Map->fInfo.post.geom_id) == 0))
    Map->fInfo.post.geom_id = strdup (GEOM_ID);
  if ((Map->fInfo.post.geom_type == NULL)
      || (strlen (Map->fInfo.post.geom_type) == 0))
    Map->fInfo.post.geom_type = strdup (GEOM_TYPE);
  if ((Map->fInfo.post.geom_geom == NULL)
      || (strlen (Map->fInfo.post.geom_geom) == 0))
    Map->fInfo.post.geom_geom = strdup (GEOM_GEOM);

  if ((Map->fInfo.post.cat_id == NULL)
      || (strlen (Map->fInfo.post.cat_id) == 0))
    Map->fInfo.post.cat_id = strdup (CAT_ID);
  if ((Map->fInfo.post.cat_field == NULL)
      || (strlen (Map->fInfo.post.cat_field) == 0))
    Map->fInfo.post.cat_field = strdup (CAT_FIELD);
  if ((Map->fInfo.post.cat_cat == NULL)
      || (strlen (Map->fInfo.post.cat_cat) == 0))
    Map->fInfo.post.cat_cat = strdup (CAT_CAT);
  Map->fInfo.post.geomRes = NULL;
  /* Try to make a connection to the specified database */
  Map->fInfo.post.conn =
    PQsetdbLogin (Map->fInfo.post.host, Map->fInfo.post.port, NULL, NULL,
		  Map->fInfo.post.database, Map->fInfo.post.user,
		  Map->fInfo.post.password);

  if (PQstatus (Map->fInfo.post.conn) == CONNECTION_BAD)
    {
      Map->fInfo.post.geomRes = NULL;
      return (-1);
    }
  connectionEstablished = 1;
  return (0);
}

/************************************************************************************ 
* Function name: V1_open_old_post.
* Arguments    : Map.
* Return       : Status (-1 error 0 all ok), and modify Map structures.
*
* Notes        : For create a new Db use the external shell script, that also insert 
*                the existtable sql function used here.
*
* Description:
*                This function use information inside Map structure to open a connection 
*                to specified database; create geometry and category table if not exist; 
*                Begin a transaction, inside it create a binary cursor to extract  rows 
*                from geometry table. Finally sets some variable with appropiates values. 
**************************************************************************************/

int
V1_open_old_post (struct Map_info *Map, int update)
{
  char *query = (char *) calloc (65536, sizeof (char));
  fprintf (stderr, "V1_open_old_post\n");
  if (!connectionEstablished)
    {
      if (setup (Map))
	{
	  return (-1);
	}
    }
 /*******************************************************************************************/
  /*Create binary cursor for selecting a geometry row */
  sprintf (query, "BEGIN");
  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
  if (!Map->fInfo.post.geomRes
      || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_COMMAND_OK)
    {
      PQclear (Map->fInfo.post.geomRes);
      Map->fInfo.post.geomRes = NULL;
      PQfinish (Map->fInfo.post.conn);
      free (query);
      return (-1);
    }

  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
  sprintf (query,
	   "DECLARE  g_cursor   CURSOR FOR SELECT %s, %s, NumPoints(%s) , Dimension(%s)  FROM  %s;",
	   Map->fInfo.post.geom_id, Map->fInfo.post.geom_type,
	   Map->fInfo.post.geom_geom, Map->fInfo.post.geom_geom,
	   Map->fInfo.post.geom_table);

  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
  if (!Map->fInfo.post.geomRes
      || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_COMMAND_OK)
    {
      PQclear (Map->fInfo.post.geomRes);
      Map->fInfo.post.geomRes = NULL;
      PQfinish (Map->fInfo.post.conn);
      free (query);
      return (-1);
    }


  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
  /*Count the rows */
  sprintf (query, "SELECT  count(%s),max(%s),min(%s) from %s",
	   Map->fInfo.post.geom_id, Map->fInfo.post.geom_id,
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

  Map->fInfo.post.nGeom = atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 0));
  if (Map->fInfo.post.nGeom == 0)
    Map->fInfo.post.nextId = 0;
  else
    Map->fInfo.post.nextId =
      atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 1)) + 1;
  Map->fInfo.post.nextRow = atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 2));
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
  Map->head.with_z = WITHOUT_Z;
  Map->fInfo.post.nCat = 0;
  Map->fInfo.post.catRes = NULL;
  free (query);
  return (0);
}

/* Open old file on level 2.
*  Map->name and Map->mapset must be set before
*  
*  Return: 0 success
*         -1 error */
int
V2_open_old_post (struct Map_info *Map, int update)
{
  int ret;

  G_debug (1, "V2_open_old_post(): name = %s mapset = %s", Map->name,
	   Map->mapset);

  /* open topo */
  ret = Vect_open_topo (Map);

  if (ret == -1)
    {				/* topo file is not available */
      G_debug (1, "Cannot open topo file for vector '%s@%s'.\n",
	       Map->name, Map->mapset);
      return -1;
    }

  ret = V1_open_old_post (Map, update);
  if (ret != 0)
    {
      dig_free_plus (&(Map->plus));
      return -1;
    }

  Map->next_line = 1;

  return 0;
}

/* Open PostGIS vector.
*
*  Return: 0 success
*         -1 error
*/
int
V1_open_new_post (struct Map_info *Map, char *name, int with_z)
{
  char *query = (char *) calloc (65536, sizeof (char));

  G_debug (1, "V1_open_new_post()");

 /************************************************************************************/

  if (!connectionEstablished)
    {
      if (setup (Map))
	return (-1);
    }
 /************************************************************************************/
  /*Check if geom table doesn't exist */
  sprintf (query, "SELECT existtable('%s')", Map->fInfo.post.geom_table);
  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);

  if (!Map->fInfo.post.geomRes
      || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_TUPLES_OK)
    {
      PQclear (Map->fInfo.post.geomRes);
      PQfinish (Map->fInfo.post.conn);
      Map->fInfo.post.geomRes = NULL;
      free (query);
      return (-1);
    }

  if (!atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 0)))
    {
      /*Create geometry table if not exist */
      PQclear (Map->fInfo.post.geomRes);
      Map->fInfo.post.geomRes = NULL;
      sprintf (query,
	       "CREATE TABLE %s  ( %s int4 PRIMARY KEY, %s int4 CHECK ((%s > 0) AND (%s < 5)), %s  geometry);\n"
	       "GRANT SELECT ON  %s TO PUBLIC;\n"
	       "CREATE INDEX %s_sidx ON %s USING GIST (%s GIST_GEOMETRY_OPS ) WITH ( ISLOSSY );\n",
	       Map->fInfo.post.geom_table, Map->fInfo.post.geom_id,
	       Map->fInfo.post.geom_type, Map->fInfo.post.geom_type,
	       Map->fInfo.post.geom_type, Map->fInfo.post.geom_geom,
	       Map->fInfo.post.geom_table, Map->fInfo.post.geom_table,
	       Map->fInfo.post.geom_table, Map->fInfo.post.geom_geom);
      fprintf (stderr, "%s", query);
      Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
      if (!Map->fInfo.post.geomRes
	  || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_COMMAND_OK)
	{
	  PQclear (Map->fInfo.post.geomRes);
	  PQfinish (Map->fInfo.post.conn);
	  Map->fInfo.post.geomRes = NULL;
	  free (query);
	  return (-1);
	}
    }
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
 /***********************************************************************************/
  /*Check if category table doesn't exist */
  sprintf (query, "SELECT existtable('%s')", Map->fInfo.post.cat_table);
  Map->fInfo.post.catRes = PQexec (Map->fInfo.post.conn, query);
  if (!Map->fInfo.post.catRes
      || PQresultStatus (Map->fInfo.post.catRes) != PGRES_TUPLES_OK)
    {
      PQclear (Map->fInfo.post.catRes);
      PQfinish (Map->fInfo.post.conn);
      Map->fInfo.post.catRes = NULL;
      free (query);
      return (-1);
    }


  if (!atoi (PQgetvalue (Map->fInfo.post.catRes, 0, 0)))
    {				/*Create category table if not exist */
      PQclear (Map->fInfo.post.catRes);
      Map->fInfo.post.catRes = NULL;
      sprintf (query,
	       "CREATE TABLE %s( %s int4, %s int4, %s int4, PRIMARY KEY (%s, %s), CONSTRAINT if_geom_exists  FOREIGN KEY(%s) REFERENCES %s  ON UPDATE CASCADE ON DELETE CASCADE );\n"
	       "GRANT SELECT ON  %s TO PUBLIC;\n",
	       Map->fInfo.post.cat_table,
	       Map->fInfo.post.cat_id,
	       Map->fInfo.post.cat_field,
	       Map->fInfo.post.cat_cat,
	       Map->fInfo.post.cat_id, Map->fInfo.post.cat_field,
	       Map->fInfo.post.cat_id,
	       Map->fInfo.post.geom_table, Map->fInfo.post.cat_table);
      fprintf (stderr, "%s", query);
      Map->fInfo.post.catRes = PQexec (Map->fInfo.post.conn, query);
      if (!Map->fInfo.post.catRes
	  || PQresultStatus (Map->fInfo.post.catRes) != PGRES_COMMAND_OK)
	{
	  PQclear (Map->fInfo.post.catRes);
	  Map->fInfo.post.catRes = NULL;
	  PQfinish (Map->fInfo.post.conn);
	  free (query);
	  return (-1);
	}
    }
  PQclear (Map->fInfo.post.catRes);
  Map->fInfo.post.catRes = NULL;

/************************************************************************************/
  return (V1_open_old_post (Map, 1));
}

#endif
