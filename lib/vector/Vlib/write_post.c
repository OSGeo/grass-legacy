/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):   
*               Written by (in alphabetic order):   
*                     Di Simone Alessio                      a.disimone@inwind.it
*                     Di Sorbo  Alessandro                   a.disorbo@inwind.it
*                     Ragni Domenico                         domrag@inwind.it
*                     Romano Enrico                          enr.omano@genie.it
*                     Serino Antonio                         antoseri@libero.it
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
#include <stdlib.h>
#include "gis.h"
#include "Vect.h"

#ifdef HAVE_POSTGRES
/*Internal function*/
int insert_geom (struct Map_info *Map, int type, int id,
		 struct line_pnts *points, char *gquery);
int insert_cat (struct Map_info *Map, struct line_cats *cats, int id);
int update_db_view (struct Map_info *Map);
char *generate_geometry_string (int type, struct line_pnts *points,
				struct Map_info *Map);
int generate_id (char *query, struct Map_info *Map, int type, int tryid,
		 int *catId);
int existcats (int id, int field, struct Map_info *Map);
/***********************************************************************************************
* Function name: V1_write_line_post. 
* Arguments    : Map, type, points, cats
* Return       : Modify Map structures, and return -1:error, nextId: Id value for next record that will be written.
* Description  :
*             Write a vector to database. 
************************************************************************************************/
long
V1_write_line_post (struct Map_info *Map,
		    int type,
		    struct line_pnts *points, struct line_cats *cats)
{
  char *query = NULL;
  int gId;
  int catId = -1;
/***********************************************************************************************/
/* Write geometry */
  if ((query = generate_geometry_string (type, points, Map)) == NULL)
    return (-1);
  if ((gId =
       generate_id (query, Map, type, Map->fInfo.post.nextId, &catId)) >= 0)
    {
      if (insert_geom (Map, type, gId, points, query))
	{
	  free (query);
	  return (-1);
	}
      Map->fInfo.post.nextId++;
    }
  /* Write categories */
  fprintf (stderr, "Catid:%d GiD:%d\n", catId, gId);
  if (insert_cat (Map, cats, catId))
    {
      free (query);
      return (-1);
    }
/*Actualize cursor view on database*/
  if (update_db_view (Map))
    {
      free (query);
      return (-1);
    }


  free (query);
  return (Map->fInfo.post.nextId);
}

/*************************************************************************************************
* Function name: generate_id 
* Arguments    : query ,Map, type, tryId, CatId
* Return       : return -1:error, Id value for next geometry and category that will be written.
* Description  :
*               Generate next id for geometry table 
**************************************************************************************************/
int
generate_id (char *query, struct Map_info *Map, int type, int tryid,
	     int *catId)
{

  PGresult *res;
  char *sqlquery = (char *) calloc (65536, sizeof (char));
  int id;
  int resid = -1;

  *sqlquery = '\x0';
  type = Vect_type_to_store (type);
  if (type == 0)
    {
      free (sqlquery);
      return (-1);
    }

/**********************************************************************/
  sprintf (sqlquery, "select %s from %s where %s = %d  ;",
	   Map->fInfo.post.geom_id, Map->fInfo.post.geom_table,
	   Map->fInfo.post.geom_id, tryid);
  res = PQexec (Map->fInfo.post.conn, sqlquery);
  if (!res || PQresultStatus (res) != PGRES_TUPLES_OK)
    {
      PQclear (res);
      free (sqlquery);
      return (-1);
    }

  if (PQntuples (res) != 0)
    resid = atoi (PQgetvalue (res, 0, 0));
  PQclear (res);

  if (resid == -1)
    {
      id = tryid;
      *catId = id;
    }				/*Id doesn't exist */
  else
    {
      id = generate_id (query, Map, type, tryid + 1, catId);
    }
  free (sqlquery);
  return (id);
}

/*****************************************************************************************
* Function name: generate_geometry_string 
* Arguments    : type, points, Map
* Return       : return Null:error, GeometryAsText on success.
* Description  :
*               Build gepmetry field in text mode 
******************************************************************************************/
char *
generate_geometry_string (int type, struct line_pnts *points,
			  struct Map_info *Map)
{

  int i;
  char *query = (char *) calloc (65536, sizeof (char));

  sprintf (query, "GeometryFromText('");
  if (GV_POINTS & type)		/* Type is a Point */
    {
      sprintf (query, "%sPOINT (", query);
    }
  else if (GV_LINES & type)	/* Type is a Linestring */
    {
      sprintf (query, "%sLINESTRING (", query);
    }
  else
    {
      free (query);
      return (NULL);
    }
  for (i = 0; i < points->n_points; i++)	/*Insert points */
    {

      if (i > 0)
	sprintf (query, "%s, ", query);
      sprintf (query, "%s %f %f", query, points->x[i], points->y[i]);
      if (Map->head.with_z == WITH_Z)
	sprintf (query, "%s %f ", query, points->z[i]);
    }
  sprintf (query, "%s)', -1)", query);
  return (query);
}

/**********************************************************************************************
* Function name: V1_rewrite_line_post. 
* Arguments    : Map, type, id, points, cats
* Return       : Modify Map structures, and return -1:error, 
*                nextId: Id value for next record that will be written.
* Description  :
             Rewrite a vector with a given Id in database. 
***********************************************************************************************/
long
V1_rewrite_line_post (struct Map_info *Map,
		      long id,
		      int type,
		      struct line_pnts *points, struct line_cats *cats)
{
  char *query = (char *) calloc (65536, sizeof (char));
  char *gquery = NULL;
  PGresult *res;
/*******************************************************************************************/
  /* Update geometry */
  /*Build query */
  gquery = generate_geometry_string (type, points, Map);
  sprintf (query, "UPDATE %s SET %s = ",
	   Map->fInfo.post.geom_table, Map->fInfo.post.geom_type);
  if (GV_POINTS & type)		/* Type is a Point */
    {
      if (type == GV_POINT)
	sprintf (query, "%s1, %s = %s", query, Map->fInfo.post.geom_geom, gquery);	/*GV_POINT */
      else
	sprintf (query, "%s4, %s = %s", query, Map->fInfo.post.geom_geom, gquery);	/*GV_CENTROID */
    }
  else if (GV_LINES & type)	/* Type is a Linestring */
    {
      if (type == GV_LINE)
	sprintf (query, "%s2, %s = %s", query, Map->fInfo.post.geom_geom, gquery);	/*GV_LINE */
      else
	sprintf (query, "%s3, %s = %s", query, Map->fInfo.post.geom_geom, gquery);	/*GV_BOUNDARY */
    }
  else
    {
      if (gquery != NULL)
	free (gquery);
      update_db_view (Map);
      free (query);
      return (-1);
    }
  sprintf (query, "%s  WHERE %s = %ld;", query, Map->fInfo.post.geom_id, id);
  res = PQexec (Map->fInfo.post.conn, query);
  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK)
    {
      PQclear (res);
      if (gquery != NULL)
	free (gquery);
      free (query);
      return (-1);
    }

  PQclear (res);
  if (gquery != NULL)
    free (gquery);
  /* Categories */
  if (insert_cat (Map, cats, id))
    {
      update_db_view (Map);
      free (query);
      return (-1);
    }
  free (query);
  /*Actualize cursor view on database */
  if (update_db_view (Map))
    return (-1);

  return (Map->fInfo.post.nextId);
}

/*************************************************************************************************
*Function name: insert_geom.
*Arguments    : Map, type, id, points.
*Return       : Status (-1 error 0 all ok).
*
*Description  :
*             Insert a geometry in geometry table
**************************************************************************************************/
int
insert_geom (struct Map_info *Map, int type, int id, struct line_pnts *points,
	     char *gquery)
{
  char *query = (char *) calloc (65536, sizeof (char));
  PGresult *res = NULL;
/*Build query*/
  sprintf (query, "INSERT INTO %s VALUES (%d",
	   Map->fInfo.post.geom_table, id);

  if (GV_POINTS & type)		/* Type is a Point */
    {
      if (type == GV_POINT)
	sprintf (query, "%s,1,%s", query, gquery);	/*GV_POINT */
      else
	sprintf (query, "%s,4,%s", query, gquery);	/*GV_CENTROID */
    }
  else if (GV_LINES & type)	/* Type is a Linestring */
    {
      if (type == GV_LINE)
	sprintf (query, "%s,2,%s", query, gquery);	/*GV_LINESTRING */
      else
	sprintf (query, "%s,3,%s", query, gquery);	/*GV_BOUNDARY */
    }
  else
    {
      free (query);
      return (-1);
    }
  sprintf (query, "%s );", query);
  res = PQexec (Map->fInfo.post.conn, query);
  free (query);
  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK)
    {
      PQclear (res);
      return (-1);
    }
  PQclear (res);
  return (0);
}

/***************************************************************************************************
*Function name: existcats
*Arguments    : id, field, Map
*Return       : Status (-1 error 0,1 number of cats in cattable).
*
*Description  :
*             Count number of cat with the given (id,field)
****************************************************************************************************/
int
existcats (int id, int field, struct Map_info *Map)
{
  PGresult *res = NULL;
  char *query = (char *) calloc (65536, sizeof (char));
  int count = -1;

  sprintf (query, "SELECT COUNT(%s) FROM %s WHERE %s = %d and %s = %d;",
	   Map->fInfo.post.cat_id, Map->fInfo.post.cat_table,
	   Map->fInfo.post.cat_id, id, Map->fInfo.post.cat_field, field);
  res = PQexec (Map->fInfo.post.conn, query);
  if (!res || PQresultStatus (res) != PGRES_TUPLES_OK)
    {
    }
  else
    {
      count = atoi (PQgetvalue (res, 0, 0));
    }
  PQclear (res);
  free (query);
  return (count);
}

/***************************************************************************************************
* Function name: insert_cat
* Arguments    : Map , cats , id
* Return       : Status (-1 error 0 all ok)
* Description  :
*             Insert category rows in table. 
****************************************************************************************************/
int
insert_cat (struct Map_info *Map, struct line_cats *cats, int id)
{
  PGresult *res = NULL;
  int i;
  char *query = (char *) calloc (65536, sizeof (char));

  for (i = 0; i < cats->n_cats; i++)	/*Insert all categories */
    {
      sprintf (query, "INSERT INTO %s VALUES (%d,%d, %d)",
	       Map->fInfo.post.cat_table, id, cats->field[i], cats->cat[i]);
      if (!existcats (id, cats->field[i], Map))
	{
	  res = PQexec (Map->fInfo.post.conn, query);
	  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK)
	    {
	      PQclear (res);
	      free (query);
	      return (-1);
	    }
	  PQclear (res);
	}
    }
  free (query);
  return (0);
}

/****************************************************************************************************
* Function name: update_db_view
* Arguments    : Map 
* Return       : Status (-1 error 0 all ok)
* Description  :
*              Restart transaction, end current transaction, start another, upadate struct values
*****************************************************************************************************/
int
update_db_view (struct Map_info *Map)
{
  char *query = (char *) calloc (65536, sizeof (char));
/*Close cursor and commit*/
  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, "CLOSE g_cursor");
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
  PQexec (Map->fInfo.post.conn, "COMMIT");
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
  if (Map->fInfo.post.catRes != NULL)
    {
      PQclear (Map->fInfo.post.catRes);
      Map->fInfo.post.catRes = NULL;
    }
/*******************************************/
/*Begin a new transiction, redeclare cursor*/
  sprintf (query, "BEGIN");
  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
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
      free (query);
      return (-1);
    }
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
/************************************************************************/
  sprintf (query, "SELECT  count(%s),max(%s),min(%s) from %s",
	   Map->fInfo.post.geom_id, Map->fInfo.post.geom_id,
	   Map->fInfo.post.geom_id, Map->fInfo.post.geom_table);
  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
  if (!Map->fInfo.post.geomRes
      || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_TUPLES_OK)
    {
      PQclear (Map->fInfo.post.geomRes);
      Map->fInfo.post.geomRes = NULL;
      free (query);
      return (-1);
    }

  Map->fInfo.post.nGeom = atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 0));
  if (Map->fInfo.post.nextId >
      (atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 1)) + 1))
    Map->fInfo.post.nextId =
      atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 1)) + 1;
  if (Map->fInfo.post.nextRow <
      atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 2)))
    Map->fInfo.post.nextRow =
      atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 2));
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
/*********************************************************************/
  free (query);
  return (0);
}

/* Deletes line at the given offset.
*
*  Returns:  0 ok
*           -1 on error
*/
int
V1_delete_line_post (struct Map_info *Map, long id)
{
  char *query = (char *) calloc (65536, sizeof (char));
  PGresult *res;

  /* Delete row from geometry table and automatic cascade to cattble */

  sprintf (query, "DELETE FROM %s WHERE %s=%ld",
	   Map->fInfo.post.geom_table, Map->fInfo.post.geom_id, id);

  res = PQexec (Map->fInfo.post.conn, query);

  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK)
    {
      PQclear (res);
      free (query);
      return (-1);
    }

  PQclear (res);
  /* Map->fInfo.post.nextId = id; */ /*This is an unuse id please save it, and use for future write */
  /* No next id will be max id + 1, deleted id is not reused */

  /*Actualize cursor view on database */
  if (update_db_view (Map))
    return (-1);

  return 0;
}

#endif
