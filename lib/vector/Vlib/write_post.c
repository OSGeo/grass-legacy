/****************************************************************************
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
#include <string.h>
#include "gis.h"
#include "Vect.h"

#ifdef HAVE_POSTGRES
/*Internal function*/
int insert_geom (struct Map_info *Map, int type, int id,
		 struct line_pnts *points, char *gquery);
int insert_cat (struct Map_info *Map, struct line_cats *cats, int id);
char *generate_geometry_string (int type, struct line_pnts *points,
				struct Map_info *Map);
int generate_id (struct Map_info *Map);
int existcats (int id, int field, struct Map_info *Map);

/*****************************************************************************************
* Function name: V1_write_line_post. 
* Arguments    : Map, type, points, cats
* Return       : Modify Map structures, and return -1: error, 
*                Id value of written line
* Description  : Write a vector to database. 
******************************************************************************************/
long
V1_write_line_post (struct Map_info *Map,
		    int type,
		    struct line_pnts *points, struct line_cats *cats)
{

  char *query = NULL;
  int gId;
  PGresult *res;

  G_debug (3, "V1_write_line_post()" );

  /* Start transaction for one line */
  res = PQexec (Map->fInfo.post.conn, "BEGIN");
  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK) {
     G_warning ("Cannot begin transaction\n%s", PQresultErrorMessage(res) );
     PQclear (res);
     return (-1);
  }
  PQclear (res);
  
  /* Write geometry */
  if ((query = generate_geometry_string (type, points, Map)) == NULL)
    return (-1);

  gId = generate_id ( Map );
  if ( gId == 0 ) return (-1);

  G_debug (3, "gId = %d", gId );
  if (insert_geom (Map, type, gId, points, query)) {
      G_warning ("cannot inser geometry");
      free (query);
      return (-1);
  }

  /* Write categories */
  G_debug ( 3, "Feature id = %d\n", gId);
  if (insert_cat (Map, cats, gId)) {
      free (query);
      return (-1);
  }
  /* Close transaction */
  res = PQexec (Map->fInfo.post.conn, "COMMIT");
  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK) {
     G_warning ("Cannot close transaction\n%s", PQresultErrorMessage(res) );
     PQclear (res);
     return (-1);
  }
  PQclear (res);

  free (query);
  return (gId);
}

/******************************************************************************************
* Function name: generate_id 
* Arguments    : Map
* Return       : 0 on error
*                id value ( > 0) for next geometry and category that will be written.
* Description  : Generate next id for geometry table 
********************************************************************************************/
int
generate_id (struct Map_info *Map)
{
  PGresult *res;
  char query[1000];
  int id;

  G_debug (3, "generate_id()" );

  sprintf (query, "select max(%s) from %s", 
	           Map->fInfo.post.geom_id, Map->fInfo.post.geom_table);

  G_debug (3, "%s", query );

  res = PQexec (Map->fInfo.post.conn, query);
  if (!res || PQresultStatus (res) != PGRES_TUPLES_OK) {
      G_warning ( "Cannot generate feature id\n%s",  PQresultErrorMessage( res ) );
      PQclear (res);
      return 0;
  }

  if (PQntuples (res) != 0)
      id = atoi (PQgetvalue (res, 0, 0)) + 1;
  else 
      id = 1;
  
  PQclear (res);

  G_debug (3, "  new id = %d", id );
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
  int i, a_query;
  char buf1[1000]; 
  char *query;

  G_debug (3, "generate_geometry_string()" );

  a_query = 1000;
  query = (char *) G_malloc ( a_query * sizeof (char) );
  
  sprintf (query, "GeometryFromText('");
  if (GV_POINTS & type)		/* Type is a Point */
      sprintf (query, "%sPOINT (", query);
  else if (GV_LINES & type)	/* Type is a Linestring */
      sprintf (query, "%sLINESTRING (", query);

  for (i = 0; i < points->n_points; i++)	/*Insert points */
    {
      buf1[0] = 0;
      if (i > 0) sprintf (buf1, ", ");
      sprintf (buf1, "%s%f %f", buf1, points->x[i], points->y[i]);
      if (Map->head.with_z == WITH_Z) {
	sprintf (buf1, "%s %f", buf1, points->z[i]);
      }

      /* Check space */
      if ( (strlen(query) + strlen(buf1) + 100) > a_query ) { /* space to close the string */
	  a_query += 1000; 
	  query = (char *) G_realloc ( query, a_query * sizeof (char) ); 
      }
      sprintf (query, "%s%s", query, buf1);
    }
  sprintf (query, "%s)', -1)", query); /* 9 chars */
  G_debug (5, "geometry_string: '%s'", query );

  return (query);
}

/**********************************************************************************************
* Function name: V1_rewrite_line_post. 
* Arguments    : Map, type, id, points, cats
* Return       : Modify Map structures, and return -1:error, 
*                Id value of written line.
* Description  : Rewrite a vector with a given Id in database. 
***********************************************************************************************/
long
V1_rewrite_line_post (struct Map_info *Map, long gId, int type,
		      struct line_pnts *points, struct line_cats *cats)
{
  char *query;
  PGresult *res;

  /* TODO: should be one transaction */
  /* We could do some tricks with 'UPDATE', but we are lazy today */

  /* Start transaction for one line */
  res = PQexec (Map->fInfo.post.conn, "BEGIN");
  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK) {
     G_warning ("Cannot begin transaction\n%s", PQresultErrorMessage(res) );
     PQclear (res);
     return (-1);
  }
  PQclear (res);
  
  /* Delete old */
  V1_delete_line_post ( Map, gId );

  /* Write geometry */
  if ((query = generate_geometry_string (type, points, Map)) == NULL)
    return (-1);

  G_debug (3, "gId = %d", gId );
  if (insert_geom (Map, type, gId, points, query)) {
      G_warning ("cannot inser geometry");
      free (query);
      return (-1);
  }

  /* Write categories */
  G_debug ( 3, "Feature id = %d\n", gId);
  if (insert_cat (Map, cats, gId)) {
      free (query);
      return (-1);
  }
  /* Close transaction */
  res = PQexec (Map->fInfo.post.conn, "COMMIT");
  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK) {
     G_warning ("Cannot close transaction\n%s", PQresultErrorMessage(res) );
     PQclear (res);
     return (-1);
  }
  PQclear (res);

  free (query);

  return (gId);
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
  int size;
  char *query;
  PGresult *res = NULL;
  
  G_debug (3, "insert_geom(): type = %d, id = %d", type, id );
  
  /*Build query*/
  size = 1000 + strlen ( gquery );
  query = (char *) G_malloc ( size );
  sprintf (query, "INSERT INTO %s VALUES (%d", Map->fInfo.post.geom_table, id);

  sprintf (query, "%s,%d,%s", query, Vect_type_to_store (type), gquery );
  sprintf (query, "%s );", query);

  G_debug (5, "%s", query );

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

/*****************************************************************************************
*Function name: existcats
*Arguments    : id, field, Map
*Return       : -1 error; number of cats in cattable
*Description  : Count number of cat with the given (id,field)
*******************************************************************************************/
int
existcats (int id, int field, struct Map_info *Map)
{
  PGresult *res = NULL;
  char query[2000];
  int count;

  sprintf (query, "SELECT COUNT(%s) FROM %s WHERE %s = %d and %s = %d;",
	   Map->fInfo.post.cat_id, Map->fInfo.post.cat_table,
	   Map->fInfo.post.cat_id, id, Map->fInfo.post.cat_field, field);
  G_debug ( 3, query );
  res = PQexec (Map->fInfo.post.conn, query);
  if (!res || PQresultStatus (res) != PGRES_TUPLES_OK) {
	G_warning ("Cannot select number of categories" );
	return (-1);
  } else {
      count = atoi (PQgetvalue (res, 0, 0));
  }
  PQclear (res);
  return (count);
}

/*****************************************************************************************
* Function name: insert_cat
* Arguments    : Map , cats , id
* Return       : Status (-1 error; 0 all ok)
* Description  : Insert category rows in table. 
******************************************************************************************/
int
insert_cat (struct Map_info *Map, struct line_cats *cats, int id)
{
  PGresult *res;
  int i, ret;
  char query[2000];

  for (i = 0; i < cats->n_cats; i++) {	/*Insert all categories */
      ret = existcats (id, cats->field[i], Map);
      if ( ret == -1 ) return -1;
      else if ( ret == 1 ) { /* should not happen */
	  G_warning ( "Category already exists");
	  return -1;
      } else { /* no cat */
	  sprintf (query, "INSERT INTO %s VALUES (%d,%d, %d)",
		   Map->fInfo.post.cat_table, id, cats->field[i], cats->cat[i]);
	  G_debug ( 3, query );
	  res = PQexec (Map->fInfo.post.conn, query);
	  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK) {
	      PQclear (res);
	      G_warning ( "Cannot write category");
	      return (-1);
	    }
	  PQclear (res);
      }
  }
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
  char query[2000];
  PGresult *res;

  /* Delete row from geometry table and automatic cascade to cattble */
  sprintf (query, "DELETE FROM %s WHERE %s=%ld",
	   Map->fInfo.post.geom_table, Map->fInfo.post.geom_id, id);

  res = PQexec (Map->fInfo.post.conn, query);

  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK) {
      PQclear (res);
      return (-1);
  }

  PQclear (res);

  return 0;
}

/*!
*  \fn int Vect_delete_post_tables ( struct Map_info *Map )
*  \brief delete PostGis tables
*  \return -1 error, 0 success
*  \param  struct Map_info*
*/
int
Vect_delete_post_tables (  struct Map_info *Map )
{
    char buf[1000];
    PGresult *res = NULL;

    G_debug (3, "Delete PostGis tables" );
    
    sprintf ( buf, "DROP TABLE %s", Map->fInfo.post.geom_table );
    res = PQexec (Map->fInfo.post.conn, buf);
    if ( !res || PQresultStatus (res) != PGRES_COMMAND_OK ) {
	G_warning ("Cannot delete PostGIS table '%s'", Map->fInfo.post.geom_table );
        PQclear (res);
	return (-1);
    }
    PQclear (res);
	
    sprintf ( buf, "DROP TABLE %s", Map->fInfo.post.cat_table );
    res = PQexec (Map->fInfo.post.conn, buf);
    if ( !res || PQresultStatus (res) != PGRES_COMMAND_OK ) {
	G_warning ("Cannot delete PostGIS table '%s'", Map->fInfo.post.cat_table );
        PQclear (res);
	return (-1);
    }
    PQclear (res);
	
    return 0;
}

#endif
