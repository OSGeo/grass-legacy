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

/* TODO: 3D */

/* Parse postgis connection string (clone from pg driver) */
int 
parse_conn ( struct Format_info *finfo )
{
    int  i;
    char **tokens, delm[2];
    
    G_debug ( 3, "parse_conn()");

    if ( finfo->post.db == NULL )
	G_fatal_error ( "Cannot parse PostGIS database (db == NULL)");

    /* reset */
    finfo->post.host = NULL;
    finfo->post.port = NULL;
    finfo->post.options = NULL;
    finfo->post.tty = NULL;
    finfo->post.database = NULL;
    finfo->post.user = NULL;
    finfo->post.password = NULL;
 
    G_debug (3, "parse_conn : %s", finfo->post.db ); 
    
    if ( strchr(finfo->post.db, '=') == NULL ) { /*db name only */
	finfo->post.database = G_store ( finfo->post.db );
    } else {
	delm[0] = ','; delm[1] = '\0';
        tokens = G_tokenize ( finfo->post.db, delm );
	i = 0;
	while ( tokens[i] ) {
	   G_debug (3, "token %d : %s", i, tokens[i] ); 
	   if ( strncmp(tokens[i], "host", 4 ) == 0 )
	       finfo->post.host = G_store ( tokens[i] + 5 );
	   else if ( strncmp(tokens[i], "port", 4 ) == 0 )
	       finfo->post.port = G_store ( tokens[i] + 5 );
	   else if ( strncmp(tokens[i], "options", 7 ) == 0 )
	       finfo->post.options = G_store ( tokens[i] + 8 );
	   else if ( strncmp(tokens[i], "tty", 3 ) == 0 )
	       finfo->post.tty = G_store ( tokens[i] + 4 );
	   else if ( strncmp(tokens[i], "dbname", 6 ) == 0 )
	       finfo->post.database = G_store ( tokens[i] + 7 );
	   else if ( strncmp(tokens[i], "user", 4 ) == 0 )
	       finfo->post.user = G_store ( tokens[i] + 5 );
	   else if ( strncmp(tokens[i], "password", 8 ) == 0 )
	       finfo->post.password = G_store ( tokens[i] + 9 );
	   else 
               G_warning ( "Unknown option in database definition for PostGIS: '%s'", tokens[i] );
	   
	   i++;
	}
	G_free_tokens ( tokens );	
    }

    G_debug ( 2, "  db = %s", finfo->post.db);
    G_debug ( 2, "  host = %s port = %s options = %s tty = %s", 
	      finfo->post.host, finfo->post.port, finfo->post.options, finfo->post.tty);
    G_debug ( 2, "  database = %s user = %s password = %s", 
	      finfo->post.database, finfo->post.user, finfo->post.password);
    return 0;
}

/* Set format for postgis */
int
set_frmt ( struct Format_info *finfo, char *name )
{
    char buf[1000];
    
    G_debug ( 3, "set_frmt_post(): name = %s", name);

    finfo->post.db         = G__getenv2 ( "GV_PGIS_DATABASE", G_VAR_MAPSET );
    if ( finfo->post.db == NULL ) G_fatal_error ( "PostGIS database was not set");

    sprintf (buf, "%s_geom", name );
    finfo->post.geom_table = G_store (buf);
    sprintf (buf, "%s_cat", name );
    finfo->post.cat_table = G_store (buf);
    finfo->post.geom_id    = G_store ("id");
    finfo->post.geom_type  = G_store ("type");
    finfo->post.geom_geom  = G_store ("geom");
    finfo->post.cat_id     = G_store ("id");
    finfo->post.cat_field  = G_store ("field");
    finfo->post.cat_cat    = G_store ("cat");

    return 1;
}
	

/************************************************************************************ 
* Function name: setup.
* Arguments    : Map.
* Return       : Status (-1 error 0 all ok), and modify Map structures.
*
**************************************************************************************/
int
setup (struct Map_info *Map)
{
  G_debug (3, "setup()\n");

  if ( Map->fInfo.post.database == NULL || strlen (Map->fInfo.post.database) == 0 ) {
      G_warning ("PostGIS connection: database table not defined");
      return -1;
  }
  
  /* Check if table names are set */
  if ( Map->fInfo.post.geom_table == NULL || strlen (Map->fInfo.post.geom_table) == 0 ) {
      G_warning ("PostGIS connection: geometry table not defined");
      return -1;
  }
  if ( Map->fInfo.post.cat_table == NULL || strlen (Map->fInfo.post.cat_table) == 0 ) {
      G_warning ("PostGIS connection: category table not defined");
      return -1;
  }
  /* Check if parameters are defined */
  if ((Map->fInfo.post.geom_id == NULL) || (strlen (Map->fInfo.post.geom_id) == 0)) {
      G_warning ("PostGIS connection: geom_id not defined");
      return -1;
  }

  if ((Map->fInfo.post.geom_type == NULL) || (strlen (Map->fInfo.post.geom_type) == 0)) {
      G_warning ("PostGIS connection: geom_type  not defined");
      return -1;
  }
  if ((Map->fInfo.post.geom_geom == NULL) || (strlen (Map->fInfo.post.geom_geom) == 0)) {
      G_warning ("PostGIS connection: geom_geom not defined");
      return -1;
  }

  if ((Map->fInfo.post.cat_id == NULL) || (strlen (Map->fInfo.post.cat_id) == 0)) {
      G_warning ("PostGIS connection: cat_id not defined");
      return -1;
  }
  if ((Map->fInfo.post.cat_field == NULL) || (strlen (Map->fInfo.post.cat_field) == 0)) {
      G_warning ("PostGIS connection: cat_field not defined");
      return -1;
  }
  if ((Map->fInfo.post.cat_cat == NULL) || (strlen (Map->fInfo.post.cat_cat) == 0)) {
      G_warning ("PostGIS connection: cat_cat not defined");
      return -1;
  }
  
  /* Try to make a connection to the specified database */
  Map->fInfo.post.conn =
       PQsetdbLogin (Map->fInfo.post.host, Map->fInfo.post.port, 
	             Map->fInfo.post.options, Map->fInfo.post.tty,
  		     Map->fInfo.post.database, Map->fInfo.post.user, Map->fInfo.post.password);

  if (PQstatus (Map->fInfo.post.conn) == CONNECTION_BAD) {
      G_warning ("Cannot make connection to PostGIS:\nhost = %s\nport = %s\n"
	          "database = %s\nuser = %s\n", 
		  Map->fInfo.post.host, Map->fInfo.post.port, 
		  Map->fInfo.post.database, Map->fInfo.post.user);
      return (-1);
  }
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
  G_debug (1, "V1_open_old_post()");
  
  parse_conn ( &(Map->fInfo) );
  if (setup (Map)) return (-1);

  Map->head.with_z = WITHOUT_Z;
  Map->fInfo.post.lastRead = 0;

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

  if (ret == -1) {/* topo file is not available */
      G_debug( 1, "Cannot open topo file for vector '%s'.", Vect_get_full_name (Map));
      return -1;
  }

  /* open spatial index */
  ret = Vect_open_spatial_index ( Map );
      
  if ( ret == -1 ) { /* spatial index is not available */
      G_debug( 1, "Cannot open spatial index file for vector '%s'.", Vect_get_full_name (Map) );
      /* free topology */
      dig_free_plus ( &(Map->plus) );
      return -1;
  }
  
  ret = V1_open_old_post (Map, update);
  if (ret != 0) {
      dig_free_plus (&(Map->plus));
      /* TODO: free spatial index */
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
  char query[2000];
  char buf[1000];
  FILE *fp;
  PGresult *res;

  G_debug (1, "V1_open_new_post()");

  set_frmt ( &(Map->fInfo), Map->name );
  parse_conn ( &(Map->fInfo) );

  if (setup (Map)) return (-1);
  
  /*Check if geom table doesn't exist */
  sprintf (query, "SELECT COUNT (tablename) FROM pg_tables WHERE tablename = '%s'", 
	                  Map->fInfo.post.geom_table);
  G_debug (1, "%s", query);
  res = PQexec (Map->fInfo.post.conn, query);

  if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
      G_warning ("%s", PQresultErrorMessage(res) );
      PQclear (res);
      PQfinish (Map->fInfo.post.conn);
      return (-1);
  }
  
  if ( atoi( PQgetvalue(res,0,0) ) ) { /* table already exist */
      G_warning ( "Table '%s' already exist", Map->fInfo.post.geom_table );
      return -1;
  }
  PQclear (res);

  /*Check if category table doesn't exist */
  sprintf (query, "SELECT COUNT (tablename) FROM pg_tables WHERE tablename = '%s'", 
	                  Map->fInfo.post.cat_table);
  G_debug (1, "%s", query);
  res = PQexec (Map->fInfo.post.conn, query);
  if (!res || PQresultStatus (res) != PGRES_TUPLES_OK) {
      G_warning ("%s", PQresultErrorMessage(res) );
      PQclear (res);
      PQfinish (Map->fInfo.post.conn);
      return (-1);
  }
  if ( atoi( PQgetvalue(res,0,0) ) ) { /* table already exist */
      G_warning ( "Table '%s' already exist", Map->fInfo.post.cat_table );
      return -1;
  }
  PQclear (res);

  /* TODO: both tables should be in transaction */
  /* Create geometry table */
  sprintf (query, "CREATE TABLE %s  ( %s int4 PRIMARY KEY, %s int4 CHECK ((%s > 0) "
	   "AND (%s < 5)), %s  geometry);\n",
	   Map->fInfo.post.geom_table, Map->fInfo.post.geom_id,
	   Map->fInfo.post.geom_type, Map->fInfo.post.geom_type,
	   Map->fInfo.post.geom_type, Map->fInfo.post.geom_geom );
  /* Originaly was as below, but "WITH ( ISLOSSY )" was problem on Postgres 7.2.1
  *  (not sure why) and "GRANT TO PUBLIC" probably is not best */
  /*
  sprintf (query, "CREATE TABLE %s  ( %s int4 PRIMARY KEY, %s int4 CHECK ((%s > 0) "
	   "AND (%s < 5)), %s  geometry);\n"
	   "GRANT SELECT ON  %s TO PUBLIC;\n"
	   "CREATE INDEX %s_sidx ON %s USING GIST (%s GIST_GEOMETRY_OPS ) WITH ( ISLOSSY );\n",
	   Map->fInfo.post.geom_table, Map->fInfo.post.geom_id,
	   Map->fInfo.post.geom_type, Map->fInfo.post.geom_type,
	   Map->fInfo.post.geom_type, Map->fInfo.post.geom_geom,
	   Map->fInfo.post.geom_table, Map->fInfo.post.geom_table,
	   Map->fInfo.post.geom_table, Map->fInfo.post.geom_geom);
  */

  G_debug(1,"%s", query);
  res = PQexec (Map->fInfo.post.conn, query);
  if (!res || PQresultStatus (res) != PGRES_COMMAND_OK) {
      G_warning ("Cannot create geometry table\n%s", PQresultErrorMessage(res) );
      PQclear (res);
      PQfinish (Map->fInfo.post.conn);
      return (-1);
  }
  PQclear (res);

  /* Create category table */
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
  G_debug (1, query);
  res = PQexec (Map->fInfo.post.conn, query);
  if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
      G_warning ("Cannot create category table\n%s", PQresultErrorMessage(res) );
      PQclear (res);
      PQfinish (Map->fInfo.post.conn);
      return (-1);
  }
  PQclear (res);

  /* Save format, it is more probable that create table fail than write_frmt */
  sprintf (buf, "%s/%s", GRASS_VECT_DIRECTORY, name);
  fp = G_fopen_new (buf, GRASS_VECT_FRMT_ELEMENT);
  dig_write_frmt_ascii ( fp, &(Map->fInfo), GV_FORMAT_POSTGIS );
  fclose (fp);

  Map->head.with_z = WITHOUT_Z;
  Map->fInfo.post.lastRead = 0;

  return (0);
}

#endif
