/*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S): 
*               Di Simone Alessio                    a.disimone@inwind.it
*               Di Sorbo  Alessandro                 a.disorbo@inwind.it
*               Ragni Domenico                       domrag@inwind.it
*               Romano Enrico                        enr.omano@genie.it
*               Serino Antonio                       antoseri@libero.it
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

/*
 *V1_read_next_line (Map, line_p)
 *              reads thru digit file looking for next line within window,
 *              stores info in struct instead of args.
 *      NOTE:  line_p->alloc_points better be set to 0 for the first call.
 * 
 * returns:  -1 on error
 *           -2 EOF
 *           line type (positive value) if it found a line

 **
 **
 **  The action of this routine can be modified by:
 **    Vect_read_constraint_region ()
 **    Vect_read_constraint_type   ()
 **    Vect_remove_constraints     ()
 **
 */

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include "gis.h"
#include "Vect.h"
#include "portable.h"

#ifdef HAVE_POSTGRES

int
Vect__Read_line_post (struct Map_info *, struct line_pnts *,
		      struct line_cats *, long);

/*
*  read line from database
*
*  Returns  line type
*           -2 End of table (last row)
*           -1 Out of memory
*
*  This is NOT affected by constraints
*/
int
V1_read_line_post (struct Map_info *Map,
		   struct line_pnts *Points, struct line_cats *Cats, long id)
{
  return Vect__Read_line_post (Map, Points, Cats, id);
}

/*
*  returns -2 on EOF   -1 on Out of memory  or  line type 
*/
int
V1_read_next_line_post (struct Map_info *Map,
			struct line_pnts *line_p, struct line_cats *line_c)
{
  PGresult *res = NULL;
  char query[2000];  
  int type;
  long id;
  BOUND_BOX lbox, mbox;

  G_debug (3, "V1_read_next_line_post()");

  if (Map->Constraint_region_flag)
    Vect_get_constraint_box (Map, &mbox);

  while (1) {
      /* Find the nex id */
      sprintf (query, "select min(%s) from (select %s from %s "
	              "where %s > %d ) as tmp",
		      Map->fInfo.post.geom_id, Map->fInfo.post.geom_id,
		      Map->fInfo.post.geom_table,
		      Map->fInfo.post.geom_id, Map->fInfo.post.lastRead);
      G_debug (3, "%s", query );
	 
      res = PQexec (Map->fInfo.post.conn, query);
      if (!res || PQresultStatus (res) != PGRES_TUPLES_OK) {
           G_warning ( "Cannot get next line id\n%s",  PQresultErrorMessage( res ) );
           PQclear (res);
	   return -1;
      }

      if ( strlen (  PQgetvalue (res, 0, 0) ) == 0 ) return -2; /* no more records */
      
      id = atoi (PQgetvalue (res, 0, 0));
      PQclear (res);
      
      G_debug (3, "  id = %d", id);
      type = Vect__Read_line_post (Map, line_p, line_c, id);
      G_debug (3, "  type  = %d", type);
      if (type < 0) return (type);

      /* Constraint on Type of line */
      if (Map->Constraint_type_flag) {
	  if (!(type & Map->Constraint_type)) continue;
      }

      /* Constraint on specified region */
      if (Map->Constraint_region_flag) {
	  Vect_line_box (line_p, &lbox);
	  if (!Vect_box_overlap (&lbox, &mbox)) continue;
      }

      return (type);
  }
  /* NOTREACHED */
}	

/*
*  reads any specified line   This is NOT affected by constraints
*/
int
V2_read_line_post (struct Map_info *Map,
		   struct line_pnts *line_p,
		   struct line_cats *line_c, int line)
{
  P_LINE *Line;

  G_debug (3, "V2_read_line_post(): line = %d", line);

  Line = Map->plus.Line[line];
  return Vect__Read_line_post (Map, line_p, line_c, Line->offset);
}

/* reads next unread line each time called.  use Vect_rewind to reset */
/* returns -2 on end of lines */
int
V2_read_next_line_post (struct Map_info *Map,
			struct line_pnts *line_p, struct line_cats *line_c)
{
  register int line;
  register P_LINE *Line;
  BOUND_BOX lbox, mbox;

  if (Map->Constraint_region_flag)
    Vect_get_constraint_box (Map, &mbox);

  while (1) {
      line = Map->next_line;

      if (line > Map->plus.n_lines) return (-2);

      Line = Map->plus.Line[line];
      if (Line == NULL) continue; /* Dead line */

      if ((Map->Constraint_type_flag && !(Line->type & Map->Constraint_type))) {
	  Map->next_line++;
	  continue;
      }

      if (Map->Constraint_region_flag) {
	  Vect_get_line_box (Map, line, &lbox);
	  if (!Vect_box_overlap (&lbox, &mbox)) {
	      Map->next_line++;
	      continue;
	  }
      }

      return V2_read_line_post (Map, line_p, line_c, Map->next_line++);
  }
  /* NOTREACHED */
}

/********************************************************************************
* Function name: Vect__read_line_post.
* Arguments    : Map , points, cats, id
* Return       : Status (-1 error, -2 end of table, else line type), and modify Map structures.
*    
* Description  : Seek the cursor to specified id, fetch the  geometry row, declare  a cursor 
*                for category table row with a given id and read all categories.
*
*********************************************************************************/
int
Vect__Read_line_post (struct Map_info *Map,
		      struct line_pnts *p, struct line_cats *c, long id)
{
  char query[2000];
  PGresult *res;
  int gType, type;
  int i;
  int dimension;
  int npoints;
  int cField;
  int cCat;
  int nCat;
  char  *cval;
  double x, y, z;
  
  G_debug (3, "Vect__Read_line_post(): id = %d", id);

  Map->head.last_offset = id;

  sprintf (query, "select %s, NumPoints(%s) , Dimension(%s)  FROM  %s where %s  = %ld;",
             Map->fInfo.post.geom_type,
             Map->fInfo.post.geom_geom, Map->fInfo.post.geom_geom,
             Map->fInfo.post.geom_table,
	     Map->fInfo.post.geom_id, id );
  
  G_debug (3, "%s", query );
       
  res = PQexec (Map->fInfo.post.conn, query);
  if (!res || PQresultStatus (res) != PGRES_TUPLES_OK) {
      G_warning("Cannot select line %d from database\n%s", id, PQresultErrorMessage(res));
      PQclear (res);
      return -1;
  }
  
  if ( PQntuples (res) == 0 ) { /* nothing selected */
      G_warning("Cannot select line %d from database", id);
      PQclear (res);
      return -1;
  }

  /***********Geometry Parser ***********************/
  gType = atoi (PQgetvalue (res, 0, 0)); /* int4 Geom type */
  type = dig_type_from_store (gType);
  npoints = atoi (PQgetvalue (res, 0, 1)); /* int4 Number of line points */
  dimension = atoi (PQgetvalue (res, 0, 2)); /* int4 Dimension 2/3 */
  PQclear (res);
  G_debug(3,"  store type = %d n_points = %d", gType, npoints );

    if (p != NULL) {
        Vect_reset_line ( p );

        /* start a transaction block */
	res = PQexec(Map->fInfo.post.conn, "BEGIN");
	if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	    G_warning("BEGIN command failed\n%s", PQresultErrorMessage(res) );
	    PQclear(res);
	    return -1;
	}
	PQclear(res);

	sprintf(query, "DECLARE gcursor BINARY CURSOR FOR SELECT ASBINARY( "); 
	
	if ( Vect_is_3d(Map) )
 	    sprintf(query, "%s force_3d(%s), ", query, Map->fInfo.post.geom_geom);
	else
 	    sprintf(query, "%s force_2d(%s), ", query, Map->fInfo.post.geom_geom);

        if ( DOUBLE_ORDER == ENDIAN_LITTLE ) sprintf(query, "%s 'ndr') ", query);
	else if ( DOUBLE_ORDER == ENDIAN_BIG ) sprintf(query, "%s 'xdr') ", query);
	else G_fatal_error ("PostGIS not supported for this platform (byte order)");
	
	sprintf(query, "%s FROM %s WHERE %s = %ld", query, Map->fInfo.post.geom_table,
		            Map->fInfo.post.geom_id, id );
	G_debug (3, "%s", query );

	res = PQexec(Map->fInfo.post.conn, query);
	if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	    G_warning("Cannot declare cursor\n%s", PQresultErrorMessage(res) );
	    PQclear(res);
	    return -1;
	}
	
	sprintf(query, "FETCH ALL in gcursor" );
	G_debug (3, "%s", query );

	res = PQexec(Map->fInfo.post.conn, query);
	if (!res || PQresultStatus(res) != PGRES_TUPLES_OK) {
	    G_warning("Cannot FETCH ALL\n%s", PQresultErrorMessage(res) );
	    PQclear(res);
	    return -1;
	}
	
	if ( PQntuples(res) != 1 ) {
	    G_warning("Incorrect number of records (%d) for geometry", PQntuples(res) );
	    PQclear(res);
	    return -1;
	}
       
	G_debug (3, "wkb length = %d", PQgetlength(res, 0, 0));

	cval = (char *) PQgetvalue(res, 0, 0);

	if ( type & GV_POINTS ) {
	    if ( Vect_is_3d ( Map ) ) {
	        memcpy(&x, &cval[5], 8);
	        memcpy(&y, &cval[5+8], 8);
	        memcpy(&z, &cval[5+16], 8);
	    } else { 
	        memcpy(&x, &cval[5], 8);
	        memcpy(&y, &cval[5+8], 8);
	        z = 0;
	    }
	    G_debug (5, "x,y,z = %f,%f,%f", x, y, z );
            Vect_append_point ( p, x, y, z); 
	} else {  /* GV_LINES */
	    memcpy(&i, &cval[1], 4);
            G_debug (3, "np = %d", i );
	    memcpy(&i, cval+1, 4);
            G_debug (3, "np = %d", i );
	    for ( i = 0; i < npoints; i++ ) {
		 
		if ( Vect_is_3d ( Map ) ) {
		    memcpy(&x, &cval[9+i*24], 8);
		    memcpy(&y, &cval[9+i*24+8], 8);
		    memcpy(&z, &cval[9+i*24+16], 8);
		} else { 
		    memcpy(&x, &cval[9+i*16], 8);
		    memcpy(&y, &cval[9+i*16+8], 8);
		    z = 0;
		}
	        G_debug (5, "%d x,y,z = %f,%f,%f", i, x, y, z );
		Vect_append_point ( p, x, y, z); 
	    }

	}
	PQclear(res);

	res = PQexec(Map->fInfo.post.conn, "CLOSE gcursor");
	if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	    G_warning("Cannot close cursor\n%s", PQresultErrorMessage(res) );
	    PQclear(res);
	    return -1;
	}
	PQclear(res);

	res = PQexec(Map->fInfo.post.conn, "COMMIT");
	if (!res || PQresultStatus(res) != PGRES_COMMAND_OK) {
	    G_warning("Cannot COMMIT\n%s", PQresultErrorMessage(res) );
	    PQclear(res);
	    return -1;
	}
	PQclear(res);
    }
    Map->fInfo.post.lastRead = id; /* Best place? To move forward if cats fail */
  
    /*********Category***************************/
    /* Count category with a gived id */
    if (c != NULL) {
      sprintf (query,
	       "SELECT %s, %s FROM %s WHERE %s = %ld;",
	       Map->fInfo.post.cat_field, Map->fInfo.post.cat_cat, 
	       Map->fInfo.post.cat_table,
	       Map->fInfo.post.cat_id, id);
      G_debug (3, "%s", query );
      res = PQexec (Map->fInfo.post.conn, query);
      if (!res || PQresultStatus (res) != PGRES_TUPLES_OK) {
	  G_warning ( "Cannot select categories from database\n%s",
		  PQresultErrorMessage( res ));
	  PQclear (res);
	  return (-1);
      }
      nCat = PQntuples (res);
      Vect_reset_cats (c);	/*Read all categories and insert into category array */
      for (i = 0; i < nCat; i++) {
	  cField = atoi (PQgetvalue (res, i, 0));
	  cCat = atoi (PQgetvalue (res, i, 1));
	  if (!Vect_cat_set (c, cField, cCat)) return (-1);
      }
      PQclear (res);
    }

    /*************************************/
    G_debug (3, "  gType = %d -> type = %d", gType, type);

    if (type == 0) return (-1);

    return type;
}

/*!
 \fn long Vect_next_line_offset_post (struct Map_info *Map)
 \brief get next line offset for PostGRASS format
 \return next line offset
 \param Map_info structure
*/
long
Vect_next_line_offset_post (struct Map_info *Map)
{
    G_fatal_error ("Vect_next_line_offset_post() not implemented");
    return -1;
}

/*!
*  \fn long Vect_last_line_offset_post ( struct Map_info *Map )
*  \brief get last line offset for postgis format
*  \return last line offset
*  \param Map_info structure
*/
long
Vect_last_line_offset_post ( struct Map_info *Map )
{
      return ( Map->fInfo.post.lastRead );
}

#endif
