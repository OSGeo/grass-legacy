/*
* $Id$
*
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
   **  returns -2 on EOF   -1 on Out of memory  or  line type 
 */
int
V1_read_next_line_post (struct Map_info *Map,
			struct line_pnts *line_p, struct line_cats *line_c)
{
  int itype;
  long id;
  BOUND_BOX lbox, mbox;

  G_debug (3, "V1_read_line_post()");

  if (Map->Constraint_region_flag)
    Vect_get_constraint_box (Map, &mbox);

  while (1)
    {
      id = (Map->fInfo.post.nextRow);
      G_debug (3, "V1_read_line_post(): id = %d", id);
      itype = Vect__Read_line_post (Map, line_p, line_c, id);
      G_debug (3, "V1_read_line_post(): itype  = %d", itype);
      if (itype < 0)
	return (itype);

      /* Constraint on Type of line */
      if (Map->Constraint_type_flag)
	{
	  if (!(itype & Map->Constraint_type))
	    continue;
	}

      /* Constraint on specified region */
      if (Map->Constraint_region_flag)
	{
	  Vect_line_box (line_p, &lbox);

	  if (!Vect_box_overlap (&lbox, &mbox))
	    continue;
	}

      return (itype);
    }
  /* NOTREACHED */

}				/*  dig_read_line_struct_in_box()  */

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

  while (1)
    {
      line = Map->next_line;

      if (line > Map->plus.n_lines)
	return (-2);

      Line = Map->plus.Line[line];
      if (Line == NULL)		/* Dead line */
	continue;

      if ((Map->Constraint_type_flag && !(Line->type & Map->Constraint_type)))
	{
	  Map->next_line++;
	  continue;
	}

      if (Map->Constraint_region_flag)
	{
	  Vect_get_line_box (Map, line, &lbox);
	  if (!Vect_box_overlap (&lbox, &mbox))
	    {
	      Map->next_line++;
	      continue;
	    }
	}

      return V2_read_line_post (Map, line_p, line_c, Map->next_line++);
    }

  /* NOTREACHED */
}

/*********************************************************************************************
* Function name: Vect__read_line_post.
* Arguments    : Map , points, cats, id
* Return       : Status (-1 error, -2 end of table, else line type), and modify Map structures.
*    
* Description  : Seek the cursor to specified id, fetch the  geometry row, declare  a cursor 
*                for category table row with a given id and read all categories.
*
*********************************************************************************************/
int
Vect__Read_line_post (struct Map_info *Map,
		      struct line_pnts *p, struct line_cats *c, long id)
{
  char *query = (char *) calloc (65536, sizeof (char));
  char *garbage;
  int gId, gType, type;
  int i;
  int dimension;
  int linepoint;
  int cField;
  int cCat;

  G_debug (3, "Vect__Read_line_post(): id = %d", id);

  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;

  if (id >= Map->fInfo.post.nGeom)
    {
      free (query);
      return (-2);		/* last record reached */
    }
/**************************************************************************************************/
  if ((id - Map->fInfo.post.nextRow))
/*Move cursor if necessary */
    {
      sprintf (query, "MOVE %ld in g_cursor", id - Map->fInfo.post.nextRow);
      Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
      if (!Map->fInfo.post.geomRes
	  || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_COMMAND_OK)
	{
	  PQclear (Map->fInfo.post.geomRes);
	  Map->fInfo.post.geomRes = NULL;
	  free (query);
	  return (-1);
	}

    }
  Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, "FETCH IN g_cursor");	/*Fetch row in cursor */
  if (!Map->fInfo.post.geomRes
      || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_TUPLES_OK)
    {
      PQclear (Map->fInfo.post.geomRes);
      Map->fInfo.post.geomRes = NULL;
      free (query);
      return (-1);
    }
/***********Geometry Parser ***********************/

/****************************************************************************************************/
  gId = atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 0));	/* int4     Goem id */
  gType = atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 1));	/* int4     Geom type */
  if (p != NULL)
    {
      dimension = atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 3));	/* int4     Dimension 2/3 */
      linepoint = atoi (PQgetvalue (Map->fInfo.post.geomRes, 0, 2));	/* int4     Number of line points */
      /*Allocate space for points */
      switch (gType)
	{
	case 1:		/*GV_POINT */
	case 4:		/*GV_CENTROID */
	  /* Point */
	  if (0 > dig_alloc_points (p, 1))
	    {
	      free (query);
	      return (-1);
	    }
	  p->n_points = 1;
	  break;
	case 2:		/*GV_LINE */
	case 3:		/*GV_BOUNDARY */
	  /* Linestring */
	  if (0 > dig_alloc_points (p, linepoint))
	    {
	      free (query);
	      return (-1);
	    }
	  p->n_points = linepoint;
	  break;
	default:
	  free (query);
	  return (-1);
	}
    /***********************Dynamic select genarator***************************************************/
      /*Generate a Select for the specific row */
      sprintf (query, "SELECT");
      switch (gType)
	{
	case 1:		/*GV_POINT */
	case 4:		/*GV_CENTROID */
	  /* Point */
	  sprintf (query, "%s X(%s), Y(%s)", query, Map->fInfo.post.geom_geom,
		   Map->fInfo.post.geom_geom);
	  if (dimension == 3)
	    sprintf (query, "%s, Z(%s)", query, Map->fInfo.post.geom_geom);
	  break;
	case 2:		/*GV_LINE */
	case 3:		/*GV_BOUNDARY */
	  /* Linestring */
	  for (i = 0; i < linepoint; i++)
	    {
	      sprintf (query, "%s X(PointN(%s,%d)), Y(PointN(%s,%d))", query,
		       Map->fInfo.post.geom_geom, i,
		       Map->fInfo.post.geom_geom, i);
	      if (dimension == 3)
		sprintf (query, "%s ,Z(PointN(%s,%d))", query,
			 Map->fInfo.post.geom_geom, i);
	      if (i != linepoint - 1)
		sprintf (query, "%s, ", query);
	    }
	  break;
	default:
	  free (query);
	  return (-1);
	}
      sprintf (query, "%s FROM %s WHERE %s = %d;", query,
	       Map->fInfo.post.geom_table, Map->fInfo.post.geom_id, gId);
      /*Run select query */
      Map->fInfo.post.geomRes = PQexec (Map->fInfo.post.conn, query);
      if (!Map->fInfo.post.geomRes
	  || PQresultStatus (Map->fInfo.post.geomRes) != PGRES_TUPLES_OK)
	{
	  PQclear (Map->fInfo.post.geomRes);
	  Map->fInfo.post.geomRes = NULL;
	  free (query);
	  return (-1);
	}
    /***************************************************************************************************/
      switch (gType)
	{
	case 1:		/*GV_POINT */
	case 4:		/*GV_CENTROID */
	  /* Point */
	  p->x[0] =
	    strtod (PQgetvalue (Map->fInfo.post.geomRes, 0, 0), &garbage);
	  p->y[0] =
	    strtod (PQgetvalue (Map->fInfo.post.geomRes, 0, 1), &garbage);
	  if (dimension == 3)	/*  if 3rd dimension exists */
	    p->z[0] =
	      strtod (PQgetvalue (Map->fInfo.post.geomRes, 0, 2), &garbage);

	  break;
	case 2:		/*GV_LINE */
	case 3:		/*GV_BOUNDARY */
	  /* Linestring */
	  for (i = 0; i < linepoint; i++)
	    {
	      p->x[i] =
		strtod (PQgetvalue
			(Map->fInfo.post.geomRes, 0, dimension * i + 0),
			&garbage);
	      p->y[i] =
		strtod (PQgetvalue
			(Map->fInfo.post.geomRes, 0, dimension * i + 1),
			&garbage);
	      if (dimension == 3)
		p->z[i] =
		  strtod (PQgetvalue
			  (Map->fInfo.post.geomRes, 0, dimension * i + 2),
			  &garbage);
	    }
	  break;
	default:
	  free (query);
	  return (-1);
	}
    }
/***************************************************************************************************/
  Map->fInfo.post.nextRow = id + 1;
  PQclear (Map->fInfo.post.geomRes);
  Map->fInfo.post.geomRes = NULL;
  PQclear (Map->fInfo.post.catRes);
  Map->fInfo.post.catRes = NULL;
/*********Category***************************/
/*Count category with a gived id*/
  if (c != NULL)
    {
      sprintf (query, "SELECT COUNT(%s) from %s where %s=%d",
	       Map->fInfo.post.cat_id, Map->fInfo.post.cat_table,
	       Map->fInfo.post.cat_id, gId);
      Map->fInfo.post.catRes = PQexec (Map->fInfo.post.conn, query);
      if (!Map->fInfo.post.catRes
	  || PQresultStatus (Map->fInfo.post.catRes) != PGRES_TUPLES_OK)
	{
	  PQclear (Map->fInfo.post.catRes);
	  Map->fInfo.post.catRes = NULL;
	  free (query);
	  return (-1);
	}

      Map->fInfo.post.nCat = atoi (PQgetvalue (Map->fInfo.post.catRes, 0, 0));	/* int4 */
      PQclear (Map->fInfo.post.catRes);
      Map->fInfo.post.catRes = NULL;
      if (Map->fInfo.post.nCat > 0)
	{
	  sprintf (query,
		   " DECLARE  c_cursor   CURSOR FOR SELECT %s, %s, %s FROM %s WHERE %s = %d;",
		   Map->fInfo.post.cat_id, Map->fInfo.post.cat_field,
		   Map->fInfo.post.cat_cat, Map->fInfo.post.cat_table,
		   Map->fInfo.post.cat_id, gId);
	  Map->fInfo.post.catRes = PQexec (Map->fInfo.post.conn, query);
	  if (!Map->fInfo.post.catRes
	      || PQresultStatus (Map->fInfo.post.catRes) != PGRES_COMMAND_OK)
	    {
	      PQclear (Map->fInfo.post.catRes);
	      Map->fInfo.post.catRes = NULL;
	      free (query);
	      return (-1);
	    }
	  PQclear (Map->fInfo.post.catRes);
	  Map->fInfo.post.catRes = NULL;
	/************************************/
	  Vect_reset_cats (c);	/*Read all categories and insert into category array */
	  for (i = 0; i < Map->fInfo.post.nCat; i++)
	    {
	      Map->fInfo.post.catRes =
		PQexec (Map->fInfo.post.conn, "FETCH  IN c_cursor");
	      if (!Map->fInfo.post.catRes
		  || PQresultStatus (Map->fInfo.post.catRes) !=
		  PGRES_TUPLES_OK)
		{
		  PQclear (Map->fInfo.post.catRes);
		  Map->fInfo.post.catRes = NULL;
		  free (query);
		  return (-1);
		}

	      cField = atoi (PQgetvalue (Map->fInfo.post.catRes, 0, 1));
	      cCat = atoi (PQgetvalue (Map->fInfo.post.catRes, 0, 2));
	      if (!Vect_cat_set (c, cField, cCat))
		return (-1);
	      PQclear (Map->fInfo.post.catRes);
	      Map->fInfo.post.catRes = NULL;
	    }
	/************************************/
	  Map->fInfo.post.catRes =
	    PQexec (Map->fInfo.post.conn, "CLOSE c_cursor");
	  if (!Map->fInfo.post.catRes
	      || PQresultStatus (Map->fInfo.post.catRes) != PGRES_COMMAND_OK)
	    {
	      PQclear (Map->fInfo.post.catRes);
	      Map->fInfo.post.catRes = NULL;
	      free (query);
	      return (-1);
	    }

	  PQclear (Map->fInfo.post.catRes);
	  Map->fInfo.post.catRes = NULL;
	}
      free (query);
    }
/*************************************/

  type = Vect_type_from_store (gType);

  if (type == 0)
    return (-1);

  return type;
}

/*
* *  Returns  next line offset
* */
long
Vect_next_line_offset_post (struct Map_info *Map)
{
  return (Map->fInfo.post.nextRow);	/* ??? correct */
}

#endif
