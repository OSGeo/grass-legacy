/**** interior.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "cutter.h"

/*
#define DEBUG2
*/

double fabs ();

extern struct poly_t *Polys[2];	/* descriptive info about current */

/*
**
**  After everything else is done with table, there are still
**  polygons laying around that didn't intersect with anything.
**
**  Go thru each one and check to see if it is interior to 
**   a [labeled] polygon of the opposite map, and if so, write it 
**   out with appropriate label.
**
**  Note that Cutter polys should be labeled with the attribute of the
**    polygon in the data map that it is within.
**
**  This code is horribly inefficient.
**
**  returns 0 or -1 on error
*/
interior_polys (Maps, Out, Table)		/* ==ISLE== */
    struct Map_info Maps[2];		/* Cutter/data to be cut */
    struct Map_info *Out;		/* output map */
    struct t_data *Table;		/* intersection table */
{
  
  int map, othermap;
  plus_t line;
  plus_t area, area2;
  int ret;
  struct Map_info *Map, *Map2;
  P_ATT *Att;
  P_AREA *Area, *Area2;
  double X, Y;			/* interior point */
  double X2, Y2;			/* interior point */
  plus_t inside, inside2;	/* check twice just in case */
  double main_area;		/* area of current area */
  double other_area;		/* area of current poly being checked  */
  int cur_isle;
  int i;
  int new_polys = 0;
  int equal = 0;	/* hack */


  for (map = 0 ; map < 2 ; map++)	/* for each map */
  {
    othermap = !map;
    Map = &(Maps[map]);
    Map2 = &(Maps[othermap]);

    if (map == 1 && Map->n_areas == 0)
	G_percent (1, 1, 1);	/* cover incase no areas */
    for (area = 1 ; area <= Map->n_areas ; area++)
    {
      if (!Quiet)
	if (map == 0)
	  G_percent (area, Map->n_areas * 2, 5);
	else
	  G_percent (Map->n_areas + area, Map->n_areas * 2, 5);
	
      Area = &(Map->Area[area]);
      if (Area->alive == POLY_NOINTERSECT)
      {
	if (0 > Vect_get_area_points (Map, area, TPoints))
	  return inter_clean (dig_out_of_memory ());

	dig_find_area2_poly (TPoints, &main_area);

	/* There can be any number of islands inside this poly
	**  so I will repetitively call 
	**  Cut_get_point_in_poly_t () until I get a point in
	**  the polygon of which this one is an island
	*/
	poly_t_load_base_points (OPoly, TPoints);

	/*  load all island line representations into OPoly */

	i = 0;
	while (++i)
	{
	/* go thru all polys of opposite map check for inside */
	  equal = 0;

	  if (i == 1)	/* first time */
	  {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint A1\n");
#endif
	    /* first time through, no islands, so just take from Area */
	    if (Area->att)
	    {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint A1a\n");
#endif
	      X = Map->Att[Area->att].x;
	      Y = Map->Att[Area->att].y;
	    }
	    else
	    {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint A1b\n");
#endif
	      Vect_get_point_in_area (Map, area, &X, &Y);
	    }
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint A\n");
#endif
	  }
	  else
	  {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint A2\n");
#endif
	    /* if cant find a good point w/in poly, then go on to next */
	    if (-1 == Cut_get_point_in_poly_t (OPoly, &X, &Y))
	    {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint b\n");
#endif
	      break;
	    }
	  }


	  /* Use coords to point to area in opposite map */
	  /*  if none found, then we are done, nothing
	  **  needs be done with this area 
	  */
	  if (!(inside = dig_point_to_area (Map2, X, Y)))
	  {
		/* area is not an island to anything */
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint C  poly\n");
#endif
	    goto bottom;
	  }
	  else
	  {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint D  Poly %d\n", inside);
#endif
	    Area2 = &(Map2->Area[inside]);
	    dig_find_area2(Map2, Area2, &other_area);

	    /* if found area is smaller than original, then
	    **  it must be an island instead of a container
	    **  so stick it in OPoly as an island and try again
	    */

	    /* TODO */
	    /* if ((fabs (other_area - main_area) < .000001)) TEST */
	    if ((fabs (other_area - main_area) < .00001))
	    {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint D2\n");
#endif
	    /* if duplicates, only take it once */
		if (map == 0)
		{
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint D2a\n");
#endif
		    goto bottom;
		}
		else
		{
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint D2b\n");
#endif
		    equal = 1;
		}
		/* TODO */
		/* it just so happens that if the equal area is a hole,
		**  we want to write it even tho it is not labeled cuz
		**  it is really an island  (well some of the time)
		*/
	    }
	    else
	    {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint D3\n");
#endif
	      if (other_area < main_area)
	      {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "INTERIOR: found island \n");
#endif
		/* we found an island instead of the area that contains
		**  this area.
		*/
		cur_isle = OPoly->n_polys;
		alloc_poly_t (OPoly, cur_isle + 1, 0);

		/* create line points if not already there. */
		if (OPoly->spoly[cur_isle].Points == NULL)
		{
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint D4\n");
#endif
		 OPoly->spoly[cur_isle].Points = Vect_new_line_struct ();
		}
		OPoly->n_polys++;

		if (0 > Vect_get_area_points (Map2, inside, OPoly->spoly[cur_isle].Points))
		  return inter_clean (dig_out_of_memory());
		

#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint D5\n");
#endif
		continue;	/* try again */
	      }
	    }

	    /* if got here, then we found an area and it is 
	    ** bigger than the current area we are testing, thus
	    **  it must be the area that contains it. 
	    **
	    **  now, if that area is labeled, just write out
	    **   the current area and the appropriate attribute
	    */

#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHekcpoint E\n");
#endif

	    
	    /* if area it is in is not labeled, then don't worry about it. 
	    **  unless user specified to output all areas
	    */
	    if (!equal && !All && !(Map2->Area[inside].att && 
		   Map2->Att[Map2->Area[inside].att].cat) )
	    {
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHECKPOINT X\n");
#endif
		/* area it is in is not labeled so skip it */
		goto bottom;
	    }

#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHECKPOINT F\n");
#endif
	    for (line = 0 ; line < Area->n_lines ; line++)
	    {
	      ret = V2_read_line (Map, TPoints, abs(Area->lines[line]));
	      if (ret < 0)
		return inter_clean (dig_out_of_memory ());

	      write_cur_line (Maps, Out, TPoints, map, abs(Area->lines[line]));
	    }
	    new_polys++;

#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "CHECKPOINT G\n");
#endif


	    if (map == B_CODE)	/* Data Map */
	    {
	      if (Area->att && Map->Att[Area->att].cat) /* if labelled */
	      {
		Att = &(Map->Att[Area->att]);
#ifdef DEBUG2
/*DEBUG*/ fprintf (stderr, "ATTRIBUTE: 		%d\n", Att->cat);
#endif
		write_att(Out->att_fp, 'A', X, Y, Att->cat);
	      }
	    }
	    else			/* Cutter Map */
	    {		/* output attribute of DATA map if both labeled */
	      if (All || (Area->att && Map->Att[Area->att].cat)) /* if labelled */
	      {
		Area2 = &(Map2->Area[inside]);
		if (Area2->att && Map2->Att[Area2->att].cat)
		{
		  Att = &(Map2->Att[Area2->att]);
		  write_att(Out->att_fp, 'A', X, Y, Att->cat);
		}
	      }
	    }


	    goto bottom;
	  }
	}
      }
bottom:
	;
    }
  }
  return new_polys;
}

/*
** since interior_polys () used OPolys in non-standard
**  way, need to clean up afterwards
*/
inter_clean (ret)
  int ret;
{
  int i;

  for (i = 1 ; i < OPoly->n_polys ; i++)
  {
    if (OPoly->spoly[i].Points != NULL)
    {
      Vect_destroy_line_struct (OPoly->spoly[i].Points);
      OPoly->spoly[i].Points = NULL;
    }
  }

  return ret;
}

/* now go back through and fix up unlabeled areas whose bounding island 
**  was intersected 
**
**  The issue is that if an island is intersected, the entire thing will
**  be written out.  If the area[s] inside it are not labelled, the
**  only place they would get dealt with is in 'interior poly' code
**  as 'holes', but since the island itself has already been written,
**  this is unnecessary.
**
**  returns number of polys which meet criteria to be added to 
**   total count of output polys.
*/

update_intersect_table (Maps)
    struct Map_info Maps[2];		/* Cutter/data to be cut */
{
  int map;
  plus_t line;
  plus_t area, area2;
  struct Map_info *Map;
  P_AREA *Area, *Area2;
  P_ISLE *Isle;
  int i, j;
  int newpolys = 0;	/* count number of output polygons */

  for (map = 0 ; map < 2 ; map++)	/* for each map */
  {
    Map = &(Maps[map]);

    for (area = 1 ; area <= Map->n_areas ; area++)
    {
      Area = &(Map->Area[area]);

      /* if area is unlabeled */
      if (!Area->att || !(Map->Att[Area->att].cat))
      {
	/* for each line */
	for (i = 0 ; i < Area->n_lines ; i++)
	{
	  line = Area->lines[i];
	  /* check the other side of the line, if that area/isle intersected */
	  if (line < 0)		/* reversed */
	    area2 = Map->Line[-line].right;
	  else
	    area2 = Map->Line[line].left;

	  if (area2 > 0)
	  {
	    Area2 = &(Map->Area[area2]);
	    if (Area2->alive == POLY_INTERSECT)
	    {
	      Area->alive = POLY_INTERSECT;
	      newpolys++;	/* update count of polys anyway */
	      break;
	    }
	  }
	  else	/* island */
	  {
	    Isle = &(Map->Isle[abs(area2)]);
	    if (Isle->alive == POLY_INTERSECT)
	    {
	      Area->alive = POLY_INTERSECT;
	      newpolys++;	/* update count of polys anyway */
	      break;
	    }
	  }
	}
      }
    }
  }
  return newpolys;
}

dump_nointersect (Maps)
    struct Map_info Maps[2];		/* Cutter/data to be cut */
{
  
  int map, othermap;
  plus_t line;
  plus_t area, area2;
  int ret;
  struct Map_info *Map;
  P_ATT *Att;
  P_AREA *Area, *Area2;
  double X, Y;			/* interior point */
  double X2, Y2;			/* interior point */
  plus_t inside, inside2;	/* check twice just in case */
  double main_area;		/* area of current area */
  double other_area;		/* area of current poly being checked  */
  int cur_isle;
  int i;


  for (map = 0 ; map < 2 ; map++)	/* for each map */
  {
    othermap = !map;
    Map = &(Maps[map]);

    for (area = 1 ; area <= Map->n_areas ; area++)
    {
      Area = &(Map->Area[area]);
      if (Area->alive == POLY_NOINTERSECT)
      {
	fprintf (stderr, "NOINTERSECT: Map %d  Area %d\n", map, area);
      }
    }
  }
}
