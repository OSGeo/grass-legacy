/**** interline.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "bitmap.h"
#include "cutter.h"

extern struct poly_t *Polys[2];	/* descriptive info about current */

/*
**
**  After everything else is done with table, there are still
**  lines laying around that didn't intersect with anything.
**
**  Go thru each one and check to see if it is interior to 
**   a [labeled] polygon of the cutter map, and if so, write it 
**   out with appropriate label.
**
**  returns 0 or -1 on error
*/
interior_lines (Maps, Out, Table)		/* ==ISLE== */
    struct Map_info Maps[2];		/* Cutter/data to be cut */
    struct Map_info *Out;		/* output map */
    struct t_data *Table;		/* intersection table */
{
  
  int map, othermap;
  plus_t line;
  int ret;
  struct Map_info *Map, *Map2;
  P_ATT *Att;
  P_LINE *Line, *Line2;
  double X, Y;			/* interior point */
  double X2, Y2;			/* interior point */
  plus_t inside;	/* check twice just in case */
  int i;
  int new_lines = 0;


  {
    map = B_CODE;
    othermap = A_CODE;
    Map = &(Maps[B_CODE]);
    Map2 = &(Maps[othermap]);

    if (Map->n_lines == 0)
	G_percent (1, 1, 1);	/* cover incase no lines */
    for (line = 1 ; line <= Map->n_lines ; line++)
    {
      if (!Quiet)
	  G_percent (line, Map->n_lines, 5);
	
      Line = &(Map->Line[line]);

      if (Line->type == AREA)	/* only lines and sites  */
	continue;

      /* check to see if line had intersection */
      if (!BM_get (intersect_bitmap, line, 0))
      {
	if (0 > V2_read_line (Map, TPoints, line))
	  return dig_out_of_memory ();

	  /* get a point representing the line */
	  /* TODO for now assume line does not touch
	  **  any areas.  If so, well this is undefined 
	  **  if the point we pick is on the edge of the area
	  */
	  get_line_center (&X, &Y, TPoints);


	/* Use coords to point to area in cutter map */
	/*  if none found, then we are done, nothing
	**  needs be done with this area 
	*/
	if (inside = dig_point_to_area (Map2, X, Y))
	{
	  /* if area it is in is not labeled, then don't worry about it. 
	  **  unless user specified to output all areas
	  */
	  if (All || (Map2->Area[inside].att && 
		 Map2->Att[Map2->Area[inside].att].cat) )
	  {
	    write_cur_line (Maps, Out, TPoints, map, line);
	    new_lines++;


#ifdef FOO
	    if (Line->att && Map->Att[Line->att].cat) /* if labelled */
	    {
	      Att = &(Map->Att[Line->att]);
	      write_att(Out->att_fp, (char) dig_new_to_old_type (Att->type), Att->x,Att->y, Att->cat);
	    }
#endif
	  }
	}
      }
    }
  }
  return new_lines;
}
