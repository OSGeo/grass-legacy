/**** area.c ****/
/*
**  Written by David Gerdes  Fall 1992
**  US Army Construction Engineering Research Lab
**  Copyright  David Gerdes  USA-CERL  1992
*/


#include "Vect.h"
/*
** Returns the position of the line (0-N) in the polyon array of lines where the
**  line is found 
**
**  returns -1 on error (can't find line)
*/
int 
get_area_line_pos_subpoly (Map, poly, subpoly, line)	/* ==ISLE== */
						/* Note arg change */
  struct Map_info *Map;
  int poly, subpoly;
  plus_t line;
{
  P_AREA *Area;
  P_ISLE *Isle;
  register int i;

  Area= &(Map->Area[poly]);

  line = abs(line);

  if (subpoly == 0)
  {
    for (i = 0 ; i < Area->n_lines ; i++)
      if (abs(Area->lines[i]) == line)
	return i;
  }
  else
  {
    Isle = &(Map->Isle[Area->isles[subpoly-1]]);
    for (i = 0 ; i < Isle->n_lines ; i++)
      if (abs(Isle->lines[i]) == line)
	return i;
  }

/*DEBUG*/ debugf ("get_area_line_pos failed to find line (%d,%d)\n", poly, line);
  return -1;
}

/*
** Returns the position of the line (0-N) in the polyon array of lines where the
**  line is found 
**
**  Also works for islands if poly is negative
**
**  returns -1 on error (can't find line)
*/
int 
get_area_line_pos (Map, poly, line)	/* ==ISLE== */
  struct Map_info *Map;
  int poly;
  plus_t line;
{
  P_AREA *Area;
  P_ISLE *Isle;
  register int i;


  line = abs(line);

  if (poly > 0)
  {
    Area= &(Map->Area[poly]);
    for (i = 0 ; i < Area->n_lines ; i++)
      if (abs(Area->lines[i]) == line)
	return i;
  }
  else
  {
    Isle = &(Map->Isle[abs(poly)]);
    for (i = 0 ; i < Isle->n_lines ; i++)
      if (abs(Isle->lines[i]) == line)
	return i;
  }

/*DEBUG*/ debugf ("get_area_line_pos failed to find line (%d,%d)\n", poly, line);
  return -1;
}


/* returns the number of the next line in the polygon array of lines 
**  Also works for islands if poly is negative
**
**  returns -1 on error (can't find line)
*/
plus_t 
get_next_area_line (Map, poly, line)	/* ==ISLE== */
  struct Map_info *Map;
  plus_t poly;
  plus_t line;
{
  P_AREA *Area;
  P_ISLE *Isle;
  register int i;


  if (poly > 0)
  {
    Area= &(Map->Area[poly]);
    for (i = 0 ; i < Area->n_lines ; i++)
      if (abs(Area->lines[i]) == abs(line))
	return Area->lines[(i+1) % Area->n_lines];
  }
  else
  {
    Isle = &(Map->Isle[abs(poly)]);
    for (i = 0 ; i < Isle->n_lines ; i++)
      if (abs(Isle->lines[i]) == abs(line))
	return Isle->lines[(i+1) % Isle->n_lines];
  }

  return -1;
}

/* returns the number of the prev line in the polygon array of lines 
**
**  returns -1 on error (can't find line)
*/
plus_t 
get_prev_area_line (Map, poly, line)	/* ==ISLE== */
  struct Map_info *Map;
  plus_t poly;
  plus_t line;
{
  P_AREA *Area;
  P_ISLE *Isle;
  register int i;

  Area= &(Map->Area[poly]);

  if (poly > 0)	/* Area */
  {
    for (i = 0 ; i < Area->n_lines ; i++)
      if (abs(Area->lines[i]) == abs(line))
      {
	if (i == 0)
	    return Area->lines[Area->n_lines-1];
	else
	    return Area->lines[i-1];
      }
  }
  else		/* island */
  {
    Isle = &(Map->Isle[abs(poly)]);
    for (i = 0 ; i < Isle->n_lines ; i++)
      if (abs(Isle->lines[i]) == abs(line))
      {
	if (i == 0)
	    return Isle->lines[Isle->n_lines-1];
	else
	    return Isle->lines[i-1];
      }
  }

  return -1;
}
