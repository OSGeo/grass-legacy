#include "Vect.h"
#include "globals2.h"

#define AREA_THRES 1.0e-5

/*much of this code is copied over from core vector routines (v.build), 
**and in some cases modified only slightly. modifications for SDTS purposes
**are by Tin Qian, Fall 1993-Summer 1994.
*/

/* [comments from original source]
** build_polygon_with_line ()
** this is the real guts of build area stuff
**
**  take a given line and start off to the RIGHT/LEFT and try to complete
**  an area.   
**  Possible Scenarios:
**	path runs into a previous line, polygon'area is positive : area
**      path runs into a previous line, polygon'area is negative : isle
**      path runs into a previous line, polygon'area is zero     : line
**
**  after we find an isle then we call point_in_area to see if the
**   specified point is w/in the area
**
**  returns  -1:  error   0:  no area    1:  point in area
**  -2: island  !!
*/

struct attr {   /* BEBUG only */
  plus_t left;
  plus_t right;
  } ;
typedef struct attr poly_attr;	/* DEBUG only */

static poly_attr *poly_edge;	/* DEBUG only */

static plus_t new_polygon;
int *SDTS_poly_index;

dig_build_polygon_with_line (map, first_line, Area)
    struct Map_info *map;
    int first_line;
    P_AREA *Area;
{
    register int i;
    int prev_line, next_line;
    /*
    double cent_x, cent_y;
    */
    double totalarea;
			    /* holds lines in area */
    static plus_t *array;
    int area_lines;
    char *p;
    static int array_size; /* 0 on startup */
    int n_lines;

    if (array_size == 0)	/* first time */
    {
	array_size = 1000;
	array = (plus_t *) dig__falloc (array_size, sizeof (plus_t));
	if (array == NULL)
	    return (dig_out_of_memory());
    }
    /*DEBUG*/ debugf ("_BUILD_AREA: first_line %d\n", first_line);

	array[0] = first_line;
	prev_line = -first_line;
	n_lines = 1;
	area_lines = 1;
	while (1)			/* when in doubt, start w/ an infinite loop */
    {
      if (prev_line > 0)	/* DEBUG only */
	poly_edge [prev_line].right = new_polygon;
      else
	poly_edge [-prev_line].left = new_polygon;

	next_line = dig_angle_next_poly_line (map, prev_line, RIGHT);
      
	if (next_line == 0)
	    return (-1);

	/*  this also handles the problem w/ 1 single area line */
	if (first_line == next_line)
	{
	    /* GOT ONE!  fill area struct  and return */
/*DEBUG*/ debugf ("GOT ONE! :");

	    if (0 > dig_area_alloc_line (Area, n_lines))
		return (-1);
	    for (i = 0 ; i < n_lines ; i++)
	    {
/*DEBUG*/ debugf ("    %d, ", array[i]);
		Area->lines[i] = array[i];
	    }
/*DEBUG*/ debugf ("\n");
	    Area->n_lines = n_lines;
	    /* att will be filled in later */
	    Area->att = 0;
	    dig_area_bound_box (map, Area);

	  /* What we found is just single lines */
	    if (!area_lines)
	      return (-1);

	    /* check if we are going have an island instead of an area */
	    dig_find_area2 (map, Area, &totalarea);
	    if (totalarea < 0)
	    {
/*DEBUG*/ debugf ("Area space was Negative. An Island.\n");  /* ISLE */
		return (-2);
	    }

	  if (totalarea == 0)	/* comparing a double with 0 is not reliable */
	    return (-1);
          else
	    return (1);
	}

	/* area_lines is used to count the number of lines which actually 
	   encompass some areaes. */	
	if ((next_line < 0 ? poly_edge[-next_line].left :
	     poly_edge[next_line].right) == new_polygon)
	   area_lines --;
	else
 	   area_lines ++;
							
        if ((next_line > 0 ? poly_edge[next_line].left
	   : poly_edge[-next_line].right) == new_polygon)	/* DEBUG only */
	   fprintf(stderr,"traverse into a previous line but not first_line\n");

	/* otherwise keep going */
	if (n_lines >= array_size)
	{
	    p = dig__frealloc(array,array_size+100,sizeof(plus_t),array_size);
	    if (p == NULL)
		return (dig_out_of_memory ());
	    array = (plus_t *) p;
	    array_size += 100;
	}
	array[n_lines++] = next_line;
	prev_line = -next_line;
    }
}

/* 
** angle_next_line ()
**   current_line will be negative if we are looking at Node 2
**    return line number of next angle to follow an area boundary
**	assume that lines are sorted in increasing angle order
**    returns 0 on error or not found
*/


dig_angle_next_poly_line (map, current_line, direction)
    struct Map_info *map;
    int current_line;		/* current line number */
    int direction;
{
    register int next;
    register int current;
    plus_t node;
    P_NODE *Node;

    if (current_line < 0)
      node = map->Line[abs(current_line)].N2;
    else
	node = map->Line[current_line].N1;
    Node = &(map->Node[node]);

    /* first find index for that line */
    for (current = 0 ; current < Node->n_lines ; current++)
	if (Node->lines[current] == current_line)
	    goto start;
/*DEBUG*/ debugf ("Angle_next_line:  Line NOT found at node %d\n", (int) node);
    return (0);	/* not found */

start:
    next = current;
    if (direction == RIGHT)
    {
	do {
	    if (next == Node->n_lines - 1)
		next = 0;
	    else
		next++;
	} while (map->Line[abs(Node->lines[next])].type==DOT && next!=current);
    }
    else
    {
	do {
	    if (next == 0)
		next = Node->n_lines -1;
	    else
		next--;
	} while (map->Line[abs(Node->lines[next])].type==DOT && next!=current);
    }
    return ((int) (Node->lines[next]));
}


double
dig_isle_in_area (map, pi)
     struct Map_info *map;
     P_ISLE *pi;
{
  P_AREA *pa = &(map->Area[1]);
  P_NODE *pn;
  int choice = 0, area, i;
  double new_dist, cur_dist;
  int got_one, first = 1, thesame;

  for (area = 1; area <= map->n_areas; area++, pa++)
    {
      if (AREA_ALIVE (pa))
	{
	  if (pi->W<pa->W || pi->E>pa->E || pi->S<pa->S || pi->N>pa->N)
	    continue;
	  thesame = 0;
	  for (i = 0; i<pa->n_lines ; i++)
	    if (pa->lines[i] == - *pi->lines)
	      {
		thesame = 1;
		break;
	      }
	  if (thesame)
	    continue;
	  
	  pn = &(map->Node[map->Line[abs(*(pi->lines))].N1]);

	  if ((new_dist = dig_point_in_area (map, pn->x, pn->y, pa))!=0.0)
	    {
	      if (first)
		{
		  cur_dist = new_dist;
		  first = 0;
		}
	      if ((++got_one == 1) || new_dist <= cur_dist)
		{
		  choice = area;
		  cur_dist = new_dist;
		}
	    }
	}
    }
  return choice;
}

/*  There is something hacking here: 
	* When an area is deleted, its attribute will not be deleted.
	  Because those new polygons generated inside this area need
	  that attribute.  Also the old area's att num must be preserved
	  so that those new polygons know which attribute they share.
*/
void 
delete_poly ( map, poly_num )
     struct Map_info *map;
     int poly_num;
{
  if (poly_num > 0)
    {
      /* An area.  Mark it deleted in area_deleted.  The things
	 done here is quite similar to dig_del_area except not
	 clear references in lines */
      P_AREA *Area;
      P_ATT *Att;

      Area = &(map->Area[poly_num]);
      if (Area->alloc_lines)
	free (Area->lines);             /* release memory */
      Area->alloc_lines = 0;
      Area->n_lines = 0;

      /* The original code also call dig_del_isle to delete islands inside
	 the area.  I left them out since I didn't see any reason to delete
	 these islands. */
      
      if (Area->alloc_isles)
	free (Area->isles);             /* release memory */
      Area->alloc_isles = 0;
      Area->n_isles = 0;

      Area->alive = 0;    /* and mark itself as deleted */

    }
  else
    {
      /* An island. The same thing as dig_del_isle except not deleting
	 the referecencs in lines. */
      P_ISLE *Isle;
      P_ATT *Att;

      Isle = &(map->Isle [abs(poly_num)]);

      if (Isle->alloc_lines)
	{
	  free (Isle->lines);             /* release memory */
	}
      Isle->alloc_lines = 0;
      Isle->n_lines = 0;

      Isle->alive = 0;    /* and mark itself as deleted */

    }
}
	       
/* add new isle
**  allocate space for new isle and copy info from Area to main array
**  Area has previously been filled with correct isle info
**
**  then for each line in isle, update line (right,left) info
*/
/* need to clean up error handling stuff */
dig_new_isle2 (map, Area, area)
    struct Map_info *map;
    P_AREA *Area;
    plus_t area;
{
    register int i;
    /*
    register int j;
    */
    register int isle, line;
    P_ISLE *TO;

    if (0 > dig_alloc_isle (map, 1))
	return (-1);

    isle = ++(map->n_isles);
    TO = &(map->Isle[isle]);

    TO->N = Area->N;/*ISLE*/
    TO->S = Area->S;
    TO->E = Area->E;
    TO->W = Area->W;
    TO->area = area;  /*ISLE*/

    TO->alive = 1;
    TO->alloc_lines = Area->alloc_lines;
    TO->n_lines = Area->n_lines;
    TO->lines = Area->lines;

    for (i = 0 ; i < Area->n_lines ; i++)
    {
	/* Chris Emmerich of Autometric found this bug  12/20/89
	**  I was reversing the order of the lines found that created
	**  the island, thinking they should be the same as areas 
	**  i.e. clockwise around the island (and doing it incorrectly.  I 
	**  should have negated line lines after reversal).  The correct
	**  soln is to leave them as found going counter clockwise around
	**  island.
	*/
	line = TO->lines[i];		/* copy line info */

	/* all code must be cleaned up */
	if (line < 0)	/*ISLE*/		/* reference lines to isle */
	{
#if 0
/*DEBUG*/ if (map->Line[abs(line)].left)
/*DEBUG*/ debugf ("Line %d already had isle %d to left.\n", line, map->Line[abs(line)].left);
#endif
	  int a_left = map->Line[abs(line)].left;

	  if (a_left<0 && map->Isle[abs(a_left)].alive)
	    delete_poly (map, a_left);
          else
            if (a_left>0)
              {
                fprintf (stderr, "Line %d has an area %d to left while it should be an isle\n", line, a_left);
		fprintf (stderr, "There may be some lines crossing without node nearby\n\n");
              }

	  map->Line[abs(line)].left = -isle;
	}
	else
	{
#if 0
/*DEBUG*/ if (map->Line[line].right)
/*DEBUG*/ debugf ("Line %d already had isle %d to right.\n", line, map->Line[line].right);
#endif 
	  int a_right = map->Line[abs(line)].right;

	  if (a_right<0 && map->Isle[abs(a_right)].alive)
	    delete_poly (map, a_right);
          else
            if (a_right >0)
              {
                fprintf (stderr, "Line %d has an area %d to right while it should be an isle\n", line, a_right);
		fprintf (stderr, "There may be some lines crossing without node nearby\n\n");
              }
	  map->Line[line].right = -isle;
	}
    }
    TO->n_lines = Area->n_lines;
    return (isle);
}

/* add new area
**  allocate space for new area and copy info from Area to main array
**  Area has previously been filled with correct area info
**
**  then for each line in area, update line (right,left) info
*/
/* need to clean up error handling stuff */
dig_new_area2 (map, Area, att)
    struct Map_info *map;
    P_AREA *Area;
{
    register int i;
    register int area, line;
    P_AREA *TO;

    if (0 > dig_alloc_area (map, 1))
	return (-1);

    area = ++(map->n_areas);
    TO = &(map->Area[area]);
    dig_struct_copy (Area, TO, sizeof (P_AREA));

    TO->alive = 1;
    TO->att = att;
    TO->alloc_lines = Area->alloc_lines;
    TO->n_lines = Area->n_lines;

    /* island stuff */
    TO->alloc_isles = 0;
    TO->n_isles = 0;
    TO->isles = NULL;

    for (i = 0 ; i < Area->n_lines ; i++)
    {
	line = TO->lines[i];

	if (line < 0)			/* reference lines to area */
	{
#if 0
/*DEBUG*/ if (map->Line[abs(line)].left)
/*DEBUG*/ debugf ("Line %d already had area %d to left.\n", line, map->Line[abs(line)].left);
#endif
	  /* This is a kind hacking to make use of old topological information
	     of vector files.  Once v.support use the same algorithm used here
	     all these code can be deleted.  */

	  int a_left;

          if ((a_left = map->Line[abs(line)].left)>0)	
	    {
	      /* Using negative att num to share label (attribute) with
		 the old area which those new polygons are inside . */
	      TO->att = - map->Area[abs(a_left)].att;
              if (map->Area[abs(a_left)].alive)
	        delete_poly (map, a_left);
	    }
	  map->Line[abs(line)].left = area;
	}
	else
	{
#if 0
/*DEBUG*/ if (map->Line[abs(line)].right)
/*DEBUG*/ debugf ("Line %d already had area %d to right.\n", line, map->Line[abs(line)].right);
#endif
	  int a_right;

	  if ((a_right = map->Line[abs(line)].right)>0)
	    {
	      /* Using negative att num to share label (attribute) with
		 the old area which those new polygons are inside . */
	      TO->att = - map->Area[abs(a_right)].att;
	      if (map->Area[abs(a_right)].alive)
	        delete_poly (map, a_right);
 	    }
	  map->Line[line].right = area;
	}
    }
    TO->n_lines = Area->n_lines;
    return (area);
}

int
build_all_polygons ( map )
     struct Map_info *map;
{
  register int cnt = 0 , i, recnum;

  if (!Aline_only)
  {
  /* DEBUG  only */
  if ((poly_edge = (poly_attr *)dig__falloc(map->n_lines+1, sizeof (poly_attr)))
      == NULL)
    return (dig_out_of_memory());

  new_polygon = map->n_areas;
  
  for (i = 1 ; i<=map->n_lines ; i++)
    {
      P_LINE *pl = &(map->Line[i]);

      if (LINE_ALIVE(pl) && pl->type != DOT)
	{
	  if (pl->right == 0)
	    cnt += build_polygon (map, i);
	  if (pl->left == 0)
	    cnt += build_polygon (map, -i);
	}
    }
  for (i = 1 ; i<=map->n_isles ; i++)
    {
      P_ISLE *pi = &(map->Isle[i]);
      plus_t area, line;
      
      if (ISLE_ALIVE (pi))
        {
          if (pi->area)
	    area = pi->area;
          else
	    area = dig_isle_in_area (map, pi);

          for (line = 0; line < pi->n_lines; line++ )
	    {
	       P_LINE *pl = &(map->Line[abs(pi->lines[line])]);
	  

	       if (pl->left < 0)
	         pl->left = area;
	       if (pl->right < 0)
	         pl->right = area;
	    }
        }
    }
  }
  if ((SDTS_poly_index = (int *)dig__falloc(map->n_areas + 1, sizeof (int))) == NULL)
    return (dig_out_of_memory());
 
  /* here dealing with the mapping of polygon number between SDTS and GRASS:
     GRASS: 0 ==> SDTS: 1, ..... */ 
  recnum = 1;
  SDTS_poly_index [0] = recnum ++;
  for (i = 1; i <= map->n_areas; i ++)
    if (AREA_ALIVE (&(map->Area[i])))
      SDTS_poly_index [i] = recnum ++;
    else
      SDTS_poly_index [i] = 0;

  return (cnt);
}


int
build_polygon (map, line)
     struct Map_info *map;
     int line;
{
  int poly = 0;
  P_AREA Area;

  Area.n_lines = Area.alloc_lines = 0;

  new_polygon ++;
  
  switch (dig_build_polygon_with_line (map, line, &Area))
    {
    case 1:
      poly = dig_new_area2 (map, &Area, 0);
/*
      free (Area.lines);
*/
      break;

    case -1:
    case -2:
      dig_new_isle2 (map, &Area, 0);
/*
      free (Area.lines);
*/
      break;
    }

  return ( poly?1:0 );
}




