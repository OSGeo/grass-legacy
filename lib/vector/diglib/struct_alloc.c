/*
   **  Written by: Dave Gerdes 5 1988
   **  US Army Construction Engineering Research Lab
 */

#include "Vect.h"

/*  These routines all eventually call calloc() to allocate and zero
   **  the new space.  BUT It is not neccessarily safe to assume that
   **  the memory will be zero.  The next memory location asked for could
   **  have been previously used and not zeroed.  (e.g. compress())
 */

/* alloc_node (map, add)
   ** alloc_line (map, add)
   ** alloc_area (map, add)
   ** alloc_points (map, num)
   ** node_alloc_line (node, add)
   ** area_alloc_line (node, add)
   **
   **   Allocate array space to add 'add' elements
 */

/* node_alloc_line (node, add)
   **     allocate space in  P_node,  lines and angles arrays to add 'add' more
   **     lines
   **
   **  Returns   0 ok    or    -1 on error
 */
int 
dig_node_alloc_line (
		      P_NODE * node,
		      int add)
{
  int alloced;
  char *p;
  int num;

  num = node->n_lines + add;

  alloced = node->alloc_lines;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 2, (char *) node->lines,
			  sizeof (plus_t))))
    {
      return (dig_out_of_memory ());
    }
  node->lines = (plus_t *) p;

  alloced = node->alloc_lines;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 2, (char *) node->angles,
			  sizeof (float))))
    {
      return (dig_out_of_memory ());
    }
  node->angles = (float *) p;
  node->alloc_lines = alloced;
  return (0);
}

int 
dig_alloc_node (
		 struct Map_info *map,
		 int add)
{
  int alloced;
  char *p;
  int num;

  num = map->n_nodes + 1 + add;

  alloced = map->alloc_nodes;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 128, (char *) map->Node,
			  sizeof (P_NODE))))
    {
      return (dig_out_of_memory ());
    }
  map->Node = (P_NODE *) p;
  map->alloc_nodes = alloced;
  return (0);
}

int 
dig_alloc_line (
		 struct Map_info *map,
		 int add)
{
  int alloced;
  char *p;
  int num;

  num = map->n_lines + 1 + add;

  alloced = map->alloc_lines;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 128, (char *) map->Line,
			  sizeof (P_LINE))))
    {
      return (dig_out_of_memory ());
    }
  map->Line = (P_LINE *) p;
  map->alloc_lines = alloced;
  return (0);
}

int 
dig_alloc_area (
		 struct Map_info *map,
		 int add)
{
  int alloced;
  char *p;
  int num;

  num = map->n_areas + 1 + add;

  alloced = map->alloc_areas;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 128, (char *) map->Area,
			  sizeof (P_AREA))))
    {
      return (dig_out_of_memory ());
    }
  map->Area = (P_AREA *) p;
  map->alloc_areas = alloced;
  return (0);
}

int 
dig_alloc_isle (
		 struct Map_info *map,
		 int add)
{
  int alloced;
  char *p;
  int num;

  num = map->n_isles + 1 + add;

  alloced = map->alloc_isles;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 128, (char *) map->Isle,
			  sizeof (P_ISLE))))
    {
      return (dig_out_of_memory ());
    }
  map->Isle = (P_ISLE *) p;
  map->alloc_isles = alloced;
  return (0);
}


/* allocate room for  'num'   X and Y  arrays in struct line_pnts 
   **   returns -1 on out of memory 
 */
int 
dig_alloc_points (
		   struct line_pnts *points,
		   int num)
{
  int alloced;
  char *p;

  alloced = points->alloc_points;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 50, (char *) points->x,
			  sizeof (double))))
    {
      return (dig_out_of_memory ());
    }
  points->x = (double *) p;

  alloced = points->alloc_points;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 50, (char *) points->y,
			  sizeof (double))))
    {
      return (dig_out_of_memory ());
    }
  points->y = (double *) p;

  alloced = points->alloc_points;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 50, (char *) points->z,
			  sizeof (double))))
    {
      return (dig_out_of_memory ());
    }
  points->z = (double *) p;

  points->alloc_points = alloced;
  return (0);
}

/* allocate room for  'num'  fields and category arrays 
   ** in struct line_cats 
   **   returns -1 on out of memory 
 */
int 
dig_alloc_cats (
		 struct line_cats *cats,
		 int num)
{
  int alloced;
  char *p;

  /* alloc_space will just return if no space is needed */
  alloced = cats->alloc_cats;
  if (!(p =
	dig__alloc_space (num, &alloced, 1, (char *) cats->field,
			  sizeof (GRASS_V_FIELD))))
    {
      return (dig_out_of_memory ());
    }
  cats->field = (GRASS_V_FIELD *) p;

  alloced = cats->alloc_cats;
  if (!(p =
	dig__alloc_space (num, &alloced, 1, (char *) cats->cat,
			  sizeof (GRASS_V_CAT))))
    {
      return (dig_out_of_memory ());
    }
  cats->cat = (GRASS_V_CAT *) p;

  cats->alloc_cats = alloced;
  return (0);
}

/* area_alloc_line (area, num)
   **     allocate space in  P_area,  line array to num lines
   **
   **  Returns   0 ok    or    -1 on error
 */
int 
dig_area_alloc_line (
		      P_AREA * area,
		      int num)
{
  int alloced;
  char *p;
/*
   int num;

   num = area->n_lines + add;
 */

  alloced = area->alloc_lines;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 4, (char *) area->lines,
			  sizeof (plus_t))))
    {
      return (dig_out_of_memory ());
    }
  area->lines = (plus_t *) p;

  area->alloc_lines = alloced;
  return (0);
}

/* area_alloc_isle (area, num)
   **     allocate space in  P_area,  isle array to num isle
   **
   **  Returns   0 ok    or    -1 on error
 */
int 
dig_area_alloc_isle (
		      P_AREA * Area,
		      int num)
{
  int alloced;
  char *p;
/*
   int num;

   num = Area->n_isles + add;
 */

  alloced = Area->alloc_isles;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 4, (char *) Area->isles,
			  sizeof (plus_t))))
    {
      return (dig_out_of_memory ());
    }
  Area->isles = (plus_t *) p;

  Area->alloc_isles = alloced;
  return (0);
}


/* isle_alloc_line (isle, num)
   **     allocate space in  P_isle,  line array to num lines
   **
   **  Returns   0 ok    or    -1 on error
 */
int 
dig_isle_alloc_line (
		      P_ISLE * isle,
		      int num)
{
  int alloced;
  char *p;
/*
   int num;

   num = isle->n_lines + add;
 */

  alloced = isle->alloc_lines;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 4, (char *) isle->lines,
			  sizeof (plus_t))))
    {
      return (dig_out_of_memory ());
    }
  isle->lines = (plus_t *) p;

  isle->alloc_lines = alloced;
  return (0);
}

int 
dig_alloc_att (
		struct Map_info *map,
		int add)
{
  int alloced;
  char *p;
  int num;

  num = map->n_atts + 1 + add;

  alloced = map->alloc_atts;
  /* alloc_space will just return if no space is needed */
  if (!(p =
	dig__alloc_space (num, &alloced, 128, (char *) map->Att,
			  sizeof (P_ATT))))
    {
      return (dig_out_of_memory ());
    }
  map->Att = (P_ATT *) p;
  map->alloc_atts = alloced;
  return (0);
}


/* for now just print message and return error code */
int 
dig_out_of_memory ()
{
  fprintf (stderr, "OUT OF MEMORY!\n");
  return (-1);
}
