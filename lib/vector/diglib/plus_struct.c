/*
   ** Written by: Dave Gerdes 5 1988
   ** US Army Construction Engineering Research Lab
   ** Modified by: Dave Gerdes 9 1988   portable
   ** Modified by: Dave Gerdes 1 1990   more portable
 */

#include "Vect.h"

// #define SUPPORT_PROG "v.support"
/*
* Routines for reading and writing Dig+ structures.
* return 0 on success, -1 on failure of whatever kind
* if you dont want it written out, then dont call these routines
* ie  check for deleted status before calling a write routine
* in as much as it would be nice to hide that code in here,
* this is a library routine and we chose to make it dependant on
* as few external files as possible 
*/

/*  These routines assume ptr->alloc_lines  is valid 
*   Make sure it is initialized before calling 
*/

/*
*  Internally, my default variables for lines/areas/nodes/isles  are type
*  plus_t  which is typedefed as short.  This limits the current version
*  to no more than 32K lines, nodes etc. (excluding points)
*  All in the name of future expansion, I have converted these values to 
*  longs in the dig_plus data file.
*
*  NOTE: 3.10 changes plus_t to  ints.
*    This assumes that any reasonable machine will use 4 bytes to
*    store an int.  The mapdev code is not guaranteed to work if
*    plus_t is changed to a type that is larger than an int.
*/

int 
dig_Rd_P_node (
		  struct Plus_head *Plus,
		  int  n,
		  FILE * fp)
{
  P_NODE *ptr;

#ifdef GDEBUG
  G_debug (3, "dig_Rd_P_node()");
#endif
  
  ptr = dig_alloc_node();
  
  if (0 >= dig__fread_port_D (&(ptr->x), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_D (&(ptr->y), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_P (&(ptr->n_lines), 1, fp))
    return (-1);

  if ( dig_node_alloc_line ( ptr, ptr->n_lines) == -1)
     return -1; 

  if (ptr->n_lines)		/* Not guaranteed what fread does w/ 0 */
    {
      if (0 >= dig__fread_port_P (ptr->lines, ptr->n_lines, fp))
	return (-1);
      if (0 >= dig__fread_port_F (ptr->angles, ptr->n_lines, fp))
	return (-1);
    }
  
  Plus->Node[n] = ptr;
  
  return (0);
}

int 
dig_Wr_P_node (
		  struct Plus_head *Plus,
		  int  n,
		  FILE * fp)
{
  P_NODE *ptr; 

  ptr = Plus->Node[n];
  
  if (0 >= dig__fwrite_port_D (&(ptr->x), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->y), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_lines), 1, fp))
    return (-1);

  if (ptr->n_lines)
    {
      if (0 >= dig__fwrite_port_P (ptr->lines, ptr->n_lines, fp))
	return (-1);
      if (0 >= dig__fwrite_port_F (ptr->angles, ptr->n_lines, fp))
	return (-1);
    }
  return (0);
}

int 
dig_Rd_P_line (
		  struct Plus_head *Plus,
		  int  n,
		  FILE * fp)
{
  P_LINE *ptr; 
#ifdef GDEBUG
  G_debug (3, "dig_Rd_P_line()");
#endif

  ptr = dig_alloc_line();

  if (0 >= dig__fread_port_P (&(ptr->N1), 1, fp))
    return -1;
  if (0 >= dig__fread_port_P (&(ptr->N2), 1, fp))
    return -1;
  if (0 >= dig__fread_port_P (&(ptr->left), 1, fp))
    return -1;
  if (0 >= dig__fread_port_P (&(ptr->right), 1, fp))
    return -1;

  if (0 >= dig__fread_port_D (&(ptr->N), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->S), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->E), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->W), 1, fp))
    return -1;

  if (0 >= dig__fread_port_L (&(ptr->offset), 1, fp))
    return (-1);

  if (0 >= dig__fread_port_C (&(ptr->type), 1, fp))
    return (-1);

  Plus->Line[n] = ptr;
  return (0);
}

int 
dig_Wr_P_line (
		  struct Plus_head *Plus,
		  int  n,
		  FILE * fp)
{
  P_LINE *ptr; 

  ptr = Plus->Line[n];
  
  if (0 >= dig__fwrite_port_P (&(ptr->N1), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->N2), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->left), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->right), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_D (&(ptr->N), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->S), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->E), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->W), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_L (&(ptr->offset), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_C (&(ptr->type), 1, fp))
    return (-1);

  return (0);
}

int 
dig_Rd_P_area (
		  struct Plus_head *Plus,
		  int  n,
		  FILE * fp)
{
  P_AREA *ptr; 
#ifdef GDEBUG
  G_debug (3, "dig_Rd_P_area(): n = %d", n );
#endif

  ptr = dig_alloc_area();

  if (0 >= dig__fread_port_D (&(ptr->N), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->S), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->E), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->W), 1, fp))
    return -1;

  if (0 >= dig__fread_port_P (&(ptr->n_lines), 1, fp))
    return -1;
  
  if (0 >= dig__fread_port_P (&(ptr->n_centroids), 1, fp))
    return -1;
  
  if (0 >= dig__fread_port_P (&(ptr->n_isles), 1, fp))
    return -1;

  if ( dig_area_alloc_line ( ptr, ptr->n_lines) == -1)
     return -1; 

  if ( dig_area_alloc_centroid ( ptr, ptr->n_centroids) == -1)
     return -1; 

  if ( dig_area_alloc_isle ( ptr, ptr->n_isles) == -1)
     return -1; 

  if (ptr->n_lines)
    if (0 >= dig__fread_port_P (ptr->lines, ptr->n_lines, fp))
      return -1;

  if (ptr->n_centroids)
    if (0 >= dig__fread_port_P (ptr->centroids, ptr->n_centroids, fp))
      return -1;

  if (ptr->n_isles)
    if (0 >= dig__fread_port_P (ptr->isles, ptr->n_isles, fp))
      return -1;

  Plus->Area[n] = ptr;
  
  return (0);
}

int 
dig_Wr_P_area (
		  struct Plus_head *Plus,
		  int  n,
		  FILE * fp)
{
  P_AREA *ptr; 

  ptr = Plus->Area[n];
  
  if (0 >= dig__fwrite_port_D (&(ptr->N), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->S), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->E), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->W), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_P (&(ptr->n_lines), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_centroids), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_isles), 1, fp))
    return (-1);

  if (ptr->n_lines)
    if (0 >= dig__fwrite_port_P (ptr->lines, ptr->n_lines, fp))
      return -1;

  if (ptr->n_centroids)
    if (0 >= dig__fwrite_port_P (ptr->centroids, ptr->n_centroids, fp))
      return -1;

  if (ptr->n_isles)
    if (0 >= dig__fwrite_port_P (ptr->isles, ptr->n_isles, fp))
      return -1;

  return (0);
}

int 
dig_Rd_P_isle (
		  struct Plus_head *Plus,
		  int  n,
		  FILE * fp)
{
  P_ISLE *ptr; 
#ifdef GDEBUG
  G_debug (3, "dig_Rd_P_isle()");
#endif

  ptr = dig_alloc_isle();

  if (0 >= dig__fread_port_D (&(ptr->N), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->S), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->E), 1, fp))
    return -1;
  if (0 >= dig__fread_port_D (&(ptr->W), 1, fp))
    return -1;

  if (0 >= dig__fread_port_P (&(ptr->area), 1, fp))
    return -1;
  
  if (0 >= dig__fread_port_P (&(ptr->n_lines), 1, fp))
    return -1;

  if ( dig_isle_alloc_line ( ptr, ptr->n_lines) == -1)
     return -1;
  
  if (ptr->n_lines)
    if (0 >= dig__fread_port_P (ptr->lines, ptr->n_lines, fp))
      return -1;

  Plus->Isle[n] = ptr;
  
  return (0);
}

int 
dig_Wr_P_isle (
		  struct Plus_head *Plus,
		  int  n,
		  FILE * fp)
{
  P_ISLE *ptr; 

  ptr = Plus->Isle[n];
  
  if (0 >= dig__fwrite_port_D (&(ptr->N), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->S), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->E), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->W), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_P (&(ptr->area), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_P (&(ptr->n_lines), 1, fp))
    return (-1);

  if (ptr->n_lines)
    if (0 >= dig__fwrite_port_P (ptr->lines, ptr->n_lines, fp))
      return -1;

  return (0);
}


int 
dig_Rd_Plus_head (   FILE * fp,
		     struct Plus_head *ptr)
{
  unsigned char buf[6];
  int byte_order;

  rewind (fp);
  if (0 >= dig__fread_port_C (buf, 6, fp))
    return (-1);

  ptr->Version_Major = buf[0];
  ptr->Version_Minor = buf[1];
  ptr->Back_Major    = buf[2];
  ptr->Back_Minor    = buf[3];
  byte_order         = buf[4];
  ptr->with_z        = buf[5];
  
  /* check version numbers */
  /*
  if (ptr->Version_Major != GRASS_V_VERSION_MAJOR ||
      (ptr->Version_Major == GRASS_V_VERSION_MAJOR && ptr->Version_Minor > GRASS_V_VERSION_MAJOR + 5))
    {
      if (GRASS_V_VERSION_MAJOR < ptr->Back_Major ||
      (GRASS_V_VERSION_MAJOR == ptr->Back_Major && GRASS_V_VERSION_MINOR < ptr->Back_Minor))
	{
	  fprintf (stderr, "Vector format version (%d.%d) is not known by this release.  EXITING\n",
		   ptr->Version_Major, ptr->Version_Minor);
	  fprintf (stderr, "Try running %s to reformat the dig_plus file\n", SUPPORT_PROG);
	  exit (-1);
	}
    }
  */
  dig_init_portable ( &(ptr->port), byte_order); 
  dig_set_cur_port ( &(ptr->port) );
  
  if (0 >= dig__fread_port_P (&(ptr->n_nodes), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_P (&(ptr->n_lines), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_P (&(ptr->n_areas), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_P (&(ptr->n_isles), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_P (&(ptr->n_plines), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_P (&(ptr->n_llines), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_P (&(ptr->n_blines), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_P (&(ptr->n_clines), 1, fp))
    return (-1);

  if (0 >= dig__fread_port_L (&(ptr->Node_offset), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->Line_offset), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->Area_offset), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->Isle_offset), 1, fp))
    return (-1);

  if (0 >= dig__fread_port_L (&(ptr->Dig_size), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->Dig_code), 1, fp))
    return (-1);

  /*
  if (0 >= dig__fread_port_I (&(ptr->all_areas), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_I (&(ptr->all_isles), 1, fp))
    return (-1);

  if (0 >= dig__fread_port_D (&(ptr->snap_thresh), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_D (&(ptr->prune_thresh), 1, fp))
    return (-1);

  if (0 >= dig__fread_port_L (&(ptr->future3), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->future4), 1, fp))
    return (-1);

  if (0 >= dig__fread_port_D (&(ptr->F1), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_D (&(ptr->F2), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_D (&(ptr->F3), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_D (&(ptr->F4), 1, fp))
    return (-1);

  if (0 >= dig__fread_port_C (ptr->Dig_name, HEADSTR, fp))
    return (-1);
  if (0 >= dig__fread_port_C (ptr->filler, HEADSTR, fp))
    return (-1);
  */

  return (0);
}

int 
dig_Wr_Plus_head ( FILE * fp,
		     struct Plus_head *ptr)
{
  unsigned char buf[6];
    
  rewind (fp);

  memset ( buf, 0, 6 );
  buf[0] = GRASS_V_VERSION_MAJOR;
  buf[1] = GRASS_V_VERSION_MINOR;
  buf[2] = GRASS_V_EARLIEST_MAJOR;
  buf[3] = GRASS_V_EARLIEST_MINOR;
  buf[4] = ptr->port.byte_order;
  //buf[5] = ???.with_z;
  if (0 >= dig__fwrite_port_C (buf, 6, fp))
    return (-1);
  
  if (0 >= dig__fwrite_port_P (&(ptr->n_nodes), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_lines), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_areas), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_isles), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_plines), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_llines), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_blines), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_P (&(ptr->n_clines), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_L (&(ptr->Node_offset), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->Line_offset), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->Area_offset), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->Isle_offset), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_L (&(ptr->Dig_size), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->Dig_code), 1, fp))
    return (-1);

  //if (0 >= dig__fwrite_port_I (&(ptr->all_areas), 1, fp))
  //  return (-1);

  /*if (0 >= dig__fwrite_port_I (&(ptr->all_areas   ), 1, fp)) return(-1);3.1? */
  /*
  if (0 >= dig__fwrite_port_I (&(ptr->all_isles), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_D (&(ptr->snap_thresh), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->prune_thresh), 1, fp))
    return (-1);
    
  if (0 >= dig__fwrite_port_L (&(ptr->future3), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->future4), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_D (&(ptr->F1), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->F2), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->F3), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_D (&(ptr->F4), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_C (ptr->Dig_name, HEADSTR, fp))
    return (-1);
  if (0 >= dig__fwrite_port_C (ptr->filler, HEADSTR, fp))
    return (-1);
  */

  return (0);
}

