/*
* $Id$
*
****************************************************************************
*
* MODULE:       Vector library 
*   	    	
* AUTHOR(S):    Radim Blazek.
*
* PURPOSE:      Lower level functions for reading/writing/manipulating vectors.
*
* COPYRIGHT:    (C) 2001 by the GRASS Development Team
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


int 
dig_Wr_spindx_head ( FILE * fp,
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
  buf[5] = ptr->with_z; 
  if (0 >= dig__fwrite_port_C (buf, 6, fp))
    return (-1);
 
  if (0 >= dig__fwrite_port_L (&(ptr->Node_spidx_offset), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->Line_spidx_offset), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->Area_spidx_offset), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->Isle_spidx_offset), 1, fp))
    return (-1);

  if (0 >= dig__fwrite_port_L (&(ptr->coor_size), 1, fp))
    return (-1);
  if (0 >= dig__fwrite_port_L (&(ptr->coor_mtime), 1, fp))
    return (-1);

  return (0);
}


int 
dig_Rd_spindx_head (   FILE * fp,
		     struct Plus_head *ptr)
{
  unsigned char buf[6];
  int byte_order;

  rewind (fp);
  if (0 >= dig__fread_port_C (buf, 6, fp))
    return (-1);

  /* TODO: separate header info for sidx from topo better and do more checks */
  /*
  ptr->Version_Major = buf[0];
  ptr->Version_Minor = buf[1];
  ptr->Back_Major    = buf[2];
  ptr->Back_Minor    = buf[3];
  */
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

  if (0 >= dig__fread_port_L (&(ptr->Node_spidx_offset), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->Line_spidx_offset), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->Area_spidx_offset), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->Isle_spidx_offset), 1, fp))
    return (-1);

  /*
  if (0 >= dig__fread_port_L (&(ptr->coor_size), 1, fp))
    return (-1);
  if (0 >= dig__fread_port_L (&(ptr->coor_mtime), 1, fp))
    return (-1);
  */

  return (0);
}

int rtree_dump_node( FILE *fp, struct Node *n, int with_z);

/* Dump RTree branch to file */
int rtree_dump_branch( FILE *fp, struct Branch *b, int with_z, int level)
{
    struct Rect *r;

    r = &(b->rect);
    
    if ( level == 0 ) 
	fprintf ( fp, "  id = %d ", (int)b->child); 
	
    fprintf ( fp, " %f %f %f %f %f %f\n",  r->boundary[0], r->boundary[1], r->boundary[2],
                                  r->boundary[3], r->boundary[4], r->boundary[5]);

    if ( level > 0 ) {
        rtree_dump_node( fp, b->child, with_z);
    }
    return 0;
}

/* Dump RTree node to file */
int rtree_dump_node( FILE *fp, struct Node *n, int with_z)
{
    int i, nn;

    fprintf (fp, "Node level=%d  count=%d\n", n->level, n->count );
    
    if ( n->level > 0 ) nn = NODECARD; else  nn = LEAFCARD;

    for (i = 0; i < nn; i++) {
	if ( n->branch[i].child ) {
            fprintf (fp, "  Branch %d", i );
	    rtree_dump_branch( fp, &n->branch[i], with_z, n->level );
	}
    }
    
    return 0;
}

int rtree_write_node( FILE *fp, struct Node *n, int with_z);

/* Write RTree branch to file */
int rtree_write_branch( FILE *fp, struct Branch *b, int with_z, int level)
{
    struct Rect *r;
    int i;

    r = &(b->rect);
    
    /* rectangle */
    if ( with_z ) {
        if (0 >= dig__fwrite_port_D (&(r->boundary[0]), 6, fp)) return (-1);
    } else {
        if (0 >= dig__fwrite_port_D (&(r->boundary[0]), 2, fp)) return (-1);
        if (0 >= dig__fwrite_port_D (&(r->boundary[3]), 2, fp)) return (-1);
    }
    if ( level == 0 ) { /* write data (element id) */
	i = (int) b->child;
        if (0 >= dig__fwrite_port_I ( &i, 1, fp)) return (-1);
    } else {
        rtree_write_node( fp, b->child, with_z);
    }
    return 0;
}

/* Write RTree node to file */
int rtree_write_node( FILE *fp, struct Node *n, int with_z)
{
    int i, nn;

    /* level ( 0 = leaf, data ) */
    if (0 >= dig__fwrite_port_I ( &(n->level), 1, fp)) return (-1);

    /* count */
    if (0 >= dig__fwrite_port_I ( &(n->count), 1, fp)) return (-1);
    
    if ( n->level > 0 ) nn = NODECARD; else  nn = LEAFCARD;
    for ( i = 0; i < nn; i++) {
	if ( n->branch[i].child ) {
	    rtree_write_branch( fp, &n->branch[i], with_z, n->level );
	}
    }
    
    return 0;
}

int rtree_read_node( FILE *fp, struct Node *n, int with_z);

/* Read RTree branch from file */
int rtree_read_branch( FILE *fp, struct Branch *b, int with_z, int level)
{
    struct Rect *r;
    int i;
    
    G_debug (3, "rtree_read_branch()");

    r = &(b->rect);
    
    /* rectangle */
    if ( with_z ) {
        if (0 >= dig__fread_port_D (&(r->boundary[0]), 6, fp)) return (-1);
    } else {
        if (0 >= dig__fread_port_D (&(r->boundary[0]), 2, fp)) return (-1);
        if (0 >= dig__fread_port_D (&(r->boundary[3]), 2, fp)) return (-1);
	r->boundary[2] = 0;
	r->boundary[5] = 0;
    }
    
    if ( level == 0 ) { /* read data (element id) */
        if (0 >= dig__fread_port_I ( &i, 1, fp)) return (-1);
	b->child = (struct Node *) i;
    } else {
	/* create new node */
	b->child = RTreeNewNode();
        rtree_read_node( fp, b->child, with_z);
    }
    return 0;
}

/* Read RTree node to file */
int rtree_read_node( FILE *fp, struct Node *n, int with_z)
{
    int level, count, i;

    G_debug (3, "rtree_read_node()");
    
    /* level ( 0 = leaf, data ) */
    if (0 >= dig__fread_port_I ( &level, 1, fp)) return (-1);
    n->level = level;

    /* count */
    if (0 >= dig__fread_port_I ( &count, 1, fp)) return (-1);
    n->count = count;
    
    for (i=0; i<count; i++) {
	if ( 0 > rtree_read_branch( fp, &n->branch[i], with_z, level ) ) return (-1);
    }
    
    return 0;
}

/* Write spatial index */
int
dig_write_spidx ( FILE * fp, struct Plus_head *Plus)
{
    dig_set_cur_port(&(Plus->port));
    rewind (fp);

    dig_Wr_spindx_head ( fp, Plus );
     
    Plus->Node_spidx_offset = ftell ( fp );
    rtree_write_node( fp, Plus->Node_spidx, Plus->with_z);
	 
    Plus->Line_spidx_offset = ftell ( fp );
    rtree_write_node( fp, Plus->Line_spidx, Plus->with_z);

    Plus->Area_spidx_offset = ftell ( fp );
    rtree_write_node( fp, Plus->Area_spidx, Plus->with_z);

    Plus->Isle_spidx_offset = ftell ( fp );
    rtree_write_node( fp, Plus->Isle_spidx, Plus->with_z);

    rewind (fp);
    dig_Wr_spindx_head ( fp, Plus ); /* rewrite with offsets */

    return 0; 
}

/* Read spatial index file */
int
dig_read_spidx ( FILE * fp, struct Plus_head *Plus)
{
    G_debug (1, "dig_read_spindx()");

    /* TODO: free old tree */
    dig_spidx_init ( Plus);

    rewind (fp);
    dig_Rd_spindx_head ( fp, Plus);
    dig_set_cur_port(&(Plus->port));
    
    fseek ( fp, Plus->Node_spidx_offset, 0);
    rtree_read_node( fp, Plus->Node_spidx, Plus->with_z);
	 
    fseek ( fp, Plus->Line_spidx_offset, 0);
    rtree_read_node( fp, Plus->Line_spidx, Plus->with_z);
	 
    fseek ( fp, Plus->Area_spidx_offset, 0);
    rtree_read_node( fp, Plus->Area_spidx, Plus->with_z);
	 
    fseek ( fp, Plus->Isle_spidx_offset, 0);
    rtree_read_node( fp, Plus->Isle_spidx, Plus->with_z);
	 
    return 0; 
}

/* Dump spatial index */
int
dig_dump_spidx ( FILE * fp, struct Plus_head *Plus)
{
     
    fprintf ( fp, "Nodes\n"); 
    rtree_dump_node( fp, Plus->Node_spidx, Plus->with_z);
	 
    fprintf ( fp, "Lines\n"); 
    rtree_dump_node( fp, Plus->Line_spidx, Plus->with_z);
	 
    fprintf ( fp, "Areas\n"); 
    rtree_dump_node( fp, Plus->Area_spidx, Plus->with_z);
	 
    fprintf ( fp, "Isles\n"); 
    rtree_dump_node( fp, Plus->Isle_spidx, Plus->with_z);
	 
    return 0; 
}

