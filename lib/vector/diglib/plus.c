#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include "gis.h"
#include "Vect.h"

/* Init head structure */
int 
dig_init_plus (struct Plus_head *Plus)
{
    
    Plus->Version_Major = 0 ;
    Plus->Version_Minor = 0 ;
    Plus->Back_Major = 0 ;
    Plus->Back_Minor = 0 ;
    
    Plus->Node_2d = NULL ;
    Plus->Line_2d = NULL ;
    Plus->Area_2d = NULL ;
    Plus->Isle_2d = NULL ;
    
    Plus->n_nodes = 0 ;
    Plus->n_lines = 0 ;
    Plus->n_areas = 0 ;
    Plus->n_isles = 0 ;
    Plus->alloc_nodes = 0 ;
    Plus->alloc_lines = 0 ;
    Plus->alloc_areas = 0 ;
    Plus->alloc_isles = 0 ;

    Plus->n_plines = 0 ;
    Plus->n_llines = 0 ;
    Plus->n_blines = 0 ;
    Plus->n_clines = 0 ;

    Plus->Node_offset = 0L ;
    Plus->Line_offset = 0L ;
    Plus->Area_offset = 0L ;
    Plus->Isle_offset = 0L ;

    /*
    Plus->future3 = 0L ;
    Plus->future4 = 0L ;
    Plus->F1 = 0.0 ;
    Plus->F2 = 0.0 ;
    Plus->F3 = 0.0 ;
    Plus->F4 = 0.0 ;
    Plus->filler[0] = '\0' ;
    */
    return 1;
}

/* Free head structure. 
*  Structure is not inited and dig_init_plus() should follow.
*/
int 
dig_free_plus (struct Plus_head *Plus)
{
    int i;    
    P_NODE_2D *Node;
    P_LINE_2D *Line;
    P_AREA_2D *Area;
    P_ISLE_2D *Isle;

    /* Nodes */
    for (i = 1; i <= Plus->n_nodes; i++) {
        Node = Plus->Node_2d[i]; 	
	if ( Node == NULL ) continue;
	
	if ( Node->alloc_lines > 0 ) {
	    free ( Node->lines);
	    free ( Node->angles);
	}
        free (Node);
    }
    free ( Plus->Node_2d );
    
    /* Lines */
    for (i = 1; i <= Plus->n_lines; i++) {
        Line = Plus->Line_2d[i]; 	
	if ( Line == NULL ) continue;
	
        free (Line);
    }
    free ( Plus->Line_2d );

    /* Areas */
    for (i = 1; i <= Plus->n_areas; i++) {
        Area = Plus->Area_2d[i]; 	
	if ( Area == NULL ) continue;
	
	if ( Area->alloc_lines > 0 ) 
	    free ( Area->lines);

	if ( Area->alloc_isles > 0 ) 
	    free ( Area->isles);
	
	if ( Area->alloc_centroids > 0 ) 
	    free ( Area->centroids);
	
        free (Area);
    }
    free ( Plus->Area_2d );
    
    /* Isles */
    for (i = 1; i <= Plus->n_isles; i++) {
        Isle = Plus->Isle_2d[i]; 	
	if ( Isle == NULL ) continue;
	
	if ( Isle->alloc_lines > 0 ) 
	    free ( Isle->lines);

        free (Isle);
    }
    free ( Plus->Isle_2d );
    
    return 1;
}

/* dig_load_plus reads topo file to topo structure
*  
*  Returns : 1 success
*            0 error
*/
int 
dig_load_plus (	struct Plus_head *Plus,
		FILE * plus)
{
  int i;


  G_debug (1, "dig_load_plus()"); 
  /* TODO
  if (do_checks)
    dig_do_file_checks (map, map->plus_file, map->digit_file);
  */
 
  /* free and init old */ 
  dig_free_plus ( Plus );
  dig_init_plus ( Plus );
  
  /* Now let's begin reading the Plus file nodes, lines, areas and isles */

  
  dig_Rd_Plus_head (plus, Plus);
  dig_set_cur_port ( &(Plus->port) ); /* in fact current port is set 
					 in dig_Rd_Plus_head() already */
  //dig_head_to_map (&P_head, map);

  /* Nodes */
  fseek (plus, Plus->Node_offset, 0);
  dig_alloc_nodes_2d ( Plus, Plus->n_nodes );
  for (i = 1; i <= Plus->n_nodes; i++)
    {
      if ( dig_Rd_P_node ( Plus, i, plus) == -1 )
	  G_fatal_error ("Cannot read topo for node %d", i);
    }
  
  /* Lines */
  fseek (plus, Plus->Line_offset, 0);
  dig_alloc_lines_2d ( Plus, Plus->n_lines );
  for (i = 1; i <= Plus->n_lines; i++)
    {
      if ( dig_Rd_P_line ( Plus, i, plus) == -1 )
	  G_fatal_error ("Cannot read topo for line %d", i);
    }
  
  /* Areas */
  fseek (plus, Plus->Area_offset, 0);
  dig_alloc_areas_2d ( Plus, Plus->n_areas );
  for (i = 1; i <= Plus->n_areas; i++)
    {
      if ( dig_Rd_P_area ( Plus, i, plus) == -1 ) 
	  G_fatal_error ("Cannot read topo for area %d", i);
    }
  
  /* Isles */
  fseek (plus, Plus->Isle_offset, 0);
  dig_alloc_isles_2d ( Plus, Plus->n_isles );
  for (i = 1; i <= Plus->n_isles; i++)
    {
      if ( dig_Rd_P_isle ( Plus, i, plus) == -1 )
	  G_fatal_error ("Cannot read topo for isle %d", i);
    }

  return (1);
}

int 
dig_write_plus_file (
		      FILE * fp_plus,
		      struct Plus_head *Plus)
{

  dig_set_cur_port(&(Plus->port));  
  rewind (fp_plus);

  if (dig_Wr_Plus_head (fp_plus, Plus) < 0)
    {
      fprintf (stderr, "\nERROR: Can't write head to plus file.\n");
      return (-1);
    }

  if (dig_write_nodes (fp_plus, Plus) < 0)
    {
      fprintf (stderr, "\nERROR: Can't write nodes to plus file.\n");
      return (-1);
    }

  if (dig_write_lines (fp_plus, Plus) < 0)
    {
      fprintf (stderr, "\nERROR: Can't write lines to plus file.\n");
      return (-1);
    }

  if (dig_write_areas (fp_plus, Plus) < 0)
    {
      fprintf (stderr, "\nERROR: Can't write areas to plus file.\n");
      return (-1);
    }

  if (dig_write_isles (fp_plus, Plus) < 0)
    {
      fprintf (stderr, "\nERROR: Can't write isles to plus file.\n");
      return (-1);
    }


  rewind (fp_plus);
  if (dig_Wr_Plus_head (fp_plus, Plus) < 0)
    {
      fprintf (stderr, "\nERROR: Can't write head to plus file.\n");
      return (-1);
    }

  rewind (fp_plus);
  if (dig_write_file_checks (fp_plus, Plus))
    {
      fprintf (stderr, "\nERROR: writing file_checks.\n");
      return (-1);
    }

  fflush (fp_plus);
  return (0);
}				/*  write_plus_file()  */


int 
dig_write_nodes (
		  FILE * plus,
		  struct Plus_head *Plus)
{
  int i;


  Plus->Node_offset = ftell (plus);

  for (i = 1; i <= Plus->n_nodes; i++)
    {
      if (dig_Wr_P_node (Plus, i, plus) < 0)
	return (-1);
    }

  return (0);
}				/*  write_nodes()  */


int 
dig_write_lines (
		  FILE * plus,
		  struct Plus_head *Plus)
{
  int i;


  Plus->Line_offset = ftell (plus);

  for (i = 1; i <= Plus->n_lines; i++)
    {
      if (dig_Wr_P_line (Plus, i, plus) < 0)
	return (-1);
    }

  return (0);

}				/*  write_line()  */

int 
dig_write_areas (
		  FILE * plus,
		  struct Plus_head *Plus)
{
  int i;


  Plus->Area_offset = ftell (plus);

  for (i = 1; i <= Plus->n_areas; i++)
    {
      if (dig_Wr_P_area (Plus, i, plus) < 0)
	return (-1);
    }

  return (0);

}				/*  write_areas()  */


int 
dig_write_isles (
		  FILE * plus,
		  struct Plus_head *Plus)
{
  int i;


  Plus->Isle_offset = ftell (plus);

  for (i = 1; i <= Plus->n_isles; i++)
    {
      if (dig_Wr_P_isle (Plus, i, plus) < 0)
	return (-1);
    }

  return (0);

}				/*  write_isles()  */
