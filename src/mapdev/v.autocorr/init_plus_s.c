#include	<stdio.h>
#include	"gis.h"
#include	"Vect.h"


int 
init_plus_struct (struct Plus_head *Plus)
{


  Plus->n_nodes = 0;
  Plus->n_lines = 0;
  Plus->n_areas = 0;
  Plus->n_isles = 0;
  Plus->n_atts = 0;

  Plus->n_llines = 0;
  Plus->n_alines = 0;
  Plus->n_points = 0;

  Plus->Node_offset = 0L;
  Plus->Line_offset = 0L;
  Plus->Area_offset = 0L;
  Plus->Att_offset = 0L;
  Plus->Isle_offset = 0L;

  Plus->Back_Major = 0;
  Plus->Back_Minor = 0;
  Plus->future3 = 0L;
  Plus->future4 = 0L;
  Plus->F1 = 0.0;
  Plus->F2 = 0.0;
  Plus->F3 = 0.0;
  Plus->F4 = 0.0;
  Plus->filler[0] = '\0';

  return (0);
}


int 
init_map_struct (struct Map_info *Map)
{

  Map->n_nodes = 0;
  Map->n_lines = 0;
  Map->n_areas = 0;
  Map->n_isles = 0;
  Map->n_atts = 0;

  Map->n_llines = 0;
  Map->n_alines = 0;
  Map->n_plines = 0;
  Map->n_points = 0;

  Map->alloc_nodes = 0;
  Map->alloc_lines = 0;
  Map->alloc_areas = 0;
  Map->alloc_isles = 0;
  Map->alloc_atts = 0;

  Map->snap_thresh = 0.0;
  Map->prune_thresh = 0.0;

  return (0);
}
