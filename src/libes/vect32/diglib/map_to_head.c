#include <string.h>
#include "Vect.h"

int dig_map_to_head (
    struct Map_info *map,
    struct Plus_head *phead)
{
    phead->Major = VERSION_MAJOR;
    phead->Minor = VERSION_MINOR;
    phead->n_lines = map->n_lines;
    phead->n_llines = map->n_llines;
    phead->n_alines = map->n_alines;
    phead->n_plines = map->n_plines;
    phead->n_points = map->n_points;
    phead->n_nodes = map->n_nodes;
    phead->n_areas = map->n_areas;
    phead->n_atts = map->n_atts;
    phead->n_isles = map->n_isles;

    phead->snap_thresh = map->snap_thresh;
    phead->prune_thresh = map->prune_thresh;

    phead->Back_Major = 0;
    phead->Back_Minor = 0;
    phead->future3 = 0;
    phead->future4 = 0;
    phead->F1 = 00;
    phead->F2 = 0.0;
    phead->F3 = 0.0;
    phead->F4 = 0.0;

    strcpy (phead->Dig_name, map->head.map_name);
    phead->filler[0] = 0;

    return 0;
}

int dig_head_to_map (
    struct Plus_head *head,
    struct Map_info *map)
{
    map->n_lines  = head->n_lines;
    map->n_areas  = head->n_areas;
    map->n_atts   = head->n_atts;
    map->n_isles  = head->n_isles;
    map->n_nodes  = head->n_nodes;
    map->n_alines = head->n_alines;
    map->n_llines = head->n_llines;
    map->n_plines = head->n_plines;
    map->n_points = head->n_points;

    map->snap_thresh = head->snap_thresh;
    map->prune_thresh = head->prune_thresh;

    map->all_areas = head->all_areas;
    map->all_isles = head->all_isles;

    return 0;
}
