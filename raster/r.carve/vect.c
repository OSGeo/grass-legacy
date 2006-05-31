#include <stdio.h>
#include <grass/gis.h>
#include "enforce.h"


/*
 * open_new_vect - opens new vector file for writing
 */
int open_new_vect(struct Map_info *map, char *vect)
{
    Vect_open_new(map, vect, 1);
    Vect_set_map_name(map, vect);
    Vect_set_comment(map, G_recreate_command());
    Vect_hist_command(map);

    return 1;
}


/*
 * close_vect - builds vector support and frees up resources
 */
int close_vect(struct Map_info *map, const int build_support)
{
    if (build_support)
        Vect_build(map, stderr);

    Vect_set_release_support(map);
    Vect_close(map);

    return 1;
}
