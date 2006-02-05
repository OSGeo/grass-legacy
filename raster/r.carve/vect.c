#include <stdio.h>
#include "gis.h"
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
int close_vect(struct Map_info *map, int build_support)
{
    if (build_support)
        Vect_build(map, stderr);

    Vect_set_release_support(map);
    Vect_close(map);

    return 1;
}

 
/*
 * write_xyz_points - Writes a sites file from two Point2 lists, using x 
 * and y from pgxypts and y from pgpts as the third (z) dimension.
 */
int write_xyz_points(struct Map_info *map, Point2 *pgxypts, Point2 *pgpts,
                    const int npts, const double depth)
{
    static struct line_pnts *points = NULL;
    static struct line_cats *cats   = NULL;
    int i;

    if (points == NULL)
        points = Vect_new_line_struct();

    if (cats == NULL)
    {
        cats = Vect_new_cats_struct();
        Vect_cat_set(cats, 1, 1);
    }

    Vect_reset_line(points);

    for (i = 0; i < npts; i++) {
        G_debug(3, "x:%.2lf y:%.2lf z:%.2lf", pgxypts[i][0],
                pgxypts[i][1], pgpts[i][1] - depth);
        Vect_append_point(points, pgxypts[i][0], pgxypts[i][1], 
                          pgpts[i][1] - depth);
    }

    Vect_write_line(map, GV_POINT, points, cats);

    return 1;
}
