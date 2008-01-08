/**
   \file line.cpp

   \brief Line manipulation

   This program is free software under the GNU General Public
   License (>=v2). Read the file COPYING that comes with GRASS
   for details.

   \author (C) by The GRASS development team
   Martin Landa <landa.martin gmail.com>

   \date 2008 
*/

#include "driver.h"
#include "digit.h"

/**
   \brief Delete selected lines

   \return number of deleted lines
   \return -1 on error
*/
int Digit::DeleteSelectedLines()
{
    int nlines = 0;

    if (!display->mapInfo) {
	return -1;
    }

    for(std::vector<int>::iterator i = display->selected.begin(), e = display->selected.end();
	i != e; ++i) {
	Vect_delete_line(display->mapInfo, *i);
    }

    return nlines;
}

/**
   \brief Add new line

   For feature type see include/vect/dig_defines.h
       #define GV_POINT      0x01
       #define GV_LINE	     0x02
       #define GV_BOUNDARY   0x04
       #define GV_CENTROID   0x08

       (unsupported)
       #define GV_FACE       0x10
1       #define GV_KERNEL     0x20
       #define GV_AREA	     0x40
       #define GV_VOLUME     0x80

   \param type   feature type
   \param coords pairs of coordinates list
   \param with_z 3D?

   \return 1 on success
   \return -1 on failure
*/
int Digit::AddLine(int type, std::vector<double> coords, bool with_z, int layer, int cat)
{
    int i, npoints;

    struct line_pnts *Points;
    struct line_cats *Cats;

    if (!display->mapInfo) {
	return -1;
    }

    npoints = coords.size() / (with_z ? 3 : 2);
    if (coords.size() != npoints * (with_z ? 3 : 2)) {
	return -1;
    }

    /* TODO: 3D */
    if (!(type & GV_POINTS) && !(type & GV_LINES)) {
	return -1;
    }

    if (layer < -1) {
	return -1;
    }

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    Vect_cat_set(Cats, layer, cat);

    if (cat > GetCategory(layer)) {
	SetCategory(layer, cat); /* set up max category for layer */
    }

    i = 0;
    while (i < coords.size()) {
	if (with_z) {
	    Vect_append_point(Points, coords[i], coords[i+1], coords[i+2]);
	    i += 3;
	}
	else {
	    Vect_append_point(Points, coords[i], coords[i+1], 0.0);
	    i += 2;
	}
    }

    if (type & GV_BOUNDARY) { /* close boundary */
	int last = Points->n_points-1;
	if (Vect_points_distance(Points->x[0], Points->x[0], Points->z[0],
				 Points->x[last], Points->x[last], Points->z[last],
				 with_z) <= display->settings.threshold) {
	    Points->x[last] = Points->x[0];
	    Points->y[last] = Points->y[0];
	    Points->z[last] = Points->z[0];
	}
    }

    Vect_write_line(display->mapInfo, type, Points, Cats);

    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);

    return 1;
}
