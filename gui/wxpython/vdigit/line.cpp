/**
   \file line.cpp

   \brief Line manipulation

   This program is free software under the GNU General Public
   License (>=v2). Read the file COPYING that comes with GRASS
   for details.

   (C) 2008 by The GRASS development team

   \author Martin Landa <landa.martin gmail.com>

   \date 2008 
*/

extern "C" {
#include <grass/vedit.h>
}
#include "driver.h"
#include "digit.h"

/**
   \brief Add new vector feature

   \param type   feature type
   \param coords pairs of coordinates list (2D or 3D map)
   \param layer  layer number
   \param cat    category number
   \param bgmap  map of background map or NULL
   \param snap   snapping mode (see vedit.h)
   \param thresh threshold value for snapping

   \return 1 on success
   \return -1 on failure
*/
int Digit::AddLine(int type, std::vector<double> coords, int layer, int cat,
		   const char *bgmap, int snap, double threshold)
{
    int i, npoints;
    int newline;

    struct line_pnts *Points;
    struct line_cats *Cats;

    struct Map_info **BgMap; /* backgroud vector maps */
    int nbgmaps;             /* number of registrated background maps */

    if (!display->mapInfo) {
	return -1;
    }

    npoints = coords.size() / (Vect_is_3d(display->mapInfo) ? 3 : 2);
    if (coords.size() != npoints * (Vect_is_3d(display->mapInfo) ? 3 : 2)) {
	return -1;
    }

    G_debug(2, "wxDigit.AddLine(): npoints=%d, layer=%d, cat=%d, snap=%d",
	    npoints, layer, cat, snap);

    /* TODO: 3D */
    if (!(type & GV_POINTS) && !(type & GV_LINES)) {
	return -1;
    }

    if (layer < -1) {
	return -1;
    }

    BgMap = NULL;

    if (bgmap) {
	const char *mapset;
	mapset = G_find_vector2 (bgmap, ""); 
	if (mapset) {
	    if (strcmp(G_fully_qualified_name((const char*) display->mapInfo->name, (const char*) G_mapset()),
		       G_fully_qualified_name((const char*) bgmap, (const char*) mapset))) {
		nbgmaps = 1;
		BgMap = (struct Map_info**) G_realloc ((void *) BgMap, nbgmaps * sizeof(struct Map_info*));
		BgMap[nbgmaps-1] = (struct Map_info *) G_malloc (sizeof(struct Map_info));
		
		Vect_open_old(BgMap[nbgmaps-1], (char *) bgmap, (char*) mapset);
	    }
	}
    }

    Points = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();

    Vect_cat_set(Cats, layer, cat);

    if (cat > GetCategory(layer)) {
	SetCategory(layer, cat); /* set up max category for layer */
    }

    i = 0;
    while (i < coords.size()) {
	if (Vect_is_3d(display->mapInfo)) {
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
				 Vect_is_3d(display->mapInfo)) <= threshold) {
	    Points->x[last] = Points->x[0];
	    Points->y[last] = Points->y[0];
	    Points->z[last] = Points->z[0];
	    G_debug(3, "wxDigit.AddLine(): boundary closed");
	}
    }

    newline = Vect_write_line(display->mapInfo, type, Points, Cats);

    if (snap != NO_SNAP) { /* apply snapping */
      /*
	Vedit_snap_line(display->mapInfo, BgMap, nbgmaps,
			newline,
			threshold, (SNAP) ? 0 : 1); 
      */
    }

    Vect_destroy_line_struct(Points);
    Vect_destroy_cats_struct(Cats);

    if (BgMap && BgMap[0]) {
	Vect_close(BgMap[0]);
    }

    G_debug(2, "wxDigit.AddLine(): line=%d written", newline);

    return 1;
}

/**
   \brief Rewrite given line

   \param line line id
   \param coords line geometry

   \return new line id
   \return -1 error
*/
int Digit::RewriteLine(int line, std::vector<double> coords)
{
    int ret, type, dim;
    double x, y, z;
    struct line_pnts *points;
    struct line_cats *cats;

    if (!display->mapInfo) {
	return -1;
    }

    /* line alive ? */
    if (!Vect_line_alive(display->mapInfo, line)) {
	return -1;
    }
    
    ret = 0;
    points = Vect_new_line_struct();
    cats = Vect_new_cats_struct();

    /* read line */
    type = Vect_read_line(display->mapInfo, NULL, cats, line);
    if (type < 0) {
	ret = -1;
    }

    /* define line geometry */
    if (Vect_is_3d(display->mapInfo)) {
	dim = 3;
    }
    else {
	dim = 2;
    }
    for(size_t i = dim - 1; i < coords.size(); i += dim) {
	if (dim == 2) {
	    Vect_append_point(points, coords[i-1], coords[i], 0.0);
	}
	else {
	    Vect_append_point(points, coords[i-2], coords[i-1], coords[i]);
	}
    }

    /* rewrite line */
    if (ret == 0) {
	if (Vect_rewrite_line(display->mapInfo, line, type, points, cats) < 0) {
	    ret = -1;
	}
    }

    Vect_destroy_line_struct(points);
    Vect_destroy_cats_struct(cats);

    return ret;
}

/**
   \brief Split/break selected line

   Shape of line is not changed.

   \param x,y,z coordinates (z is used only if map is 3d)
   \param thresh threshold value to find a point on line
*/
int Digit::SplitLine(double x, double y, double z,
		     double thresh)
{
    int ret;
    struct line_pnts *point;

    if (!display->mapInfo)
	return -1;

    point = Vect_new_line_struct();
    Vect_append_point(point, x, y, z);

    ret = Vedit_split_lines(display->mapInfo, display->selected,
			    point, thresh, NULL);

    Vect_destroy_line_struct(point);

    return ret;
}

/**
   \brief Delete selected vector features

   \return number of deleted lines
   \return -1 on error
*/
int Digit::DeleteLines()
{
    int ret;

    if (!display->mapInfo) {
	return -1;
    }

    ret = Vedit_delete_lines(display->mapInfo, display->selected);

    return ret;
}


/** 
    \brief Move selected vector features

    \param move_x,move_y,move_z move direction (move_z is used only if map is 3D)
    \param snap snapping move (see vedit.h)
    \param thresh threshold value for snapping

    \return number of moved features
    \return -1 on error
*/
int Digit::MoveLines(double move_x, double move_y, double move_z,
		     int snap, double thresh)
{
    int ret;

    if (!display->mapInfo)
	return -1;

    ret = Vedit_move_lines(display->mapInfo, display->selected,
			   move_x, move_y, move_z,
			   snap, thresh);

    return ret;
}

/**
   \brief Flip selected lines/boundaries

   \return number of modified lines
   \return -1 on error
*/
int Digit::FlipLines()
{
    int ret;

    if (!display->mapInfo) {
	return -1;
    }

    /*
      ret = Vedit_flip_lines(display->mapInfo, display->selected);
    */

    return ret;
}

/**
   \brief Merge selected lines/boundaries

   \return number of modified lines
   \return -1 on error
*/
int Digit::MergeLines()
{
    int ret;

    if (!display->mapInfo) {
	return -1;
    }

    /*
      ret = Vedit_merge_lines(display->mapInfo, display->selected);
    */

    return ret;
}

/**
   \brief Breaks selected lines/boundaries

   \return number of modified lines
   \return -1 on error
*/
int Digit::BreakLines()
{
    int ret;

    if (!display->mapInfo) {
	return -1;
    }

    /*
    ret = Vect_break_lines_list(display->mapInfo, display->selected,
				GV_LINES, NULL, NULL);
    */

    return ret;
}

/**
   \brief Snap selected lines/boundaries

   \param thresh threshold value for snapping

   \return number of modified lines
   \return -1 on error
*/
int Digit::SnapLines(double thresh)
{
    int ret;

    if (!display->mapInfo) {
	return -1;
    }

    /*
    Vect_snap_lines_list (display->mapInfo, display->selected,
    thresh, NULL, NULL);
    */
    return ret;
}

/**
   \brief Connect two selected lines/boundaries

   \return 1 lines connected
   \return 0 lines not connected
   \return -1 on error
*/
int Digit::ConnectLines(double thresh)
{
    int ret;

    if (!display->mapInfo) {
	return -1;
    }

    /*
    ret = Vedit_connect_lines(&Map, List,
			      thresh);
    */
    return ret;
}

/**
   \brief Automated labeling (z coordinate assignment) of vector lines (contours).

   Modified vector map must be 3D.

   \param x1,y1,x2,y2 line nodes for intersection
   \param start starting value
   \param step  step value for labeling

   \return number of modified lines
   \return -1 on error
*/
int Digit::ZBulkLabeling(double x1, double y1, double x2, double y2,
			 double start, double step)
{
    int ret;

    if (!display->mapInfo) {
	return -1;
    }

    /*
    ret = Vedit_bulk_labeling (display->mapInfo, display->selected,
			       x1, y1, x2, y2, start, step);
    */

    return ret;
}

/**
   \brief Copy vector features

   \param ids line ids to be copied (if not given selected are used)
   \param bgmap name of background map (if not given, copy features from input)

   \return number of copied features
   \return -1 on error
*/
int Digit::CopyLines(std::vector<int> ids, const char* bgmap_name)
{
    int ret;
    struct Map_info *bgMap;
    struct ilist *list;

    bgMap = NULL;
    list = NULL;

    if (!display->mapInfo) {
	return -1;
    }


    if (bgmap_name) {
	const char *mapset;
	bgMap = (struct Map_info *) G_malloc(sizeof (struct Map_info));
	mapset = G_find_vector2 (bgmap_name, ""); 
	Vect_open_old(bgMap, (char *) bgmap_name, (char *) mapset); /* TODO */
    }

/*
    if (!ids.empty) {
	list = Vect_new_list_struct();
    }
    else {
	list = display->selected;
    }

    int Vedit_copy_lines (display->mapInfo, bgMap,
			  list);

    if (list != display->selected) {
	Vect_destroy_list_struct(list);
    }
*/

    if (bgMap) {
	Vect_close(bgMap);
	G_free ((void *) bgMap);
    }

    return ret;
    
}
