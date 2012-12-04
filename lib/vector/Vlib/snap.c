/*!
 * \file snap.c
 *
 * \brief Vector library - Clean vector map (snap lines)
 *
 * Higher level functions for reading/writing/manipulating vectors.
 *
 * (C) 2001-2008 by the GRASS Development Team
 *
 * This program is free software under the 
 * GNU General Public License (>=v2). 
 * Read the file COPYING that comes with GRASS
 * for details.
 *
 * \author Radim Blazek
 *
 * \date 2001
 */

#include <math.h>
#include <stdlib.h>

#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

/* function prototypes */
static int sort_new(const void *pa, const void *pb);

/* Vertex */
typedef struct
{
    double x, y;
    int anchor;			/* 0 - anchor, do not snap this point, that means snap others to this */
    /* >0  - index of anchor to which snap this point */
    /* -1  - init value */
} XPNT;

/* Segment */
typedef struct
{
    double x1, y1,  /* start point */
           x2, y2;  /* end point */
} XSEG;

typedef struct
{
    int anchor;
    double along;
} NEW;

typedef struct
{
    double x, y, z, along;
} NEW2;

/* for qsort */
static int sort_new2(const void *pa, const void *pb)
{
    NEW2 *p1 = (NEW2 *) pa;
    NEW2 *p2 = (NEW2 *) pb;

    return (p1->along < p2->along ? -1 : (p1->along > p2->along));
}

/* This function is called by RTreeSearch() to add selected node/line/area/isle to the list */
static int add_item(int id, struct ilist *list)
{
    dig_list_add(list, id);
    return 1;
}

/* This function is called by RTreeSearch() to find an item in the list */
static int find_item(int id, struct ilist *list)
{
    dig_list_add(list, id);
    return 0;
}

/*!
 * \brief Snap selected lines to existing vertex in threshold.
 *
 * Snap selected lines to existing vertices.
 * 
 * \warning Lines are not necessarily snapped to nearest vertex, but to vertex in threshold! 
 *
 * Lines showing how vertices were snapped may be optionally written to error map. 
 * Input map must be opened on level 2 for update at least on GV_BUILD_BASE.
 *
 * \param[in] Map input map where vertices will be snapped
 * \param[in] List_lines list of lines to snap
 * \param[in] thresh threshold in which snap vertices
 * \param[out] Err vector map where lines representing snap are written or NULL
 *
 * \return void
 */

/* As mentioned above, lines are not necessarily snapped to nearest vertex! For example:
   |                    
   | 1         line 3 is snapped to line 1,
   |           then line 2 is not snapped to common node at lines 1 and 3,
   because it is already outside of threshold
   ----------- 3   

   |
   | 2
   |    
 */
void
Vect_snap_lines_list(struct Map_info *Map, struct ilist *List_lines,
		     double thresh, struct Map_info *Err)
{
    struct line_pnts *Points, *NPoints;
    struct line_cats *Cats;
    int line, ltype, line_idx;
    double thresh2;

    struct Node *RTree;
    int point;			/* index in points array */
    int nanchors, ntosnap;	/* number of anchors and number of points to be snapped */
    int nsnapped, ncreated;	/* number of snapped verices, number of new vertices (on segments) */
    int apoints, npoints, nvertices;	/* number of allocated points, registered points, vertices */
    XPNT *XPnts;		/* Array of points */
    NEW *New = NULL;		/* Array of new points */
    int anew = 0, nnew;		/* allocated new points , number of new points */
    struct Rect rect;
    struct ilist *List;
    int *Index = NULL;		/* indexes of anchors for vertices */
    int aindex = 0;		/* allocated Index */

    if (List_lines->n_values < 1)
	return;

    Points = Vect_new_line_struct();
    NPoints = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    List = Vect_new_list();
    RTree = RTreeNewIndex();

    thresh2 = thresh * thresh;

    /* Go through all lines in vector, and add each point to structure of points */
    apoints = 0;
    point = 1;			/* index starts from 1 ! */
    nvertices = 0;
    XPnts = NULL;

    G_verbose_message(_("Snap vertices Pass 1: select points"));
    for (line_idx = 0; line_idx < List_lines->n_values; line_idx++) {
	int v;

	G_percent(line_idx, List_lines->n_values, 2);

	line = List_lines->value[line_idx];

	G_debug(3, "line =  %d", line);
	if (!Vect_line_alive(Map, line))
	    continue;

	ltype = Vect_read_line(Map, Points, Cats, line);

	for (v = 0; v < Points->n_points; v++) {
	    G_debug(3, "  vertex v = %d", v);
	    nvertices++;

	    /* Box */
	    rect.boundary[0] = Points->x[v];
	    rect.boundary[3] = Points->x[v];
	    rect.boundary[1] = Points->y[v];
	    rect.boundary[4] = Points->y[v];
	    rect.boundary[2] = 0;
	    rect.boundary[5] = 0;

	    /* Already registered ? */
	    Vect_reset_list(List);
	    RTreeSearch(RTree, &rect, (void *)add_item, List);
	    G_debug(3, "List : nvalues =  %d", List->n_values);

	    if (List->n_values == 0) {	/* Not found */
		/* Add to tree and to structure */
		RTreeInsertRect(&rect, point, &RTree, 0);
		if ((point - 1) == apoints) {
		    apoints += 10000;
		    XPnts =
			(XPNT *) G_realloc(XPnts,
					   (apoints + 1) * sizeof(XPNT));
		}
		XPnts[point].x = Points->x[v];
		XPnts[point].y = Points->y[v];
		XPnts[point].anchor = -1;
		point++;
	    }
	}
    }
    G_percent(line_idx, List_lines->n_values, 2); /* finish it */

    npoints = point - 1;

    /* Go through all registered points and if not yet marked mark it as anchor and assign this anchor
     * to all not yet marked points in threshold */

    G_verbose_message(_("Snap vertices Pass 2: assign anchor vertices"));

    nanchors = ntosnap = 0;
    for (point = 1; point <= npoints; point++) {
	int i;

	G_percent(point, npoints, 2);

	G_debug(3, "  point = %d", point);

	if (XPnts[point].anchor >= 0)
	    continue;

	XPnts[point].anchor = 0;	/* make it anchor */
	nanchors++;

	/* Find points in threshold */
	rect.boundary[0] = XPnts[point].x - thresh;
	rect.boundary[3] = XPnts[point].x + thresh;
	rect.boundary[1] = XPnts[point].y - thresh;
	rect.boundary[4] = XPnts[point].y + thresh;
	rect.boundary[2] = 0;
	rect.boundary[5] = 0;

	Vect_reset_list(List);
	RTreeSearch(RTree, &rect, (void *)add_item, List);
	G_debug(4, "  %d points in threshold box", List->n_values);

	for (i = 0; i < List->n_values; i++) {
	    int pointb;
	    double dx, dy, dist2;

	    pointb = List->value[i];
	    if (pointb == point)
		continue;

	    dx = XPnts[pointb].x - XPnts[point].x;
	    dy = XPnts[pointb].y - XPnts[point].y;
	    dist2 = dx * dx + dy * dy;

	    if (dist2 > thresh2) /* outside threshold */
		continue;
		
	    /* doesn't have an anchor yet */
	    if (XPnts[pointb].anchor == -1) {
		XPnts[pointb].anchor = point;
		ntosnap++;
	    }
	    else if (XPnts[pointb].anchor > 0) {   /* check distance to previously assigned anchor */
		double dist2_a;

		dx = XPnts[XPnts[pointb].anchor].x - XPnts[pointb].x;
		dy = XPnts[XPnts[pointb].anchor].y - XPnts[pointb].y;
		dist2_a = dx * dx + dy * dy;

		/* replace old anchor */
		if (dist2 < dist2_a) {
		    XPnts[pointb].anchor = point;
		}
	    }
	}
    }

    /* Go through all lines and: 
     *   1) for all vertices: if not anchor snap it to its anchor
     *   2) for all segments: snap it to all anchors in threshold (except anchors of vertices of course) */

    nsnapped = ncreated = 0;

    G_verbose_message(_("Snap vertices Pass 3: snap to assigned points"));

    for (line_idx = 0; line_idx < List_lines->n_values; line_idx++) {
	int v, spoint, anchor;
	int changed = 0;

	G_percent(line_idx, List_lines->n_values, 2);

	line = List_lines->value[line_idx];

	G_debug(3, "line =  %d", line);
	if (!Vect_line_alive(Map, line))
	    continue;

	ltype = Vect_read_line(Map, Points, Cats, line);

	if (Points->n_points >= aindex) {
	    aindex = Points->n_points;
	    Index = (int *)G_realloc(Index, aindex * sizeof(int));
	}

	/* Snap all vertices */
	for (v = 0; v < Points->n_points; v++) {
	    /* Box */
	    rect.boundary[0] = Points->x[v];
	    rect.boundary[3] = Points->x[v];
	    rect.boundary[1] = Points->y[v];
	    rect.boundary[4] = Points->y[v];
	    rect.boundary[2] = 0;
	    rect.boundary[5] = 0;

	    /* Find point ( should always find one point ) */
	    Vect_reset_list(List);

	    RTreeSearch(RTree, &rect, (void *)add_item, List);

	    spoint = List->value[0];
	    anchor = XPnts[spoint].anchor;

	    if (anchor > 0) {	/* to be snapped */
		Points->x[v] = XPnts[anchor].x;
		Points->y[v] = XPnts[anchor].y;
		nsnapped++;
		changed = 1;
		Index[v] = anchor;	/* point on new location */
	    }
	    else {
		Index[v] = spoint;	/* old point */
	    }
	}

	/* New points */
	Vect_reset_line(NPoints);

	/* Snap all segments to anchors in threshold */
	for (v = 0; v < Points->n_points - 1; v++) {
	    int i;
	    double x1, x2, y1, y2, xmin, xmax, ymin, ymax;

	    G_debug(3, "  segment = %d end anchors : %d  %d", v, Index[v],
		    Index[v + 1]);

	    x1 = Points->x[v];
	    x2 = Points->x[v + 1];
	    y1 = Points->y[v];
	    y2 = Points->y[v + 1];

	    Vect_append_point(NPoints, Points->x[v], Points->y[v],
			      Points->z[v]);

	    /* Box */
	    if (x1 <= x2) {
		xmin = x1;
		xmax = x2;
	    }
	    else {
		xmin = x2;
		xmax = x1;
	    }
	    if (y1 <= y2) {
		ymin = y1;
		ymax = y2;
	    }
	    else {
		ymin = y2;
		ymax = y1;
	    }

	    rect.boundary[0] = xmin - thresh;
	    rect.boundary[3] = xmax + thresh;
	    rect.boundary[1] = ymin - thresh;
	    rect.boundary[4] = ymax + thresh;
	    rect.boundary[2] = 0;
	    rect.boundary[5] = 0;

	    /* Find points */
	    Vect_reset_list(List);
	    RTreeSearch(RTree, &rect, (void *)add_item, List);

	    G_debug(3, "  %d points in box", List->n_values);

	    /* Snap to anchor in threshold different from end points */
	    nnew = 0;
	    for (i = 0; i < List->n_values; i++) {
		double dist2, along;

		spoint = List->value[i];
		G_debug(4, "    spoint = %d anchor = %d", spoint,
			XPnts[spoint].anchor);

		if (spoint == Index[v] || spoint == Index[v + 1])
		    continue;	/* end point */
		if (XPnts[spoint].anchor > 0)
		    continue;	/* point is not anchor */

		/* Check the distance */
		dist2 =
		    dig_distance2_point_to_line(XPnts[spoint].x,
						XPnts[spoint].y, 0, x1, y1, 0,
						x2, y2, 0, 0, NULL, NULL,
						NULL, &along, NULL);

		G_debug(4, "      distance = %lf", sqrt(dist2));

		if (dist2 <= thresh2) {
		    G_debug(4, "      anchor in thresh, along = %lf", along);

		    if (nnew == anew) {
			anew += 100;
			New = (NEW *) G_realloc(New, anew * sizeof(NEW));
		    }
		    New[nnew].anchor = spoint;
		    New[nnew].along = along;
		    nnew++;
		}
	    }
	    G_debug(3, "  nnew = %d", nnew);
	    /* insert new vertices */
	    if (nnew > 0) {
		/* sort by distance along the segment */
		qsort(New, sizeof(char) * nnew, sizeof(NEW), sort_new);

		for (i = 0; i < nnew; i++) {
		    anchor = New[i].anchor;
		    /* Vect_line_insert_point ( Points, ++v, XPnts[anchor].x, XPnts[anchor].y, 0); */
		    Vect_append_point(NPoints, XPnts[anchor].x,
				      XPnts[anchor].y, 0);
		    ncreated++;
		}
		changed = 1;
	    }
	}

	/* append end point */
	v = Points->n_points - 1;
	Vect_append_point(NPoints, Points->x[v], Points->y[v], Points->z[v]);

	if (changed) {		/* rewrite the line */
	    Vect_line_prune(NPoints);	/* remove duplicates */
	    if (NPoints->n_points > 1 || ltype & GV_LINES) {
		Vect_rewrite_line(Map, line, ltype, NPoints, Cats);
	    }
	    else {
		Vect_delete_line(Map, line);
	    }
	    if (Err) {
		Vect_write_line(Err, ltype, Points, Cats);
	    }
	}
    }				/* for each line */
    G_percent(line_idx, List_lines->n_values, 2); /* finish it */

    Vect_destroy_line_struct(Points);
    Vect_destroy_line_struct(NPoints);
    Vect_destroy_cats_struct(Cats);
    G_free(XPnts);
    G_free(Index);
    G_free(New);
    RTreeDestroyNode(RTree);

    G_verbose_message(_("Snapped vertices: %d"), nsnapped);
    G_verbose_message(_("New vertices: %d"), ncreated);
}

/* for qsort */
static int sort_new(const void *pa, const void *pb)
{
    NEW *p1 = (NEW *) pa;
    NEW *p2 = (NEW *) pb;

    if (p1->along < p2->along)
	return -1;
    if (p1->along > p2->along)
	return 1;
    return 1;
}

/*!
 * \brief Snap lines in vector map to existing vertex in threshold.
 *
 * For details see Vect_snap_lines_list()
 *
 * \param[in] Map input map where vertices will be snapped
 * \param[in] type type of lines to snap
 * \param[in] thresh threshold in which snap vertices
 * \param[out] Err vector map where lines representing snap are written or NULL
 *
 * \return void
 */
void
Vect_snap_lines(struct Map_info *Map, int type, double thresh,
		struct Map_info *Err)
{
    int line, nlines, ltype;

    struct ilist *List;

    List = Vect_new_list();

    nlines = Vect_get_num_lines(Map);

    for (line = 1; line <= nlines; line++) {
	G_debug(3, "line =  %d", line);

	if (!Vect_line_alive(Map, line))
	    continue;

	ltype = Vect_read_line(Map, NULL, NULL, line);

	if (!(ltype & type))
	    continue;

	/* Vect_list_append(List, line); */
	dig_list_add(List, line);
    }

    Vect_snap_lines_list(Map, List, thresh, Err);

    Vect_destroy_list(List);

    return;
}

/*!
   \brief Snap a line to reference lines in Map with threshold.
  
   The line to snap and the reference lines can but do not need to be 
   in different vector maps.
   
   Vect_snap_line() uses less memory, but is slower than 
   Vect_snap_lines_list()

   For details on snapping, see Vect_snap_lines_list()

   \param[in] Map input map with reference lines
   \param[in] reflist list of reference lines
   \param[in,out] Points line points to snap
   \param[in] thresh threshold in which to snap vertices
   \param[in,out] nsnapped number of snapped verices
   \param[in,out] ncreated number of new vertices (on segments)
  
   \return 1 if line was changed, otherwise 0
 */
int
Vect_snap_line(struct Map_info *Map, struct ilist *reflist,
	       struct line_pnts *Points, double thresh,
	       int *nsnapped, int *ncreated)
{
    struct line_pnts *LPoints, *NPoints;
    struct line_cats *Cats;
    int i, v, line, nlines;
    int changed;
    double thresh2;

    int point;			/* index in points array */
    int segment;		/* index in segments array */
    int asegments;		/* number of allocated segments */
    int apoints, nvertices;	/* number of allocated points and registered vertices */
    XSEG *XSegs = NULL;		/* Array of segments */
    XPNT *XPnts = NULL;		/* Array of points */
    NEW2 *New = NULL;		/* Array of new points */
    int anew = 0, nnew;		/* allocated new points , number of new points */
    struct ilist *List;

    struct Node *pnt_tree,	/* spatial index for reference points */
                *seg_tree;	/* spatial index for reference segments */
    struct Rect rect;

    rect.boundary[0] = 0;
    rect.boundary[1] = 0;
    rect.boundary[2] = 0;
    rect.boundary[3] = 0;
    rect.boundary[4] = 0;
    rect.boundary[5] = 0;

    changed = 0;
    if (nsnapped)
	*nsnapped = 0;
    if (ncreated)
	*ncreated = 0;

    point = Points->n_points;
    Vect_line_prune(Points);
    if (point != Points->n_points)
	changed = 1;

    nlines = reflist->n_values;
    if (nlines < 1)
	return changed;

    LPoints = Vect_new_line_struct();
    NPoints = Vect_new_line_struct();
    Cats = Vect_new_cats_struct();
    List = Vect_new_list();
    pnt_tree = RTreeNewIndex();
    seg_tree = RTreeNewIndex();

    thresh2 = thresh * thresh;

    point = segment = 1;	/* index starts from 1 ! */
    nvertices = 0;
    asegments = 0;
    apoints = 0;

    /* Add all vertices and all segments of all reference lines 
     * to spatial indices */
    nlines = reflist->n_values;
    for (i = 0; i < nlines; i++) {

	line = reflist->value[i];

	G_debug(3, "line =  %d", line);
	if (!Vect_line_alive(Map, line))
	    continue;

	Vect_read_line(Map, LPoints, Cats, line);
	Vect_line_prune(LPoints);

	for (v = 0; v < LPoints->n_points; v++) {
	    G_debug(3, "  vertex v = %d", v);
	    nvertices++;

	    /* Box */
	    rect.boundary[0] = LPoints->x[v];
	    rect.boundary[3] = LPoints->x[v];
	    rect.boundary[1] = LPoints->y[v];
	    rect.boundary[4] = LPoints->y[v];

	    /* Already registered ? */
	    Vect_reset_list(List);
	    RTreeSearch(pnt_tree, &rect, (void *)find_item, List);
	    G_debug(3, "List : nvalues =  %d", List->n_values);

	    if (List->n_values == 0) {	/* Not found */

		/* Add to points tree */
		RTreeInsertRect(&rect, point, &pnt_tree, 0);

		if ((point - 1) == apoints) {
		    apoints += 10000;
		    XPnts =
			(XPNT *) G_realloc(XPnts,
					   (apoints + 1) * sizeof(XPNT));
		}
		XPnts[point].x = LPoints->x[v];
		XPnts[point].y = LPoints->y[v];
		XPnts[point].anchor = 0;

		point++;
	    }
	    
	    /* reference segments */
	    if (v) {
		/* Box */
		if (LPoints->x[v - 1] < LPoints->x[v]) {
		    rect.boundary[0] = LPoints->x[v - 1];
		    rect.boundary[3] = LPoints->x[v];
		}
		else {
		    rect.boundary[0] = LPoints->x[v];
		    rect.boundary[3] = LPoints->x[v - 1];
		}
		if (LPoints->y[v - 1] < LPoints->y[v]) {
		    rect.boundary[1] = LPoints->y[v - 1];
		    rect.boundary[4] = LPoints->y[v];
		}
		else {
		    rect.boundary[1] = LPoints->y[v];
		    rect.boundary[4] = LPoints->y[v - 1];
		}

		/* do not check for duplicates, too costly
		 * because different segments can have identical boxes */
		RTreeInsertRect(&rect, segment, &seg_tree, 0);

		if ((segment - 1) == asegments) {
		    asegments += 1000;
		    XSegs =
			(XSEG *) G_realloc(XSegs,
					   (asegments + 1) * sizeof(XSEG));
		}
		XSegs[segment].x1 = LPoints->x[v - 1];
		XSegs[segment].x2 = LPoints->x[v];
		XSegs[segment].y1 = LPoints->y[v - 1];
		XSegs[segment].y2 = LPoints->y[v];

		segment++;
	    }
	}
    }

    /* go through all vertices of the line to snap */
    for (v = 0; v < Points->n_points; v++) {
	double dist2, tmpdist2;
	double x, y;

	dist2 = thresh2 + thresh2;
	x = Points->x[v];
	y = Points->y[v];

	/* Box */
	rect.boundary[0] = Points->x[v] - thresh;
	rect.boundary[3] = Points->x[v] + thresh;
	rect.boundary[1] = Points->y[v] - thresh;
	rect.boundary[4] = Points->y[v] + thresh;

	/* find nearest reference vertex */
	Vect_reset_list(List);

	RTreeSearch(pnt_tree, &rect, (void *)add_item, List);

	for (i = 0; i < List->n_values; i++) {
	    double dx, dy;
	    
	    point = List->value[i];
	    
	    dx = Points->x[v] - XPnts[point].x;
	    dy = Points->y[v] - XPnts[point].y;
	    
	    tmpdist2 = dx * dx + dy * dy;
	    
	    if (tmpdist2 < dist2) {
		dist2 = tmpdist2;

		x = XPnts[point].x;
		y = XPnts[point].y;
	    }
	}

	if (dist2 <= thresh2 && dist2 > 0) {
	    Points->x[v] = x;
	    Points->y[v] = y;

	    changed = 1;
	    if (nsnapped)
		(*nsnapped)++;
	}
    }

    /* go through all vertices of the line to snap */
    for (v = 0; v < Points->n_points; v++) {
	double dist2, tmpdist2;
	double x, y;
	
	dist2 = thresh2 + thresh2;
	x = Points->x[v];
	y = Points->y[v];

	/* Box */
	rect.boundary[0] = Points->x[v] - thresh;
	rect.boundary[3] = Points->x[v] + thresh;
	rect.boundary[1] = Points->y[v] - thresh;
	rect.boundary[4] = Points->y[v] + thresh;

	/* find nearest reference segment */
	Vect_reset_list(List);

	RTreeSearch(seg_tree, &rect, (void *)add_item, List);

	for (i = 0; i < List->n_values; i++) {
	    double tmpx, tmpy;
	    int segment, status;
	    
	    segment = List->value[i];
	    
	    /* Check the distance */
	    tmpdist2 =
		dig_distance2_point_to_line(Points->x[v],
					    Points->y[v],
					    0, 
					    XSegs[segment].x1,
					    XSegs[segment].y1,
					    0,
					    XSegs[segment].x2,
					    XSegs[segment].y2,
					    0,
					    0, &tmpx, &tmpy, NULL,
					    NULL, &status);

	    if (tmpdist2 < dist2 && status == 0) {
		dist2 = tmpdist2;

		x = tmpx;
		y = tmpy;
	    }
	}

	if (dist2 <= thresh2 && dist2 > 0) {
	    Points->x[v] = x;
	    Points->y[v] = y;
	    
	    changed = 1;
	    if (nsnapped)
		(*nsnapped)++;
	}
    }

    RTreeDestroyNode(seg_tree);
    G_free(XSegs);

    /* go through all segments of the line to snap */
    /* find nearest reference vertex, add this vertex */
    for (v = 0; v < Points->n_points - 1; v++) {
	double x1, x2, y1, y2;
	double xmin, xmax, ymin, ymax;

	x1 = Points->x[v];
	x2 = Points->x[v + 1];
	y1 = Points->y[v];
	y2 = Points->y[v + 1];

	Vect_append_point(NPoints, Points->x[v], Points->y[v],
			  Points->z[v]);

	/* Box */
	if (x1 <= x2) {
	    xmin = x1;
	    xmax = x2;
	}
	else {
	    xmin = x2;
	    xmax = x1;
	}
	if (y1 <= y2) {
	    ymin = y1;
	    ymax = y2;
	}
	else {
	    ymin = y2;
	    ymax = y1;
	}

	rect.boundary[0] = xmin - thresh;
	rect.boundary[3] = xmax + thresh;
	rect.boundary[1] = ymin - thresh;
	rect.boundary[4] = ymax + thresh;

	/* Find points */
	Vect_reset_list(List);
	RTreeSearch(pnt_tree, &rect, (void *)add_item, List);

	G_debug(3, "  %d points in box", List->n_values);

	/* Snap to vertex in threshold different from end points */
	nnew = 0;
	for (i = 0; i < List->n_values; i++) {
	    double dist2, along;
	    int status;

	    point = List->value[i];

	    if (Points->x[v] == XPnts[i].x && 
	        Points->y[v] == XPnts[i].y)
		continue;	/* start point */

	    if (Points->x[v + 1] == XPnts[i].x && 
	        Points->y[v + 1] == XPnts[i].y)
		continue;	/* end point */

	    /* Check the distance */
	    dist2 =
		dig_distance2_point_to_line(XPnts[i].x,
					    XPnts[i].y,
					    0, x1, y1, 0,
					    x2, y2, 0, 0, NULL, NULL,
					    NULL, &along, &status);

	    if (dist2 <= thresh2 && status == 0) {
		G_debug(4, "      anchor in thresh, along = %lf", along);

		if (nnew == anew) {
		    anew += 100;
		    New = (NEW2 *) G_realloc(New, anew * sizeof(NEW2));
		}
		New[nnew].x = XPnts[i].x;
		New[nnew].y = XPnts[i].y;
		New[nnew].along = along;
		nnew++;
	    }
	    G_debug(3, "dist: %g, thresh: %g", dist2, thresh2);
	}
	G_debug(3, "  nnew = %d", nnew);
	/* insert new vertices */
	if (nnew > 0) {
	    /* sort by distance along the segment */
	    qsort(New, nnew, sizeof(NEW2), sort_new2);

	    for (i = 0; i < nnew; i++) {
		Vect_append_point(NPoints, New[i].x, New[i].y, 0);
		if (ncreated)
		    (*ncreated)++;
	    }
	    changed = 1;
	}
    }

    /* append end point */
    v = Points->n_points - 1;
    Vect_append_point(NPoints, Points->x[v], Points->y[v], Points->z[v]);

    if (Points->n_points != NPoints->n_points) {
	Vect_line_prune(NPoints);	/* remove duplicates */
	Vect_reset_line(Points);
	Vect_append_points(Points, NPoints, GV_FORWARD);
    }

    Vect_destroy_line_struct(LPoints);
    Vect_destroy_line_struct(NPoints);
    Vect_destroy_cats_struct(Cats);
    Vect_destroy_list(List);
    G_free(New);
    G_free(XPnts);
    RTreeDestroyNode(pnt_tree);

    return changed;
}
