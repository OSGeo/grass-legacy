/*!
  \file geos.c
  
  \brief Vector library - GEOS support
  
  Higher level functions for reading/writing/manipulating vectors.
  
  Note: current GEOS support is *very* slow

  (C) 2009 by the GRASS Development Team
  
  This program is free software under the GNU General Public License
  (>=v2).  Read the file COPYING that comes with GRASS for details.
  
  \author Martin Landa <landa.martin gmail.com>
 */

#include <grass/config.h>
#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/glocale.h>

#ifdef HAVE_GEOS
#include <geos_c.h>

static struct line_pnts *Points;

static GEOSGeometry *Vect__read_line_geos(struct Map_info *, long);

/*!
   \brief Read vector feature and stores it as GEOSGeometry instance

   Note: Free allocated memory by GEOSGeom_destroy().

   \param Map pointer to Map_info structure
   \param line feature id

   \return pointer to GEOSGeometry instance
   \return NULL on error
 */
GEOSGeometry *Vect_read_line_geos(const struct Map_info *Map, int line)
{
    P_LINE *Line;
    
    G_debug(3, "Vect_read_line_geos(): line = %d", line);
    
    if (!VECT_OPEN(Map))
	G_fatal_error("Vect_read_line_geos(): %s", _("vector map is not opened"));
    
    if (line < 1 || line > Map->plus.n_lines)
	G_fatal_error(_("Vect_read_line_geos(): feature id %d is not reasonable "
			"(max features in vector map <%s>: %d)"),
		      line, Vect_get_full_name(Map), Map->plus.n_lines);
    
    if (Map->format != GV_FORMAT_NATIVE)
	G_fatal_error("Vect_read_line_geos(): %s", _("only native format supported"));
    
    Line = Map->plus.Line[line];
    if (Line == NULL)
	G_fatal_error("Vect_read_line_geos(): %s %d",
		      _("Attempt to read dead line"), line);
    
    return Vect__read_line_geos(Map, Line->offset);
}

/*!
   \brief Read vector area and stores it as GEOSGeometry instance

   Note: Free allocated memory by GEOSGeom_destroy().

   \param Map pointer to Map_info structure
   \param area area id 

   \return pointer to GEOSGeometry instance
   \return NULL on error
 */
GEOSGeometry *Vect_read_area_geos(const struct Map_info * Map, int area)
{
    int i, type, nholes, isle;
    GEOSGeometry *boundary, **holes;

    if (!Points)
	Points = Vect_new_line_struct();

    G_debug(3, "Vect_read_area_geos(area=%d)", area);

    if (Vect_get_area_points(Map, area, Points) == -1) {
	G_fatal_error(_("Vect_read_area_geos(): unable to read area id %d"),
		      area);
    }
    boundary = Vect_line_to_geos(Map, Points, GV_BOUNDARY);
    if (!boundary) {
	G_fatal_error(_("Vect_read_area_geos(): unable to read area id %d"),
		      area);
    }

    nholes = Vect_get_area_num_isles(Map, area);
    holes = (GEOSGeometry **) G_malloc(nholes * sizeof(GEOSGeometry *));
    for (i = 0; i < nholes; i++) {
	isle = Vect_get_area_isle(Map, area, i);
	if (isle < 1)
	    continue;
	if (Vect_get_isle_points(Map, isle, Points) < 0)
	    G_fatal_error(_("Vect_read_area_geos(): unable to read isle id %d of area id %d"),
			  isle, area);
	holes[i] = Vect_line_to_geos(Map, Points, GV_BOUNDARY);
    }

    return GEOSGeom_createPolygon(boundary, holes, nholes);
}

/*!
   \brief Create GEOSGeometry of given type from feature points.

   Supported types:
   - GV_POINT    -> POINT
   - GV_LINE     -> LINESTRING
   - GV_BOUNDARY -> LINERING

   Note: Free allocated memory by GEOSGeom_destroy().

   \param Map pointer to Map_info structure
   \param type feature type (see supported types)

   \return pointer to GEOSGeometry instance
   \return NULL on error
 */
GEOSGeometry *Vect_line_to_geos(const struct Map_info * Map,
				const struct line_pnts * points, int type)
{
    int i, with_z;
    GEOSGeometry *geom;
    GEOSCoordSequence *pseq;

    G_debug(3, "Vect_line_to_geos(): type = %d", type);
    
    with_z = Vect_is_3d(Map);
    
    /* read only points / lines / boundaries */
    if (!(type & (GV_POINT | GV_LINES)))
	return NULL;

    if (type == GV_POINT) { 
	if (points->n_points != 1)
	    /* point is not valid */
	    return NULL;
    }
    else {			
	if (points->n_points < 2)
	    /* line/boundary is not valid */
	    return NULL;
    }
    
    pseq = GEOSCoordSeq_create(points->n_points, with_z ? 3 : 2);
    
    for (i = 0; i < points->n_points; i++) {
	GEOSCoordSeq_setX(pseq, i, points->x[i]);
	GEOSCoordSeq_setY(pseq, i, points->y[i]);
	if (with_z)
	    GEOSCoordSeq_setZ(pseq, i, points->z[i]);
    }

    if (type == GV_POINT)
	geom = GEOSGeom_createPoint(pseq);
    else if (type == GV_LINE)
	geom = GEOSGeom_createLineString(pseq);
    else { /* boundary */
	geom = GEOSGeom_createLineString(pseq);
	if (GEOSisRing(geom)) {
	    /* GEOSGeom_destroy(geom); */
	    geom = GEOSGeom_createLinearRing(pseq);
	}
    }
    
    /* GEOSCoordSeq_destroy(pseq); */

    return geom;
}

/*!  
  \brief Read line from coor file
  
  \param Map pointer to Map_info
  \param offset lineoffset
 
  \return pointer to GEOSGeometry
  \return NULL on error
  \return NULL dead line
  \return NULL end of file
*/
GEOSGeometry *Vect__read_line_geos(struct Map_info *Map, long offset)
{
    int i;
    int n_points;
    int type;
    char rhead;
    double *x, *y, *z;
    
    GEOSGeometry *geom;
    GEOSCoordSequence *pseq;
    
    G_debug(3, "Vect__read_line_geos(): offset = %ld", offset);
    
    Map->head.last_offset = offset;
    
    /* reads must set in_head, but writes use default */
    dig_set_cur_port(&(Map->head.port));
    
    dig_fseek(&(Map->dig_fp), offset, 0);
    
    if (0 >= dig__fread_port_C(&rhead, 1, &(Map->dig_fp)))
	return NULL;            /* end of file */
    
    if (!(rhead & 0x01))	/* dead line */
	return NULL;
    
    rhead >>= 2;
    type = dig_type_from_store((int)rhead);
    
    /* read only points / lines / boundaries */
    if (!(type & (GV_POINT | GV_LINES)))
	return NULL;
    
    if (type & GV_POINTS) {
	n_points = 1;
    }
    else {
	if (0 >= dig__fread_port_I(&n_points, 1, &(Map->dig_fp)))
	    return NULL;
	if (n_points < 2)
	    /* line / boundary is not valid */
	    return NULL;
    }
    
    G_debug(3, "    n_points = %d dim = %d", n_points, (Map->head.with_z) ? 3 : 2);
    
    pseq = GEOSCoordSeq_create(n_points, (Map->head.with_z) ? 3 : 2);
    
    x = (double *) G_malloc(n_points * sizeof(double));
    y = (double *) G_malloc(n_points * sizeof(double));
    if (Map->head.with_z)
	z = (double *) G_malloc(n_points * sizeof(double));
    else
	z = NULL;
    
    if (0 >= dig__fread_port_D(x, n_points, &(Map->dig_fp)))
	return NULL; /* end of file */

    if (0 >= dig__fread_port_D(y, n_points, &(Map->dig_fp)))
	return NULL; /* end of file */

    if (Map->head.with_z) {
	if (0 >= dig__fread_port_D(z, n_points, &(Map->dig_fp)))
	    return NULL; /* end of file */

    }

    for (i = 0; i < n_points; i++) {
	GEOSCoordSeq_setX(pseq, i, x[i]);    
	GEOSCoordSeq_setY(pseq, i, y[i]);
	if (Map->head.with_z)
	    GEOSCoordSeq_setZ(pseq, i, z[i]);
    }
    
    G_debug(3, "    off = %ld", dig_ftell(&(Map->dig_fp)));

    if (type & GV_POINT) {
	G_debug(3, "    geos_type = point");
	geom = GEOSGeom_createPoint(pseq);
    }
    else if (type & GV_LINE) {
	G_debug(3, "    geos_type = linestring");
	geom = GEOSGeom_createLineString(pseq);
    }
    else { /* boundary */
	geom = GEOSGeom_createLineString(pseq);
	if (GEOSisRing(geom)) {
	    /* GEOSGeom_destroy(geom); */
	    geom = GEOSGeom_createLinearRing(pseq);
	    G_debug(3, "    geos_type = linearring");
	}
	else {
	    G_debug(3, "    geos_type = linestring");
	}
    }
    
    G_free((void *) x);
    G_free((void *) y);
    if (z)
	G_free((void *) z);
    
    /* GEOSCoordSeq_destroy(pseq); */
    
    return geom;
}

#endif /* HAVE_GEOS */
