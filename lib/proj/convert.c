/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       gproj library
 * AUTHOR(S):    Paul Kelly
 * PURPOSE:      Functions for manipulating co-ordinate system representations
 * COPYRIGHT:    (C) 2003 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include "config.h"
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "gprojects.h"

#ifdef HAVE_OGR

/**
 * \brief Convert a GRASS co-ordinate system representation to WKT style
 * 
 * Takes a GRASS co-ordinate system as specifed by two sets of key/value
 * pairs derived from the PROJ_INFO and PROJ_UNITS files, and converts it
 * to the 'Well Known Text' format popularised by proprietary GIS
 * 
 * \param proj_info Set of GRASS PROJ_INFO key/value pairs
 * \param proj_units Set of GRASS PROJ_UNIT key/value pairs
 * \param esri_style boolean Output ESRI-style WKT (Use OSRMorphToESRI() 
 *                   function provided by OGR library)
 * \param prettify boolean Use linebreaks and indents to 'prettify' output
 *                 WKT string (Use OSRExportToPrettyWkt() function in OGR)
 *
 * \return Pointer to a string containing the co-ordinate system in WKT
 *         format
 **/

char *GPJ_grass_to_wkt(struct Key_Value *proj_info,
		       struct Key_Value *proj_units,
		       int esri_style, int prettify)
{
    OGRSpatialReferenceH *hSRS;
    char *wkt;

    hSRS = GPJ_grass_to_osr(proj_info, proj_units);

    if (esri_style)
	OSRMorphToESRI(hSRS);

    if (prettify)
	OSRExportToPrettyWkt(hSRS, &wkt, 0);
    else
	OSRExportToWkt(hSRS, &wkt);

    OSRDestroySpatialReference(hSRS);
    return wkt;
}

/**
 * Convert a GRASS co-ordinate system to an OGRSpatialReferenceH object
 * 
 * \param proj_info Set of GRASS PROJ_INFO key/value pairs
 * \param proj_units Set of GRASS PROJ_UNIT key/value pairs
 * 
 * \return Pointer to an OGRSpatialReferenceH object representing the
 *         co-ordinate system defined by proj_info and proj_units
 **/

OGRSpatialReferenceH *GPJ_grass_to_osr(struct Key_Value * proj_info,
				       struct Key_Value * proj_units)
{
    struct pj_info pjinfo;
    char *proj4, *proj4mod, *wkt, *modwkt, *startmod, *lastpart;
    OGRSpatialReferenceH *hSRS, *hSRS2;
    struct gpj_datum dstruct;
    size_t len;
    char *ellps, *datum, *params, *towgs84, *datumlongname, *start, *end,
	*unit, *unfact, *buff;
    const char *sysname, *osrunit, *osrunfact;
    double a, es, rf;
    int haveparams = 0;

    if( (proj_info == NULL) || (proj_units == NULL) )
        return NULL;

    hSRS = OSRNewSpatialReference(NULL);

    if (pj_get_kv(&pjinfo, proj_info, proj_units) < 0)
	G_fatal_error("Can't parse GRASS PROJ_INFO file");

    if ((proj4 = pj_get_def(pjinfo.pj, 0)) == NULL)
	G_fatal_error("Can't get PROJ.4-style parameter string");

    unit = G_find_key_value("unit", proj_units);
    unfact = G_find_key_value("meters", proj_units);
    if (unfact != NULL && (strcmp(pjinfo.proj, "ll") != 0))
	G_asprintf(&proj4mod, "%s +to_meter=%s", proj4, unfact);
    else
	proj4mod = proj4;

    if (OSRImportFromProj4(hSRS, proj4mod) != 0)
	G_fatal_error("OGR can't parse PROJ.4-style parameter string:\n"
		      "%s", proj4mod);

    if (OSRExportToWkt(hSRS, &wkt) != 0)
	G_fatal_error("OGR can't get WKT-style parameter string");

    ellps = G_find_key_value("ellps", proj_info);
    GPJ__get_ellipsoid_params(proj_info, &a, &es, &rf);
    haveparams = GPJ__get_datum_params(proj_info, &datum, &params);

    if ((datum == NULL) || (GPJ_get_datum_by_name(datum, &dstruct) < 0)) {
	G_asprintf(&datumlongname, "unknown");
	if (ellps == NULL)
	    G_asprintf(&ellps, "unnamed");
    }
    else {
	datumlongname = G_store(dstruct.longname);
	if (ellps == NULL)
	    ellps = G_store(dstruct.ellps);
	GPJ_free_datum(&dstruct);
    }

    startmod = G_strstr(wkt, "GEOGCS");
    lastpart = G_strstr(wkt, "PRIMEM");
    len = strlen(wkt) - strlen(startmod);
    wkt[len] = '\0';
    if (haveparams == 2) {
	/* Only put datum params into the WKT if they were specifically
	 * specified in PROJ_INFO */
	char *paramkey, *paramvalue;
	paramkey = strtok(params, "=");
	paramvalue = params + strlen(paramkey) + 1;
	if (strcasecmp(paramkey, "towgs84") == 0)
	    G_asprintf(&towgs84, ",TOWGS84[%s]", paramvalue);
	else
	    towgs84 = "";
    }
    else
	towgs84 = "";

    sysname = OSRGetAttrValue(hSRS, "PROJCS", 0);
    if (sysname == NULL) {
	/* Not a projected co-ordinate system */
	start = "";
	end = "";
    }
    else {
	if ((strncmp(sysname, "unnamed", 7) == 0) &&
	    (G_find_key_value("name", proj_info) != NULL))
	    G_asprintf(&start, "PROJCS[\"%s\",",
		       G_find_key_value("name", proj_info));
	else
	    start = G_store(wkt);

	osrunit = OSRGetAttrValue(hSRS, "UNIT", 0);
	osrunfact = OSRGetAttrValue(hSRS, "UNIT", 1);

	if ((unfact == NULL) || (strcasecmp(osrunit, "unknown") != 0))
	    end = "";
	else {
	    double unfactf = atof(unfact);

	    G_asprintf(&buff, ",UNIT[\"%s\",", osrunit);

	    startmod = G_strstr(lastpart, buff);
	    len = strlen(lastpart) - strlen(startmod);
	    lastpart[len] = '\0';

	    if (unit == NULL)
		G_asprintf(&unit, "unknown");
	    G_asprintf(&end, ",UNIT[\"%s\",%.16g]]", unit, unfactf);
	}

    }

    G_asprintf(&modwkt,
	       "%sGEOGCS[\"%s\",DATUM[\"%s\",SPHEROID[\"%s\",%.16g,%.16g]%s],%s%s",
	       start, ellps, datumlongname, ellps, a, rf, towgs84, lastpart,
	       end);

    hSRS2 = OSRNewSpatialReference(modwkt);

    OSRDestroySpatialReference(hSRS);
    G_free(modwkt);
    G_free(wkt);
    if (proj4 != proj4mod)
	G_free(proj4);
    G_free(proj4mod);
    G_free(datum);
    G_free(params);
    G_free(datumlongname);
    pj_free(pjinfo.pj);
    /* Other string pointers may or may not need to be freed depending
     * on sequence of execution so just leave them. */

    return hSRS2;
}

/*
 * int GPJ_osr_to_grass(struct Cell_Head *cellhd, struct Key_Value *projinfo, 
 * struct Key_Value *projunits, OGRSpatialReferenceH *hSRS)
 * {
 * This function is currently in r.in.gdal but will be moved here
 * 
 * 
 * }
 */

#endif /* HAVE_OGR */
