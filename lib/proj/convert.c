/*
 * $Id$
 *
 ****************************************************************************
 *
 * MODULE:       gproj library
 * AUTHOR(S):    Paul Kelly
 *               Frank Warmerdam
 * PURPOSE:      Functions for manipulating co-ordinate system representations
 * COPYRIGHT:    (C) 2003 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include "config.h"

#ifdef HAVE_OGR
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "gprojects.h"
#include <cpl_csv.h>
#include "local_proto.h"

/* GRASS relative location of OGR co-ordinate system lookup tables */
#define CSVDIR "/etc/ogr_csv"

static void GPJ_DatumNameMassage( char ** );

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
    OGRSpatialReferenceH *hSRS = NULL;
    char *wkt;

    hSRS = GPJ_grass_to_osr(proj_info, proj_units);
  
    if (hSRS == NULL)
        return NULL;
   
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
 *         or NULL if it fails
 **/

OGRSpatialReferenceH *GPJ_grass_to_osr(struct Key_Value * proj_info,
				       struct Key_Value * proj_units)
{
    struct pj_info pjinfo;
    char *proj4, *proj4mod, *wkt, *modwkt, *startmod, *lastpart;
    OGRSpatialReferenceH *hSRS, *hSRS2;
    OGRErr errcode;
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

    if (pj_get_kv(&pjinfo, proj_info, proj_units) < 0) {
	G_warning("Can't parse GRASS PROJ_INFO file");
	return NULL;
    }

    if ((proj4 = pj_get_def(pjinfo.pj, 0)) == NULL) {
	G_warning("Can't get PROJ.4-style parameter string");
	return NULL;
    }

    unit = G_find_key_value("unit", proj_units);
    unfact = G_find_key_value("meters", proj_units);
    if (unfact != NULL && (strcmp(pjinfo.proj, "ll") != 0))
	G_asprintf(&proj4mod, "%s +to_meter=%s", proj4, unfact);
    else
	proj4mod = proj4;

    if ((errcode = OSRImportFromProj4(hSRS, proj4mod)) != OGRERR_NONE) {
	G_warning("OGR can't parse PROJ.4-style parameter string:\n" 
		      "%s\n(OGR Error code was %d)", proj4mod, errcode);
	return NULL;
    }

    if ((errcode = OSRExportToWkt(hSRS, &wkt)) != OGRERR_NONE) {
	G_warning("OGR can't get WKT-style parameter string\n"
		  "(OGR Error code was %d)", errcode);
	return NULL;
    }

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

int GPJ_osr_to_grass(struct Cell_head *cellhd, struct Key_Value **projinfo, 
                     struct Key_Value **projunits, OGRSpatialReferenceH *hSRS,
		     int interactive)
{
    struct Key_Value *temp_projinfo;
    char *pszProj4 = NULL, *pszRemaining;
    char *pszProj = NULL;
    
    if( hSRS == NULL )
        goto default_to_xy;
 
    /* Set finder function for locating OGR csv co-ordinate system tables */
    SetCSVFilenameHook( GPJ_set_csv_loc );
     
    /* Hopefully this doesn't do any harm if it wasn't in ESRI format
     * to start with... */
    OSRMorphFromESRI( hSRS );

/* -------------------------------------------------------------------- */
/*      Set cellhd for well known coordinate systems.                   */
/* -------------------------------------------------------------------- */
    if( !OSRIsGeographic( hSRS ) && !OSRIsProjected( hSRS ) )
        goto default_to_xy;

    if( cellhd )
    {
        int   bNorth;

        if( OSRIsGeographic( hSRS ) )
        {
            cellhd->proj = PROJECTION_LL;
            cellhd->zone = 0;
        }
        else if( OSRGetUTMZone( hSRS, &bNorth ) != 0 )
        {
            cellhd->proj = PROJECTION_UTM;
            cellhd->zone = OSRGetUTMZone( hSRS, &bNorth );
            if( !bNorth )
                cellhd->zone *= -1;
        }
        else 
        {
            cellhd->proj = PROJECTION_OTHER;
            cellhd->zone = 0;
        }
    }

/* -------------------------------------------------------------------- */
/*      Get the coordinate system definition in PROJ.4 format.          */
/* -------------------------------------------------------------------- */
    if( OSRExportToProj4( hSRS, &pszProj4 ) != OGRERR_NONE )
        goto default_to_xy;

/* -------------------------------------------------------------------- */
/*      Parse the PROJ.4 string into key/value pairs.  Do a bit of      */
/*      extra work to "GRASSify" the result.                            */
/* -------------------------------------------------------------------- */
    temp_projinfo = G_create_key_value();
   
    pszRemaining = pszProj4;
    while( (pszRemaining = strstr(pszRemaining,"+")) != NULL )
    {
        char 	*pszToken, *pszValue;

        pszRemaining++;
        
        /* Advance pszRemaining to end of this token[=value] pair */
        pszToken = pszRemaining;
        while( *pszRemaining != ' ' && *pszRemaining != '\0' )
            pszRemaining++;

        if( *pszRemaining == ' ' )
        {
            *pszRemaining = '\0';
            pszRemaining++;
        }

        /* parse token, and value */
        if( strstr(pszToken,"=") != NULL )
        {
            pszValue = strstr(pszToken,"=");
            *pszValue = '\0';
            pszValue++;
        }
        else
            pszValue = "defined";
        

        if( G_strcasecmp(pszToken,"proj") == 0 )
	{
            /* The ll projection is known as longlat in PROJ.4 */
            if( G_strcasecmp(pszValue,"longlat") == 0 )
	        pszValue = "ll";
	   
            pszProj = pszValue;
	    continue;
	}

        /* Ellipsoid and datum handled separately below */
        if( G_strcasecmp(pszToken,"ellps") == 0
            || G_strcasecmp(pszToken,"a") == 0
            || G_strcasecmp(pszToken,"b") == 0
            || G_strcasecmp(pszToken,"es") == 0
            || G_strcasecmp(pszToken,"rf") == 0
            || G_strcasecmp(pszToken,"datum") == 0 )
            continue;
       
        /* We will handle units separately */
        if( G_strcasecmp(pszToken,"to_meter") == 0 
            || G_strcasecmp(pszToken,"units") == 0 )
            continue;

        G_set_key_value( pszToken, pszValue, temp_projinfo );
    }

    *projinfo = G_create_key_value();
   
/* -------------------------------------------------------------------- */
/*      Derive the user name for the projection.                        */
/* -------------------------------------------------------------------- */
    {
        char	path[4095];
        char    name[80];

        sprintf(path,"%s/etc/projections",G_gisbase());
        if( G_lookup_key_value_from_file(path,pszProj,name,sizeof(name)) > 0 )
            G_set_key_value( "name", name, *projinfo );
        else
            G_set_key_value( "name", pszProj, *projinfo );
    }

    G_set_key_value( "proj", pszProj, *projinfo );
       
/* -------------------------------------------------------------------- */
/*      Find the GRASS datum name and choose parameters either          */
/*      interactively or not.                                           */
/* -------------------------------------------------------------------- */

    {   
        const char *pszDatumNameConst = OSRGetAttrValue( hSRS, "DATUM", 0 );
        struct datum_list *list, *listhead;
        char *datum = NULL, *dum1, *dum2, *pszDatumName;
        int paramspresent = GPJ__get_datum_params(temp_projinfo, &dum1, &dum2);

        if ( pszDatumNameConst )
        {
	    /* Need to make a new copy of the string so we don't mess
	     * around with the memory inside the OGRSpatialReferenceH? */
	    G_asprintf( &pszDatumName, pszDatumNameConst );
            GPJ_DatumNameMassage( &pszDatumName );

            list = listhead = read_datum_table();
 
            while (list != NULL) {
    	        if (strcasecmp(pszDatumName, list->longname) == 0) {
    	            datum = G_store(list->name);
	            break;
	        }
	        list = list->next;
            }
            free_datum_list(listhead);
	   
	    if (datum == NULL)
	    {
	        if( paramspresent < 2)
		    /* Only give warning if no parameters present */
	            G_warning("Datum '%s' not recognised by GRASS and no parameters found. "
			      "Datum transformation will not be possible using this projection information.",
			      pszDatumName);
	    }
            else
	    {
                G_set_key_value( "datum", datum, *projinfo );        
       
                if (paramspresent < 2)
    	        {
	            /* If no datum parameters were imported from the OSR
	             * object then we may prompt the user */
                    char *params, *chosenparams;
                    int paramsets;
		   
		    fprintf(stderr, "A datum name %s (%s) was specified "
			            "without transformation parameters.\n",
			            datum, pszDatumName);
                    if( (paramsets = GPJ_get_default_datum_params_by_name(datum, &params))
		        == 1 ) 
		        /* GRASS only knows one parameter set for this so
			 * just use it but inform the user what we're doing */
		        fprintf(stderr, "Note that the GRASS default for %s "
			            "is %s.\n", datum, params);
                    else if( paramsets < 0 )
                        G_warning("Datum '%s' apparently recognised by GRASS but no parameters found. "
                                  "You may want to look into this.", datum );
                    else if( interactive && (GPJ_ask_datum_params(datum, &chosenparams) > 0) )
		    {
		        /* Force the user to think about it and make a 
			 * decision on which set of parameters is most
			 * appropriate for his/her location */

                        char *paramkey, *paramvalue;
                        paramkey = strtok(chosenparams, "=");
                        paramvalue = chosenparams + strlen(paramkey) + 1;
                        G_set_key_value( paramkey, paramvalue, *projinfo );
                        G_free( chosenparams );
		    }
                    else if( !interactive )
                        G_warning("Non-interactive mode: the GRASS default "
				  "for %s is %s.\n", datum, params);
		    else
		        G_warning("No parameters specified: the GRASS default "
				  "for %s is %s.\n", datum, params);
                    if(paramsets > 0)
                        G_free(params);
	        }
       
                G_free(datum);
	    }
	}   
    }

/* -------------------------------------------------------------------- */
/*	Despite having the +ellps set, GRASS still requires +a and +es	*/
/* -------------------------------------------------------------------- */

    {
        const char *pszSemiMajor = OSRGetAttrValue( hSRS, "SPHEROID", 1 );
        const char *pszInvFlat = OSRGetAttrValue( hSRS, "SPHEROID", 2 );

        if( strstr(pszProj4,"+a") == NULL && pszSemiMajor != NULL )
            G_set_key_value( "a", (char *) pszSemiMajor, *projinfo );

        if( pszInvFlat != NULL )
        {
            double	es, flat;
            char	es_str[100];

            flat = 1 / atof(pszInvFlat);
            
            es = flat * (2.0 - flat);

            sprintf( es_str, "%.10f", es );
            G_set_key_value( "es", es_str, *projinfo );
        }
    }

/* -------------------------------------------------------------------- */
/*	Finally append the detailed projection parameters to the end	*/
/* -------------------------------------------------------------------- */

    {
        int i;
        
        for ( i = 0; i < temp_projinfo->nitems; i++)	 	
	    G_set_key_value( temp_projinfo->key[i], 
			     temp_projinfo->value[i], *projinfo );
       
        G_free_key_value( temp_projinfo );
    }
       
    free( pszProj4 ); /* hopefully the same as CPLFree()! */

/* -------------------------------------------------------------------- */
/*      Set the linear units.                                           */
/* -------------------------------------------------------------------- */
    *projunits = G_create_key_value();
    
    if( OSRIsGeographic( hSRS ) )
    {
        /* We assume degrees ... someday we will be wrong! */
        G_set_key_value( "unit", "degree", *projunits );
        G_set_key_value( "units", "degrees", *projunits );
        G_set_key_value( "meter", "1.0", *projunits );
    }
    else 
    {
        char	szFormatBuf[256];
        char    *pszUnitsName = NULL;
        double  dfToMeters;

        dfToMeters = OSRGetLinearUnits( hSRS, &pszUnitsName );
        
        /* Workaround for the most obvious case when unit name is unknown */
        if( (strcasecmp(pszUnitsName, "unknown") == 0) && (dfToMeters == 1.) )
	    G_asprintf( &pszUnitsName, "meter" );
       
        G_set_key_value( "unit", pszUnitsName, *projunits );
        sprintf( szFormatBuf, "%ss", pszUnitsName );
        G_set_key_value( "units", szFormatBuf, *projunits );
        sprintf( szFormatBuf, "%.16g", dfToMeters );
        G_set_key_value( "meter", szFormatBuf, *projunits );

    }

    return 1;

/* -------------------------------------------------------------------- */
/*      Fallback to returning an ungeoreferenced definition.            */
/* -------------------------------------------------------------------- */
  default_to_xy:
    if( cellhd != NULL )
    {
        cellhd->proj = PROJECTION_XY;
        cellhd->zone = 0;
    }

    *projinfo = NULL;
    *projunits = NULL;
    
    return 1; 
}


int GPJ_wkt_to_grass(struct Cell_head *cellhd, struct Key_Value **projinfo, 
                     struct Key_Value **projunits, const char *wkt,
		     int interactive)
{
    int retval;

    if( wkt == NULL )
        retval = GPJ_osr_to_grass(cellhd, projinfo, projunits, NULL, interactive);
    else
    {
        OGRSpatialReferenceH *hSRS;

        /* Set finder function for locating OGR csv co-ordinate system tables */
        SetCSVFilenameHook( GPJ_set_csv_loc );
       
        hSRS = OSRNewSpatialReference(wkt);
        retval = GPJ_osr_to_grass(cellhd, projinfo, projunits, hSRS, interactive);
        OSRDestroySpatialReference(hSRS);
    }
   
    return retval;
}


/* GPJ_set_csv_loc()
 * 'finder function' for use with OGR SetCSVFilenameHook() function */

const char *GPJ_set_csv_loc(const char *name)
{
    const char *gisbase = G_gisbase();
    static char *buf = NULL;

    if (buf != NULL)
        G_free(buf);

    G_asprintf(&buf, "%s%s/%s", gisbase, CSVDIR, name);

    return buf;
}

/* N.B. The order of these pairs is different from that in 
 * ogr/ogrfromepsg.cpp in the GDAL source tree! GRASS uses the EPSG
 * names in its WKT representation except WGS_1984 and WGS_1972 as
 * these shortened versions seem to be standard */

static const char *papszDatumEquiv[] =
{
    "Militar_Geographische_Institute",
    "Militar_Geographische_Institut",
    "World_Geodetic_System_1984",
    "WGS_1984",
    "World_Geodetic_System_1972",
    "WGS_1972",
    "European_Terrestrial_Reference_System_89",
    "European_Terrestrial_Reference_System_1989",
    "European_Reference_System_1989",
    "European_Terrestrial_Reference_System_1989",
    "NZGD_2000",
    "New_Zealand_Geodetic_Datum_2000",
    "Monte_Mario_Rome",
    "Monte_Mario",
    NULL
};

/************************************************************************/
/*                      OGREPSGDatumNameMassage()                       */
/*                                                                      */
/*      Massage an EPSG datum name into WMT format.  Also transform     */
/*      specific exception cases into WKT versions.                     */
/************************************************************************/

static void GPJ_DatumNameMassage( char ** ppszDatum )

{
    int         i, j;
    char        *pszDatum = *ppszDatum;

/* -------------------------------------------------------------------- */
/*      Translate non-alphanumeric values to underscores.               */
/* -------------------------------------------------------------------- */
    for( i = 0; pszDatum[i] != '\0'; i++ )
    {
        if( !(pszDatum[i] >= 'A' && pszDatum[i] <= 'Z')
            && !(pszDatum[i] >= 'a' && pszDatum[i] <= 'z')
            && !(pszDatum[i] >= '0' && pszDatum[i] <= '9') )
        {
            pszDatum[i] = '_';
        }
    }

/* -------------------------------------------------------------------- */
/*      Remove repeated and trailing underscores.                       */
/* -------------------------------------------------------------------- */
    for( i = 1, j = 0; pszDatum[i] != '\0'; i++ )
    {
        if( pszDatum[j] == '_' && pszDatum[i] == '_' )
            continue;

        pszDatum[++j] = pszDatum[i];
    }
    if( pszDatum[j] == '_' )
        pszDatum[j] = '\0';
    else
        pszDatum[j+1] = '\0';
    
/* -------------------------------------------------------------------- */
/*      Search for datum equivelences.  Specific massaged names get     */
/*      mapped to OpenGIS specified names.                              */
/* -------------------------------------------------------------------- */
    for( i = 0; papszDatumEquiv[i] != NULL; i += 2 )
    {
        if( EQUAL(*ppszDatum,papszDatumEquiv[i]) )
        {
            CPLFree( *ppszDatum );
            *ppszDatum = CPLStrdup( papszDatumEquiv[i+1] );
            break;
        }
    }
}

#endif /* HAVE_OGR */
