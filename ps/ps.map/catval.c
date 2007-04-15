/****************************************************************************
 *
 * MODULE:       ps.map
 * FILE:         catval.c
 * AUTHOR(S):    Martin Landa <landa.martin@gmail.com>
 * PURPOSE:      This is an enhanced PostScript version of the p.map program
 * COPYRIGHT:    (C) 2007 by the GRASS Development Team
 *
 *               This program is free software under the GNU General Public
 *               License (>=v2). Read the file COPYING that comes with GRASS
 *               for details.
 *
 *****************************************************************************/

#include <grass/gis.h>
#include <grass/Vect.h>
#include <grass/dbmi.h>
#include <grass/glocale.h>

#include "vector.h"

/**
 * \fn int load_catval_array_rgb (struct Map_info* map, int vec, dbCatValArray* cvarr_rgb)
 *
 * \brief Loads categories and RGB color definition into dbCatValArray structure
 *
 * \param map: poiter to a vector Map_info structure
 * \param level: layer number identifier
 * \param level: output dbCatValArray structure
 *
 * \return number of records or -1 if it fails
 */

int load_catval_array_rgb (struct Map_info* map, int vec, dbCatValArray* cvarr_rgb)
{
    int i, nrec, ctype;
    struct field_info *Fi;
    dbDriver *driver;
    
    db_CatValArray_init (cvarr_rgb);
    
    Fi = Vect_get_field (map, vector.layer[vec].field);
    if (Fi == NULL) {
	G_fatal_error(_("Cannot get layer info for vector map"));
    }
    
    driver = db_start_driver_open_database (Fi->driver, Fi->database);
    if (driver == NULL)
	G_fatal_error(_("Cannot open database <%s> by driver <%s>"), Fi->database, Fi->driver);
    
    nrec = db_select_CatValArray (driver, Fi->table, Fi->key, 
				  vector.layer[vec].rgbcol, NULL, cvarr_rgb);
    
    G_debug (3, "nrec_rgb = %d", nrec);
    
    ctype = cvarr_rgb -> ctype;
    if (ctype != DB_C_TYPE_STRING)
	G_fatal_error (_("Column type not supported (must be string)"));
    
    if ( nrec < 0 )
	    G_fatal_error (_("Cannot select data from table"));
    
    G_debug(2, "\n%d records (rgbcol) selected from table", nrec);
    
    for (i = 0; i < cvarr_rgb -> n_values; i++) {
	G_debug (4, "cat = %d val = %s", cvarr_rgb -> value[i].cat, cvarr_rgb -> value[i].val.s->string);
    }
    
    db_close_database_shutdown_driver (driver);

    return nrec;
}
