/*****************************************************************************
*
* MODULE:       MySQL driver forked from DBF driver by Radim Blazek 
*   	    	
* AUTHOR(S):    Alex Shevlakov
*
* PURPOSE:      Simple driver for reading and writing data     
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

#include <dbmi.h>
#include <datetime.h>
#include "globals.h"
#include "proto.h"

int db__driver_describe_table(table_name, table)
     dbString *table_name;
     dbTable **table;
{
    int tab;
    char *name;

    name = db_get_string(table_name);

    tab = find_table(name);
    describe_table(tab, NULL, 0, table);

    return DB_OK;
}

/* scols is array of indexes selected columns or null,
 * if nscols == 0 => describe all columns */
int describe_table(int tab, int *scols, int nscols, dbTable ** table)
{
    int i, col, ncols, dbtype;
/*     int precision, scale; */
    dbColumn *column;
    COLUMN *dcol;

/*    load_table_head ( tab );*/

    load_table(tab, NULL);

    ncols = db.tables[tab].ncols;

    if (nscols > 0)
	ncols = nscols;

    if (!(*table = db_alloc_table(ncols))) {
	return DB_FAILED;
    }

    for (i = 0; i < ncols; i++) {
	if (nscols > 0)
	    col = scols[i];
	else
	    col = i;

	dcol = &(db.tables[tab].cols[col]);
	column = db_get_table_column(*table, i);

	db_set_column_name(column, dcol->name);
	db_set_column_length(column, dcol->width);
	db_set_column_host_type(column, dcol->type);

	switch (dcol->type) {
	case MYSQL_INT:
	    dbtype = DB_SQL_TYPE_INTEGER;
/*
 *             precision = dcol->width - 1; 
 *             scale = 0;  
 */
	    break;
	case MYSQL_DOUBLE:
	    dbtype = DB_SQL_TYPE_DOUBLE_PRECISION;
/*
 *             precision = dcol->width - 2;  
 *             scale = dcol->decimals;  
 */
	    break;
	case MYSQL_CHAR:
	    dbtype = DB_SQL_TYPE_CHARACTER;
/*
 *             precision = 0;
 *             scale = 0; 
 */
	    break;

	default:
	    dbtype = DB_SQL_TYPE_UNKNOWN;
	    break;
	}

	db_set_column_sqltype(column, dbtype);

/*
 *         db_set_column_precision (column, precision);
 * 	db_set_column_scale (column, scale);
 */

	db_set_column_null_allowed(column);
	db_set_column_has_undefined_default_value(column);
	db_unset_column_use_default_value(column);

/*
 *         db_set_column_select_priv_granted (column);
 * 	
 *         if ( db.tables[tab].write )
 *             db_set_column_update_priv_granted (column);
 * 	   else
 *             db_set_column_update_priv_not_granted (column); 
 */


    }

    /* set the table name */
    db_set_table_name(*table, db.tables[tab].name);

    /* set the table description */
    db_set_table_description(*table, "");

/*
 *     if ( db.tables[tab].write )
 *       {	    
 *         db_set_table_delete_priv_granted (*table);
 *         db_set_table_insert_priv_granted (*table);
 *       } 
 *     else 
 *       {
 *         db_set_table_delete_priv_not_granted (*table);
 *         db_set_table_insert_priv_not_granted (*table);
 *       }
 */

    return DB_OK;
}
