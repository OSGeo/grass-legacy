/******************************************************************************
 * dbmi.c [v.out.shape]
 * Routines to transfer table records from database to dbf files
 * associated with shapefile export.

 * @Copyright Radim Blazek
 * 1st. Dec. 2001

 * This file is part of GRASS GIS. It is free software. You can 
 * redistribute it and/or modify it under the terms of 
 * the GNU General Public License as published by the Free Software
 * Foundation; either version 2 of the License, or (at your option)
 * any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 ******************************************************************************/

#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "dbmi.h"
#include "shapefil.h"
#include "local_structs.h"

int load_dbf_from_table( DBFHandle dbf, fieldDescriptor *hFD, char *table_name, char *key ) {

  /* Transfer data from table to a dbf file on disk */

  /* Local */
  int nrec;

  /* Loop */
  int i, j;
 
  int cat; 
  char  buf[2000];
  dbString stmt;
  dbDriver *driver;
  dbHandle handle;
  dbCursor cursor;
  dbTable *table;
  dbColumn *column;
  dbValue *value;
  dbString value_string;
  int col, ncols, sqltype, ctype, len;
  int more;
  char *col_name;

  driver = db_start_driver(NULL);
  if( driver == NULL ) {
      fprintf(stderr, "Unable to open DBMI driver.\n" );
      return -1;
  }

  db_init_handle (&handle);
  db_set_handle (&handle, NULL, NULL);
  if (db_open_database(driver, &handle) != DB_OK) {
      fprintf(stderr, "Unable to open database.\n" );
      return -1;
  }

  db_init_string (&stmt);
    
  /* Get the number of records */
  nrec = hFD[0].nRec;

  /* Loop through all the records and write out the fields */

  for( i = 0; i < nrec; ++i ) {
    cat  = hFD[1].fldRecs[i].intField;
    
    sprintf (buf, "select * from %s where %s = %d", table_name, key, cat); 
    db_set_string (&stmt, buf); 

    if (db_open_select_cursor(driver, &stmt, &cursor, DB_SEQUENTIAL) != DB_OK) {
        fprintf(stderr, "Unable to open cursor.\n" );
        return -1;
    } 
    table = db_get_cursor_table (&cursor);
    ncols = db_get_table_number_of_columns (table);

    if ( i == 0 ) {
        for(col = 0; col < ncols; col++) {
            column = db_get_table_column(table, col);
            col_name = db_get_column_name(column);
            sqltype = db_get_column_sqltype(column); 
            ctype   = db_sqltype_to_Ctype(sqltype);
  	    switch ( ctype ) {
                case DB_C_TYPE_INT:
	            if( DBFAddField(dbf, col_name, FTInteger, 10, 0) < 0 ) { 
                        fprintf(stderr, "Unable to create DBF integer field %s\n", col_name );
		        return -1;
	            }
	            break;
		case DB_C_TYPE_DOUBLE:
	            if( DBFAddField(dbf, col_name, FTDouble, 16, 6) < 0 ) { 
                        fprintf(stderr, "Unable to create DBF double field %s\n", col_name );
		        return -1;
	            }
	            break;
		case DB_C_TYPE_STRING:
		    len=db_get_column_length (column);
	            if( DBFAddField(dbf, col_name, FTString, len, 0) < 0 ) { 
                        fprintf(stderr, "Unable to create DBF string field %s\n", col_name );
		        return -1;
	            }
	            break;
	        default:
                    fprintf(stderr, "Unsupported type %s of column %s\n", db_sqltype_name(db_get_column_sqltype(column)), col_name); 
	            break;
	    }	    
        }
    }
    if(db_fetch (&cursor, DB_NEXT, &more) != DB_OK)
        return -1;
    
    db_init_string (&value_string); 
    for(col = 0; col < ncols; col++) {
        column = db_get_table_column(table, col);
        sqltype = db_get_column_sqltype(column); 
        ctype   = db_sqltype_to_Ctype(sqltype);
        value  = db_get_column_value(column);
        db_convert_column_value_to_string (column, &value_string);
        switch ( ctype ) {
            case DB_C_TYPE_INT:
                if( !DBFWriteIntegerAttribute(dbf, i, col, db_get_value_int(value))) {
                    fprintf(stderr, "Unable to write integer attribute to DBF. \n" );
		}
                break;
            case DB_C_TYPE_DOUBLE:
                if( !DBFWriteDoubleAttribute(dbf, i, col, db_get_value_double(value))) {
                    fprintf(stderr, "Unable to write double attribute to DBF. \n" );
		}
	        break;
	    case DB_C_TYPE_STRING:
                if( !DBFWriteStringAttribute(dbf, i, col, db_get_value_string(value))) {
                    fprintf(stderr, "Unable to write string attribute to DBF. \n" );
		}
	        break;
	    default:
	        /* Unsupported types */
	        break;
        }    
    }
 

  }
  db_close_database(driver);
  db_shutdown_driver(driver);

  return 0;
}

