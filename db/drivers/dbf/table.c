/*****************************************************************************
*
* MODULE:       DBF driver 
*   	    	
* AUTHOR(S):    Radim Blazek
*
* PURPOSE:      Simple driver for reading and writing dbf files     
*
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <dirent.h>
#include <gis.h>
#include <dbmi.h>
#include <shapefil.h>
#include "globals.h"
#include "proto.h" 

/* add table to database */
int add_table (char *table)
{
    if ( db.atables == db.ntables )
      {
        db.atables += 15; 
	db.tables = (TABLE *) realloc ( db.tables, db.atables * sizeof (TABLE) ); 
      }

    
    strcpy ( db.tables[db.ntables].name, table );
    
    sprintf ( db.tables[db.ntables].file, "%s/%s.dbf", db.name, table );
    
    db.tables[db.ntables].alive = TRUE;
    db.tables[db.ntables].described = FALSE;
    db.tables[db.ntables].loaded = FALSE;
    db.tables[db.ntables].updated = FALSE;
    db.tables[db.ntables].cols = NULL;
    db.tables[db.ntables].rows = NULL;
    db.tables[db.ntables].acols = 0;
    db.tables[db.ntables].ncols = 0;
    db.tables[db.ntables].arows = 0;
    db.tables[db.ntables].nrows = 0;

    db.ntables++ ;
    
    return DB_OK;
}


/* returns table index or -1 */
int find_table (char *table)
{
    int i;
	
    for ( i = 0; i < db.ntables; i++ )
      {
         if ( strcmp( db.tables[i].name, table ) == 0 )
	     return (i);   
      }
    
    return (-1);
}

int
load_table_head( int t)
{
    int  i, ncol, dtype, type, width, decimals;
    DBFHandle   dbf;
    char fname[20];
    FILE *fd;

    if ( db.tables[t].described == TRUE ) /*already described */
        return DB_OK;
     
    /* test R/W rights */
    fd = fopen ( db.tables[t].file, "rb" );
    if ( fd != NULL )
      {
        db.tables[t].read = TRUE;
	fclose ( fd );
      }
    else
      {
        db.tables[t].read = FALSE;
      }
    
    fd = fopen ( db.tables[t].file, "ab" );
    if ( fd != NULL )
      {
        db.tables[t].write = TRUE;
	fclose ( fd );
      }
    else
      {
        db.tables[t].write = FALSE;
      }
    
    /* load */
    dbf = DBFOpen( db.tables[t].file, "r" );
    if( dbf == NULL )
        return DB_FAILED;

    ncol = DBFGetFieldCount(dbf);

    if ( drv_mode == DBF_MODE_SHP ) {
	add_column ( t, DBF_INT, DBF_FID_NAME, 11, 0);
    }
    
    for( i = 0; i < ncol; i++ )
      {
         dtype = DBFGetFieldInfo( dbf, i, fname, &width, &decimals );

	 switch ( dtype )
	   {
             case FTInteger:
		 type = DBF_INT;    
                 break;
             case FTString:
		 type = DBF_CHAR;    
                 break;
             case FTDouble:
		 type = DBF_DOUBLE;    
                 break;
	   }
	 
	 add_column ( t, type, fname, width, decimals);  
      }
    
    DBFClose ( dbf );
    db.tables[t].described = TRUE;
	
    return DB_OK;
}

int
load_table ( int t)
{
    int  i, j, ncols, nrows, dbfcol;
    DBFHandle   dbf;
    char *buf;
    ROW  *rows;
    VALUE *val;

    if ( db.tables[t].loaded == TRUE ) /*already loaded */
        return DB_OK;
    
    dbf = DBFOpen( db.tables[t].file, "r" );
    if( dbf == NULL )
        return DB_FAILED;

    ncols = db.tables[t].ncols;
    nrows = DBFGetRecordCount( dbf );
    rows = db.tables[t].rows;
    rows = (ROW *) malloc ( nrows * sizeof(ROW) );
    db.tables[t].arows = nrows;
    
    for( i = 0; i < nrows; i++ )
      {
         rows[i].alive = TRUE;
         rows[i].values = (VALUE *) calloc ( ncols, sizeof (VALUE) );

         for( j = 0; j < ncols; j++ )
           {
             val = &(rows[i].values[j]);		   
	     
             if ( drv_mode == DBF_MODE_SHP ) {
                 if ( j == 0 ) {
		     val->i = i + 1; 
		     continue;
		 } else {
		     dbfcol = j - 1;
		 }
	     } else {
		 dbfcol = j;
	     }
	     switch ( db.tables[t].cols[j].type )
	       {
                 case DBF_INT:    
                     val->i = DBFReadIntegerAttribute( dbf, i, dbfcol );
                     break;
                 case DBF_CHAR:    
                     buf = (char *) DBFReadStringAttribute( dbf, i, dbfcol );
		     save_string ( val, buf);
                     break;
                 case DBF_DOUBLE:    
                     val->d = DBFReadDoubleAttribute( dbf, i, dbfcol );
                     break;
               }
             
           }
      }

    DBFClose ( dbf );
    
    db.tables[t].rows = rows;
    db.tables[t].nrows = nrows;
    db.tables[t].loaded = TRUE;
    
    return DB_OK;
}

int
save_table ( int t)
{
    int  i, j, ncols, nrows, ret;
    DBFHandle   dbf;
    ROW  *rows;
    VALUE *val;
    int  dbftype, width, decimals;

    if ( !(db.tables[t].alive) || !(db.tables[t].updated) )
        return DB_OK;
    
    dbf = DBFCreate( db.tables[t].file );
    if( dbf == NULL )
        return DB_FAILED;

    ncols = db.tables[t].ncols;
    rows = db.tables[t].rows;
    nrows = db.tables[t].nrows;

    for( i = 0; i < ncols; i++ )
      {
	switch ( db.tables[t].cols[i].type )
          {
            case DBF_INT:
		dbftype = FTInteger;
		break;
            case DBF_CHAR:
		dbftype = FTString;
		break;
            case DBF_DOUBLE:
		dbftype = FTDouble;
		break;
	  }
	      
        width = db.tables[t].cols[i].width;
	decimals = db.tables[t].cols[i].decimals;
        DBFAddField( dbf, db.tables[t].cols[i].name, dbftype, width, decimals );

      }
    
    for( i = 0; i < nrows; i++ )
      {
         if ( rows[i].alive == FALSE ) continue;
		 
         for( j = 0; j < ncols; j++ )
           {
             val = &(rows[i].values[j]);		   
	     switch ( db.tables[t].cols[j].type )
	       {
                 case DBF_INT:    
		     ret = DBFWriteIntegerAttribute( dbf, i, j, val->i ); 
                     break;
                 case DBF_CHAR:    
		     if ( val->c != NULL )
		         ret = DBFWriteStringAttribute( dbf, i, j, val->c ); 
		     else
		         ret = DBFWriteStringAttribute( dbf, i, j, "" ); 
                     break;
                 case DBF_DOUBLE:    
		     ret = DBFWriteDoubleAttribute( dbf, i, j, val->d ); 
                     break;
               }
           }
      }

   DBFClose ( dbf );
    
    return DB_OK;
}

int free_table (int tab)
{
    int i,j;

    for ( i = 0; i < db.tables[tab].nrows; i++ )
      {
	for( j = 0; j < db.tables[tab].ncols; j++ )
	  {
            if ( db.tables[tab].cols[j].type == DBF_CHAR )
	      {	    
                free ( db.tables[tab].rows[i].values[j].c );
	      }
	  }
        free ( db.tables[tab].rows[i].values );
      }
    
    free ( db.tables[tab].rows );
	      
    return DB_OK;
}


