/******************************************************************************
 * Copyright (c) 1998, Frank Warmerdam
 *
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************
 *
 * 02/2000 Alex Shevlakov sixote@yahoo.com	
 *			      
 ******************************************************************************
 ******************************************************************************
 *
 * 03/2000 with modifications by DD Gray  ddgray@armadce.demon.co.uk
 *         Information is now loaded from field descriptors.
 *			      
 ******************************************************************************/
 
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "shapefil.h"
#include "shp2dig.h"
#include "pg_local.h"
#include <libpq-fe.h>

typedef unsigned char uchar;


int PgDumpFromFieldD( const fieldDescript *fd1, const int nfields, 
		      const char *table_name, const unsigned char dump_flags ) {

	char buf[256]="";
	
	int i,j;
	char *dbname, *pp;
	
	static char SQL_create [1024]="";
	static char SQL_insert [4096]="";
	static char name[128]="";
	static char chunks[1024]="";
	static char fldstrng[1024]="";
	
	PGconn*	pg_conn;
    	PGresult*	res;
	char	*pghost;
	
	FILE *fp;
    	char *tmpfile_nm; 

	int dump_coords, dump_orig, dump_id;

	/* Do we include special fields? */

	dump_coords = (int)(dump_flags & 4);
	dump_orig = (int)(dump_flags & 2);
	dump_id = (int)(dump_flags & 1);

	
	/* Check DATABASE env variable */
        if ((dbname=G__getenv("PG_DBASE")) == NULL) {
            fprintf(stderr,
                  "Please run g.select.pg to identify a current database.\n");
	    exit(-1);
        }

	/* Should we include the additional fields (1-4) here? */
		
	for( i = 0; i < nfields + 4; i++ )
        {
            char	field_name[15];
	    int		field_width, k;
	    char 	c_tmpbuf[128];
	    char *fld;
	    
	    
	    DBFFieldType ftype;

	    if( (i == 2 || i == 3) && !dump_coords ) continue;
	    if( i == 0 && !dump_id ) continue;
	    if( i == 1 && !dump_orig ) continue;



            ftype=fd1[i].fldType;
	    field_width = fd1[i].fldSize;
	    strncpy( field_name, fd1[i].fldName, 12 );

	switch (ftype) {
		case 0:
			fld="text";
		break;
		case 1:
			if (field_width<=7) fld="int4";
				else fld="int8";
		break;
		case 2:
			fld="float4";
		break;
		case 3:
            		G_fatal_error ("Invalid field type - bailing out");
		break;
	}
	
	/*chunks -for create stmt*/	
	snprintf(c_tmpbuf,128,"%s %s,",field_name,fld);
		strncat(chunks,c_tmpbuf,strlen(c_tmpbuf));
		
	/*fldstrng - for insert stmt*/
	snprintf(c_tmpbuf,128,"%s,",field_name);
		strncat(fldstrng,c_tmpbuf,strlen(c_tmpbuf));
		
        }
	/*stripping last commas*/
	pp = strrchr(chunks, ',');
    	if (pp != NULL)
        	*pp = '\0';
	
	pp = strrchr(fldstrng, ',');
    	if (pp != NULL)
        	*pp = '\0';

	snprintf(SQL_create,1024,"create table %s (%s)",table_name, chunks);
	
	pghost = G__getenv("PG_HOST");
        
    	pg_conn = PQsetdb(pghost,NULL, NULL,NULL,G_getenv("PG_DBASE"));
    	if (PQstatus (pg_conn) == CONNECTION_BAD) {
     		printf ("Error Quering Postgres:%s\n",PQerrorMessage(pg_conn));
      		PQfinish(pg_conn);
      		exit (-1); 
    	}
  	fprintf(stdout,"Executing %s\n",SQL_create);      
   	res = PQexec (pg_conn, SQL_create);
	
		if (strlen(PQresultErrorMessage(res))){
			fprintf(stdout,"FIXME: Postgres Says:\n**********************\n%s\nPlease make sure that created table name is not used by another table.\n", PQresultErrorMessage(res));
		PQclear(res);
		PQfinish(pg_conn);
		/* DBFClose( hDBF ); */
		exit(-1);
		}
		
	PQclear(res);
    /* explicitly close select result to avoid memory leaks  */ 

	
	/*Loop over records*/
   for( i = 0; i < fd1[0].nRec; i++ ) {
   
   char valstrng[1024]="";

   /* Again: do we want to dump the special fields? */
		
	for( j = 0; j < nfields + 4; j++ ) {
	
            char	field_name[15];
	    char 	c_tmpbuf[128];
	    char fld[128];
	    
	    DBFFieldType ftype;

            ftype=fd1[j].fldType;

	    if( (j == 2 || j == 3) && !dump_coords ) continue;
	    if( j == 0 && !dump_id ) continue;
	    if( j == 1 && !dump_orig ) continue;

	  switch (ftype) {
		case 0:
			snprintf(fld,128,"'%s'",fd1[j].fldRecs[i].stringField);
		break;
		case 1:
			snprintf(fld,128,"%d",fd1[j].fldRecs[i].intField);

		break;
		case 2:
			snprintf(fld,128,"%f",fd1[j].fldRecs[i].doubleField);

		break;
		case 3:
		  /* Fields not as above should have been converted to an int
		     holder (for future compatibility), with all records set to
		     0. We shouldn't get here.
		  */
            		G_fatal_error ("Invalid field type - bailing out");
		break;
	  }
	/*valstrng -for insert stmt*/
	snprintf(c_tmpbuf,128,"%s,",fld);
		strncat(valstrng,c_tmpbuf,strlen(c_tmpbuf));
	}
	
		pp = strrchr(valstrng, ',');
    		if (pp != NULL)
        		*pp = '\0';
			
		snprintf(SQL_insert,4096,"insert into %s (%s) values (%s)",table_name, 
			fldstrng,valstrng);
			
		fprintf(stdout,"Executing %s\n",SQL_insert);
		
		res = PQexec (pg_conn, SQL_insert);
		/*explicitly close select result to avoid memory leaks*/  
		PQclear(res);
   }
	fprintf(stdout,"\nSuccessfully inserted %d records to Postgres table %s\n",
		fd1[0].nRec,name);   


    	PQfinish(pg_conn);	
	
	return 0;
}

/************************************************************************/
/*                             SfRealloc()                              */
/*                                                                      */
/*      A realloc cover function that will access a NULL pointer as     */
/*      a valid input.                                                  */
/************************************************************************/

static void * SfRealloc( void * pMem, int nNewSize )

{
    if( pMem == NULL )
        return( (void *) malloc(nNewSize) );
    else
        return( (void *) realloc(pMem,nNewSize) );
}

