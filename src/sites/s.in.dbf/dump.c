/*
 * 
 * Markus Neteler January 20 2001
 * write DBF table to GRASS sites file
 *
 ******************************************************************************
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
 * 1/2001 Removed PG stuff to make s.in.dbf from it  Markus Neteler
 * 12/2000 Federico Ponchio ponhio@dm.unipi.it (minor changes to memory 
 *  allocation for inserting in normal mode)
 * 02/2000 Alex Shevlakov sixote@yahoo.com	
 *			      
 ******************************************************************************/

/*#define DEBUG2*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "site.h"
#include "shapefil.h"

typedef unsigned char uchar;

/* ok... i guess someone invented C++ just for this...*/

struct my_string {
  char *data;
  int len;
  int totlen;
};

int init(struct my_string *str) {
  str->data = (char *)malloc(1024 *sizeof(char));
  if(str->data == NULL) {
    fprintf(stderr, "Failed to allocate new memory.\n");
    exit(-1);
  } 
  str->len = 1;
  str->totlen = 128;
}

int append(struct my_string *str, const char *s) {
  int newlen;
  newlen = strlen(s);
  str->len += newlen;
  if(str->len >= str->totlen) {
    str->totlen = str->len;
    str->data = (char *)realloc(str->data, sizeof(char) * str->len);
    if(str->data == NULL) {
      fprintf(stderr, "Failed to allocate new memory.\n");
      exit(-1);
    }
  }
  strcat(str->data, s);
}

int clear(struct my_string *str) {
  strcpy(str->data, "");
  str->len = 1;
}

int delete(struct my_string *str) {
  if(str->data != NULL) 
    free(str->data);
}


int DumpFromDBF (char *infile, char *outfile) {
	
	DBFHandle   hDBF;
	char buf[256]="";
	
	int i, m;
	char *pp;
	
	struct my_string SQL_create;
	struct my_string SQL_insert;
	struct my_string chunks;
	struct my_string headerline;
	struct my_string fldstrng;

	static char name[128]="";
	
	FILE *sites;
    	char *sitesname;

	sites = G_fopen_sites_new (outfile);
	if (sites == NULL)
	{
		fprintf (stderr, " Can't create sites file [%s]", outfile);
		exit(1);
	}

        /* generate some strings */
	init(&SQL_create);
	init(&SQL_insert);
	init(&chunks);
	init(&headerline);
	init(&fldstrng);

	append(&headerline, "#"); /* it will become a comment */
	
/* -------------------------------------------------------------------- */
/*      Extract basename of dbf file.                                   */
/* -------------------------------------------------------------------- */
    for( pp = infile+strlen(infile)-1;
         pp != infile-1 && (isalnum(*pp) || *pp == '_' || *pp == '.' );
         pp-- ) {}
    strcpy( name, pp+1);
    
    pp = strrchr( name, '.');
    if (pp != NULL)
        *pp = '\0';

	/* Open the dbf file */
	hDBF = NULL;
	hDBF = DBFOpen( infile, "r" );
	 
	if( hDBF == NULL )
        {
            sprintf (buf, "%s - DBF not found, or wrong format.\n", infile);
            G_fatal_error (buf);
        }

		
	for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
        {
	    char	field_name[128];
	    int		field_width;
	    char *fld;
	    

	    DBFFieldType ftype;

            ftype=DBFGetFieldInfo( hDBF, i, field_name, &field_width, NULL );

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
	append(&chunks, field_name);
	append(&chunks, " ");
	append(&chunks, fld);
	append(&chunks, ",");
	append(&headerline, field_name); /* keep this for sites header */
	append(&headerline, " ");        /* header delimiter */

	/*fldstrng - for insert stmt*/
	
	append(&fldstrng, field_name);
	append(&fldstrng, ",");
		
        } /* for */
	/*stripping last commas*/

	pp = strrchr(chunks.data, ',');
    	if (pp != NULL)
        	*pp = '\0';
	
	pp = strrchr(fldstrng.data, ',');
    	if (pp != NULL)
        	*pp = '\0';
	
	
	fprintf(stdout, "Writing to sites map %s...\n", outfile);

        /* Write Header to sites list*/
        fprintf(sites, "%s\n", headerline);
        fprintf(sites, "#\n");

        /* dump the fields */                                              
	DBFDumpASCII(hDBF, sites);
		
	fprintf(stdout,"\nTable %s successfully imported into %s.\n",infile, outfile);

	DBFClose( hDBF );

	fclose(sites);

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

/************************************************************************/
/*                          DBFDumpASCII()                          	*/
/*                                                                      */
/*     		 Dumps DBF to comma-separated list. 			*/
/************************************************************************/

 int DBFDumpASCII(DBFHandle psDBF, FILE *fp)

{
    int	       	nRecordOffset;
    uchar	*pabyRec;
    void	*pReturnField = NULL;
    int 	hEntity=0, iField=0;
    double      atof();

    static char * pszStringField = NULL;
    static int	nStringFieldLen = 0;
    static char single_line[4096]="";
    char buf[MAX_SITE_LEN], fbuf[MAX_SITE_STRING];
    int field_width;
    DBFFieldType ftype;

  for ( hEntity=0; hEntity < psDBF->nRecords; hEntity++) {
  
  	single_line[0]='\0';
	
	nRecordOffset = psDBF->nRecordLength * hEntity + psDBF->nHeaderLength;

	fseek( psDBF->fp, nRecordOffset, 0 );
	fread( psDBF->pszCurrentRecord, psDBF->nRecordLength, 1, psDBF->fp );
	psDBF->nCurrentRecord = hEntity;
	pabyRec = (uchar *) psDBF->pszCurrentRecord;
	
    for ( iField=0; iField < psDBF->nFields; iField++)
    {	
    	char tmp_buf[1024]="";
	
	
	
/* -------------------------------------------------------------------- */
/*	Ensure our field buffer is large enough to hold this buffer.	*/
/* -------------------------------------------------------------------- */
    if( psDBF->panFieldSize[iField]+1 > nStringFieldLen )
    {
	nStringFieldLen = psDBF->panFieldSize[iField]*2 + 10;
	pszStringField = (char *) SfRealloc(pszStringField,nStringFieldLen);
     }

/* -------------------------------------------------------------------- */
/*	Extract the requested field.					*/
/* -------------------------------------------------------------------- */
    strncpy( pszStringField, pabyRec+psDBF->panFieldOffset[iField],
	     psDBF->panFieldSize[iField] );
    pszStringField[psDBF->panFieldSize[iField]] = '\0';

/* Convert float numbers to non-scientifically (no exponents)
   and add site list related characters*/
	ftype=DBFGetFieldInfo( psDBF, iField, buf, &field_width, NULL );
	switch (ftype) {
	case 0: /*text*/
		sprintf(fbuf,"@\"%s\"", G_squeeze((char *)G_chop(pszStringField)));
		sprintf(pszStringField, "%s", fbuf);
		if (strlen(pszStringField) < 4 )
		{
		   /*prevent crash of sites modules with empty string @"": 
		     add space */
		   sprintf(pszStringField, "@\" \"");
		}
		break;
	case 1: /*int*/
		/*treat as float */
		if (iField < 2)
			sprintf(fbuf,"%s", G_squeeze(pszStringField));
		else
		{
			sprintf(fbuf,"%%%s ", G_squeeze(pszStringField));
		}
		sprintf(pszStringField,fbuf);
		break;
	case 2: /* float */
		if (iField < 2)
			sprintf(fbuf,"%f", atof(pszStringField));
		else
			sprintf(fbuf,"%%%f ", atof(pszStringField));
		sprintf(pszStringField,fbuf);
		break;
	case 3: /* error */
		break;
	}
	
    	/*Remove white spaces if any*/
#ifdef TRIM_DBF_WHITESPACE
    	if (1)
    	{
        	char	*pchSrc, *pchDst;

        	pchDst = pchSrc = pszStringField;
        	while( *pchSrc == ' ' )
            		pchSrc++;

        	while( *pchSrc != '\0' )
            		*(pchDst++) = *(pchSrc++);
        	*pchDst = '\0';

        	while( *(--pchDst) == ' ' && pchDst != pszStringField )
            		*pchDst = '\0';

    	}
#endif
        /* insert re-ordering here: get field order from command line */
	if (!iField)
		snprintf(tmp_buf,1024,"%s",pszStringField);
	else if (iField == psDBF->nFields-1)
       		snprintf(tmp_buf,1024," %s\n",pszStringField);
	else
	{
	 if (iField > 2) /* coordinates */
    		snprintf(tmp_buf,1024," %s",pszStringField);
    	else             /* something else */
    		snprintf(tmp_buf,1024,"|%s",pszStringField);
	}
	
	/* construct singe line : */
	strncat(single_line,tmp_buf,strlen(tmp_buf));
    }

	fwrite( single_line, strlen(single_line), 1, fp );
	
	if (ferror(fp)) {
		fprintf(stderr,"Error occurred while writing to sites file!\n");
		fclose(fp);
		exit(-1);
	}
  }

    return 0;
}
