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

/* uncomment to get some debug output */
/*#define DEBUG2*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "gis.h"
#include "site.h"
#include "shapefil.h"

typedef unsigned char uchar;

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


int DumpFromDBF (char *infile, char *outfile, char *timestamp, int third) {
	
	DBFHandle   hDBF;
	char buf[256]="";
	
	int i, m;
	char *pp;
	int dummy;
	
	struct my_string SQL_create;
	struct my_string SQL_insert;
	struct my_string chunks;
	struct my_string headerN, headerD, headerT;
	struct my_string fldstrng;

	static char name[128]="";
	char  fname[15];
	int k, dimension;

	FILE *sites;
    	char *sitesname;

	sites = G_fopen_sites_new (outfile);
	if (sites == NULL)
	{
		fprintf (stderr, " Can't create sites file [%s]", outfile);
		exit(1);
	}

	/* check if third column shall be used as third dimension coordinate */
	if (third)
		dimension=3;
	else
		dimension=2;

        /* generate some strings */
	init(&SQL_create);
	init(&SQL_insert);
	init(&chunks);
	init(&headerN); /*name*/
	init(&headerD); /*desc*/
	init(&headerT); /*time*/
	init(&fldstrng);

	append(&headerN, "name|");
	append(&headerD, "desc|");
	append(&headerT, "time|");

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
	append(&headerN, outfile);
	
	append(&headerT, timestamp);
	
	/* reorder header: first int and float as it comes */
	dummy=1-dimension; /* don't count the dimension fields */
        for( k = 0; k < DBFGetFieldCount(hDBF); k++ )
        {
          DBFFieldType ftype;
	  ftype=DBFGetFieldInfo( hDBF, k, fname, NULL, NULL );
	  if (ftype != 0) /* no text */
	  {
	    if (dummy > 0) /* don't number coordinate fields */
	    {
	    	sprintf(buf, "%i:", dummy); /*enumber the fields */
	    	append(&headerD, buf);
	    }
	    append(&headerD, fname );
	    append(&headerD, " " );
	    dummy=dummy + 1;
	  }
        }

	/* reorder header: second strings */
        for( k = 0; k < DBFGetFieldCount(hDBF); k++ )
        {
          DBFFieldType ftype;
	  ftype=DBFGetFieldInfo( hDBF, k, fname, NULL, NULL );
	  if (ftype == 0) /* text */
	  {
	    if (dummy > 0) /* don't number coordinate fields */
	    {
	    	sprintf(buf, "%i:", dummy); /*enumber the fields */
	    	append(&headerD, buf);
	    }
	    append(&headerD, fname );
	    append(&headerD, " " );
	    dummy=dummy + 1;
	  }
        }

        /* Write Header to sites list - should be G_sites routines*/
        fprintf(sites, "%s\n", headerN);
        fprintf(sites, "%s\n", headerD);
        /* fprintf(sites, "%s\n", headerT); */ /* not yet implemented */

        /* dump the fields */

	DBFDumpASCII(hDBF, sites, dimension);
	
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

 int DBFDumpASCII(DBFHandle psDBF, FILE *fp, int dimension)

{
    int	       	nRecordOffset;
    uchar	*pabyRec;
    void	*pReturnField = NULL;
    int 	hEntity=0, iField=0;
    double      atof();
    int         atoi();

    static char * pszStringField = NULL;
    static int	nStringFieldLen = 0;
    static char single_line[4096]="";
    char interim_lineS[4096]="";
    char interim_lineN[4096]="";
    char buf[MAX_SITE_LEN], fbuf[MAX_SITE_STRING];
    int field_width;
    DBFFieldType ftype;
    int done_fields; /*count fields already done */


  /* To achive the special sites column order wanted by GRASS we have
          to scan the dbase file:
             1. int fields and double fields
             2. strings fields
   */

  /* run 1 with search for INT */ 
  for ( hEntity=0; hEntity < psDBF->nRecords; hEntity++) {

        single_line[0]='\0';  
  	interim_lineN[0]='\0';
	interim_lineS[0]='\0';
	fbuf[0]='0';
	fbuf[1]='\0';
		
	nRecordOffset = psDBF->nRecordLength * hEntity + psDBF->nHeaderLength;

	fseek( psDBF->fp, nRecordOffset, 0 );
	fread( psDBF->pszCurrentRecord, psDBF->nRecordLength, 1, psDBF->fp );
	psDBF->nCurrentRecord = hEntity;
	pabyRec = (uchar *) psDBF->pszCurrentRecord;

    /* first scan for int and float */	
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
#ifdef DEBUG2
 if (iField == 8)
   fprintf(stderr, "A: %i-line %i: %s\n",iField, hEntity, pszStringField);
#endif 
    ftype=DBFGetFieldInfo( psDBF, iField, buf, &field_width, NULL );
    switch (ftype) {
	case 0: /*text*/
		/*ignore, do later */
		break;
	case 1: /*int*/
		if (iField < dimension) /* treat coordinate columns differently */
			sprintf(fbuf,"%s", pszStringField);
		else
			sprintf(fbuf,"%%%s ", pszStringField);
		sprintf(pszStringField, fbuf);
		if(strlen(pszStringField) == 0)  /* this is crazy, but I don't know how to fix MN */
			sprintf(pszStringField,"%%0 ");
#ifdef DEBUG2
 if (iField == 8)
  fprintf(stderr, "B: %i-line %i: %s\n",iField, hEntity, pszStringField);
#endif
		break;
	case 2: /* float */
		if (iField < dimension) /* treat coordinate columns differently */
			sprintf(fbuf,"%f", atof(pszStringField));
		else
			sprintf(fbuf,"%%%f ", atof(pszStringField));
		sprintf(pszStringField,fbuf);
		break;
	case 3: /* error */
		break;
	} /* switch */
#ifdef DEBUG2
 if (iField == 8)
  fprintf(stderr, "C: %i-line %i: %s\n",iField, hEntity, pszStringField);
#endif

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
        if (ftype != 0) /* no text */
        {
	   if (!iField)
		snprintf(tmp_buf,1024,"%s",pszStringField);
	   else if (iField == psDBF->nFields-1)
       		snprintf(tmp_buf,1024," %s\n",pszStringField);
	   else
	   {
	     if (iField > dimension) /* coordinates */
    		snprintf(tmp_buf,1024," %s",pszStringField);
     	     else             /* something else */
    		snprintf(tmp_buf,1024,"|%s",pszStringField);
	   }
	
	   /* construct single line, ignore text : */
	   strncat(interim_lineN,tmp_buf,strlen(tmp_buf));
	   
#ifdef DEBUG
 fprintf(stderr, "%i-lineN: %s\n",iField, interim_lineN);
#endif

	} /* if */
    } /* for iField col-wise: float/int scan*/



   /*-------------------*/
    /* second scan strings */
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
	        /*ignore, already done */
		break;
	case 2: /* float */
		/*ignore, already done */
		break;
	case 3: /* error */
		break;
	} /* switch */

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
        if (ftype == 0) /* do only text */
        {
	  if (!iField)
		snprintf(tmp_buf,1024,"%s",pszStringField);
	  else if (iField == psDBF->nFields-1)
       		snprintf(tmp_buf,1024," %s\n",pszStringField);
	  else
	  {
	   if (iField > dimension) /* coordinates */
    		snprintf(tmp_buf,1024," %s",pszStringField);
    	   else             /* something else */
    		snprintf(tmp_buf,1024,"|%s",pszStringField);
	  }
	
	  /* construct single line : */
	  strncat(interim_lineS,tmp_buf,strlen(tmp_buf));
	  
#ifdef DEBUG
 fprintf(stderr, "%i-lineS: %s\n",iField, interim_lineS);
#endif
 	}
    } /* for iField col-wise: float/int scan*/      

   strncat(single_line,interim_lineN,strlen(interim_lineN));
   strncat(single_line,interim_lineS,strlen(interim_lineS));
   
   /* write out the fully composed line to sites file*/
   /*fwrite( single_line, strlen(single_line), 1, fp );*/
   fprintf(fp, "%s\n", G_squeeze(single_line));
	
   if (ferror(fp)) {
		fprintf(stderr,"Error occurred while writing to sites file!\n");
		fclose(fp);
		exit(-1);
    } 
  } /* for hEntity: row-wise*/


    return 0;
}
