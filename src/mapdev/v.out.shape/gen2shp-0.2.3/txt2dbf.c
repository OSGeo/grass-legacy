/* Frank Koormann	$Date$
 * $Id$
 *
 * Copyright (C) 2000 by Frank Koormann
 * 
 *   This program is free software; you can redistribute it and/or
 *   modify it under the terms of the GNU General Public License
 *   as published by the Free Software Foundation; either version 2
 *   of the License, or (at your option) any later version.
 *
 *   This program is distributed in the hope that it will be useful,
 *   but WITHOUT ANY WARRANTY; without even the implied warranty of
 *   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *   GNU General Public License for more details.
 *
 *   You should have received a copy of the GNU General Public License
 *   along with this program; if not, write to the Free Software
 *   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
 *
 * Description:
 *	Converting character-delimited ASCII-Tables in the dbase-III-Format.
 *	The delimiter can be specified by the -d option.
 *	It is necessary to describe the structure of the table on the 
 *	commandline:
 *		-Cn	text, n characters
 *		-In	integer, with n digits
 *		-Rn.d	real, with n digits (including '.') and d decimals
 *
 *	To handle with *dbf-files, this program uses the functions and
 *	datastructures of the shapelib by Frank Warmerdam, released under
 *	LGPL.
 *
 */
 
#include <stdio.h>
#include <fcntl.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#include "../../../libes/vect32_64/shapelib-1.2.8/shapefil.h"
#include "utils.h"

/* program information -------------------------------------------------*/
char *progname;
char usage[] = "[{-Cn | -In | -Rn.d}] [-d delimiter] [-v] txt-file dbf-file";
char version[] = "txt2dbf 1.1, GRASS-version 22.05.2000  by Frank Koormann";

/* type definition -----------------------------------------------------*/
/* content of record as list of char-vectors 				*/
typedef char **tRecord;

/* global variables ----------------------------------------------------*/

/* Fielddescription: type, length, decimals
   The structure is described by a list of fielddescriptions 		*/
typedef struct {
		DBFFieldType	type;
		int		n,d;
		} tFieldDescr;
	
/* empty string noData. If a field in a record is empty (or missed), the
 * concerning pointer is directed to noData to avoid errors while
 * coping NULL pointers with strcpy					*/
char noData[] = "";

/* function declaration ------------------------------------------------*/

/* add field to the list of fielddesriptions 				*/
tFieldDescr *tabAddField(DBFFieldType type, 
			 char *format, 
			 tFieldDescr *ptr, 
			 int num );

/* read line from the ASCII-table and write it divided by delim in the 
 * record description, delim should contain ONE delimiter !		*/
int readRecord(FILE *fp, tRecord rec, int fields, char * delim );


/* main ----------------------------------------------------------------*/
int main( int argc, char *argv[] )
{	
	/* common */
	char *dummy;
	char *delim = "\t";
	int i;
	int v=0;	/* verbose? 0: no, 1: yes  			*/ 
	int cnt;	/* Counter for written Records 			*/

	/* getopt() - related */
	int c;		
        extern char *optarg;
        extern int optind;
	/* .txt */
	char *txtname = NULL;
	tRecord record;

	tFieldDescr *tabDescr = NULL;
	DBFFieldType fType;
	int numFields = 0;

	/* .dbf */
	DBFHandle dbh;
	int  	  rec;
	char      *dbfname = NULL;

	/* Get program name and eval commandline			*/
	progname = *argv;	
	while ((c = getopt(argc, argv, "+C:I:R:d:v")) != -1) {
		fprintf(stderr,"Opt is -%c, Arg is: %s\n",c,optarg);
		switch (c) {
			case 'C' : 
				fType = FTString;
				tabDescr = 
				    tabAddField(
					fType,
					strdup(optarg),tabDescr,++numFields);
				break;
			case 'I' : 
				fType = FTInteger;
				tabDescr = 
				    tabAddField(
					fType,
					strdup(optarg),tabDescr,++numFields);
				break;
			case 'R' : 	
				fType = FTDouble;
				tabDescr =
				    tabAddField(
					fType,
					strdup(optarg),tabDescr,++numFields);
				break;
			case 'd' : 
				delim = strdup(optarg);
				break;
			case 'v' :
				fprintf(stderr,"%s\n",version);
				v=1;
				break;
		}
	}

	/* after processing the commandline, argv should contain the two 
         * filenames ... else error and abort				*/
	if (optind < argc-1)
           {
		txtname = argv[optind];
		dbfname = argv[optind+1];
	   }
		
		fprintf(stderr," %s --> %s\n",txtname,dbfname);
	if ( txtname==NULL || dbfname==NULL )
	{	fprintf(stderr,"Usage: %s %s\n",progname, usage);
                exit(1);
	}

	if (v)
		fprintf(stderr," %s --> %s\n",txtname,dbfname);

	/* alloc mem for the recordhead (numFields vectors of char) 	*/
	if ((record = (tRecord ) calloc(numFields, sizeof(char *))) == NULL)
		perror(progname), exit(1);  
 
	/* alloc mem for the vectors (with constant size)    	*/
	for (i=0; i<numFields; i++) {
		if ((record[i] = (char *) calloc(255, sizeof(char)))
			== NULL )
			perror(progname), exit(1);
	}
	
	/* open the txt-file */
	if ( (strcmp(txtname, "-") != 0) && (!freopen(txtname, "r", stdin)) ) 
		perror(progname), exit(1);

	/* read the first line (contains fieldnames)			*/
	if ( readRecord(stdin, record, numFields, delim) == EOF )
	{	fprintf(stderr,"%s: %s: empty file\n",progname,txtname);
		exit(1);
	}

	/* cut of '#' out of first fieldname (the '#' declares the line for
 	 * some programs, i.e. gnuplot, as comment)			*/
	if ( record[0][0] == '#' )
	{	dummy = record[0];
		dummy++;
		strcpy(record[0], dummy);
	}
	
	/* create new dbase-file					*/
	dbh = DBFCreate(dbfname);

	if (v)
		fprintf(stderr," FIELDS:\n NAME\t\tTYPE\tLENGTH\tDECIMALS\n");
		
	/* add fields to dbf						*/
	for (i=0; i<numFields; i++) {
		DBFAddField(dbh, record[i], tabDescr[i].type, 
			tabDescr[i].n, tabDescr[i].d);
		if (v)
			fprintf(stderr," %-12s\t%c\t%d\t%d\n",
				record[i], tabDescr[i].type,
				tabDescr[i].n, tabDescr[i].d);
	}	

	/* write records into dbf					*/
	if (v)
	{	cnt = 0;
		fprintf(stderr," RECORDS:\n");
	}

	rec = 0;
	while ( readRecord(stdin, record, numFields, delim ) != EOF )
	{
	    for (i=0; i< numFields; i++) {
		if (strlen(record[i]) > tabDescr[i].n) {
		    if (v){
			fprintf(stderr, " ignoring %s (too long) ", record[i]);
				strcpy(record[i], noData);
		    } 
		}
		switch (tabDescr[i].type) {
		case FTString  : 
				if (!DBFWriteStringAttribute(
						dbh, 
						rec, 
						i, 
						record[i])){
					fprintf(stderr,
						"Field writing failed!\n");
				 	exit(2);
				 }
				 break;
					 
		case FTInteger	: 
				 if (!DBFWriteIntegerAttribute(
						dbh, 
						rec, 
						i, 
						atoi(record[i]) )){
					fprintf(stderr,
						"Field writing failed!\n");
				 	exit(2);
				 }
				 break;
					 
		case FTDouble : 
				 if (!DBFWriteDoubleAttribute(
						dbh, 
						rec, 
						i, 
						atof(record[i]))){
					fprintf(stderr,
						"Field writing failed!\n");
				 	exit(2);
				 }
				 break;
		case FTInvalid : break;	
		}
	    }
	    rec++;

	    if (v)
		fprintf(stderr," [%d]",++cnt);

	}

	/* rewrite head (num of records known yet)			*/

	DBFClose(dbh);

	if (v)
		fprintf(stderr,"\n %d Records written into %s\n",cnt, dbfname);
	return 0;
}

/* tabAddField ---------------------------------------------------------*/
/* Add field to the list of fielddescriptions				*/
tFieldDescr *tabAddField( 
		DBFFieldType type, 
		char *format, 
		tFieldDescr *ptr, 
		int num )
{	int new;

	if (( ptr = (tFieldDescr *)realloc(ptr, num*sizeof(tFieldDescr)))==NULL) 
	{	perror(progname);
		exit(1);
	}
	
	new = num-1;

	switch ( type ) {
		case FTString : ptr[new].type = FTString;
			   	ptr[new].n    = atoi(format);
				ptr[new].d    = 0;
			   break;
		case FTInteger: ptr[new].type = FTInteger;
			   	ptr[new].n    = atoi(format);
				ptr[new].d    = 0;
			   break;
		case FTDouble :	ptr[new].type = FTDouble;
				ptr[new].n    = atoi(strtok(format,".")); 
				ptr[new].d    = atoi(strtok(NULL,"."));
			   break;
	} 
	
	return ptr;
}

/* readRecord ----------------------------------------------------------*/
/* read line from the ASCII-table and write it divided in the 
 * record description							*/
int readRecord(FILE *fp, tRecord rec, int fields, char * delim )
{ 	char line[255];
	char *dummy;
	int ret,i;

	ret = getline(fp, line);

	if ( ret != EOF )
	{	strcpy(rec[0], dtok(line, delim[0] ));
		for (i=1; i<fields; i++)	
			if ((dummy = dtok(NULL, delim[0] )) != NULL)
				strcpy(rec[i], dummy);
			else
			{	strcpy(rec[i], noData);
				fprintf(stderr," Missing field %d:",i);
			}
	}

	return ret;
}

