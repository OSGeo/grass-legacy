/***********************************************************************************/
/***                                  db_head()					 ***/
/***  	Reads in the Database Header Record (02) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

db_head()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  DBHDREC	dbhdrec;	/* Database header structure.			*/
    char    		buffer[80];	/* Temporary storage of text data 		*/

    /* Initialise all string structures */

    strcpy(dbhdrec.DBNAME,	"                    ");
    strcpy(dbhdrec.DDNAME,	"                    ");
    strcpy(dbhdrec.DDATE,	"        ");
    strcpy(dbhdrec.DDBASE,	"                    ");
    strcpy(dbhdrec.DDBDATE,	"        ");
    strcpy(dbhdrec.FCNAME,	"                    ");
    strcpy(dbhdrec.FCDATE,	"        ");
    strcpy(dbhdrec.DQNAME,	"                    ");
    strcpy(dbhdrec.DQDATE,	"        ");


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    dbhdrec.REC_DESC_1	= 	2;
    strncpy(dbhdrec.DBNAME,	text+2,20);
    strncpy(dbhdrec.DDNAME,	text+22,20);
    strncpy(dbhdrec.DDATE,	text+42,8);
    strncpy(dbhdrec.DDBASE,	text+50,20);
    strncpy(dbhdrec.DDBDATE,	text+70,8);
    dbhdrec.CONT_MARK_1 =	text[78] - '0';
    dbhdrec.EOR_1 =		text[79];

    if (dbhdrec.CONT_MARK_1 == 1)
    {
	if ( (dbhdrec.REC_DESC_2 = read_line()) != 0)
	    fprintf(stderr,
   "WARNING: Line %d - Continuation of Database header has unexpected descriptor [%d] \n",
									line_number);
    	strncpy(dbhdrec.FCNAME,	text+2,20);
    	strncpy(dbhdrec.FCDATE,	text+22,8);
    	strncpy(dbhdrec.DQNAME,	text+30,20);
    	strncpy(dbhdrec.DQDATE,	text+50,8);
    	strncpy(buffer,		text+58,2);
	    buffer[2] = NULL;
    	    dbhdrec.DATA_MODEL =atoi(buffer);
    	dbhdrec.CONT_MARK_2 =	text[60] - '0';
    	dbhdrec.EOR_2 =		text[61];
    }
    else
	dbhdrec.EOR_2 =		'%';


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (dbhdrec.CONT_MARK_2 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Database header record has second continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    if (dbhdrec.EOR_2 != '%')
	fprintf(stderr,
	"WARNING: Line %d - Database header record has no line terminator.\n",
									line_number);


    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
	printf("Database Header:\n");
	printf("----------------\n");
	printf("\tDatabase Name:\t\t%s\n",dbhdrec.DBNAME);
	printf("\tData Dictionary Name:\t%s\n",dbhdrec.DDNAME);
    }

    strcpy(H_mname,dbhdrec.DBNAME);

}
