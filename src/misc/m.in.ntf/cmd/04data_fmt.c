
/***********************************************************************************/
/***                                  data_fmt()				 ***/
/***  	Reads in the Data Format Record (04) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

data_fmt()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  DATAFMT	datafmt;	/* Data format record structure.		*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			*posn_ptr;	/* Position of record terminator.		*/

    /* Initialise all string structures */

    strcpy(datafmt.REC_NAME,	"          ");
    strcpy(datafmt.FIELD_NAME,	"          ");
 
    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    datafmt.REC_DESC_1	= 	4;
    strncpy(buffer,		text+2,2);
	buffer[2] = NULL;
    	datafmt.REC_TYPE =	atoi(buffer);
    strncpy(datafmt.REC_NAME,	text+4,10);
    strncpy(buffer,		text+14,2);
	buffer[2] = NULL;
    	datafmt.NUM_FIELD =	atoi(buffer);
    strncpy(datafmt.FIELD_NAME,	text+16,10);
    datafmt.FUSE =		text[26];

    if ( (posn_ptr = strrchr(text,'%')) == NULL)
	fprintf(stderr,
	"WARNING: Line %d - Data Format Record has no terminator.\n",line_number);

    datafmt.CONT_MARK_1 =	*(posn_ptr-1) - '0';
    datafmt.EOR_1 =		*posn_ptr;


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (datafmt.CONT_MARK_1 != 0)
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;

    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
	printf("Data Format for record %d:\n", datafmt.REC_TYPE);
	printf("--------------------------\n");
	printf("\tRecord name:\t\t%s\n",datafmt.REC_NAME);
	printf("\tField name:\t\t%s\n",datafmt.FIELD_NAME);
    }


}
