
/***********************************************************************************/
/***                                 data_desc()				 ***/
/***  	Reads in the Data Description Record (03) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

data_desc()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  DATADESC 	datadesc;	/* Volume header structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			*posn_ptr;	/* Points to '\' divider at end of descriptor.	*/

    /* Initialise all string structures */

    strcpy(datadesc.FIELD_NAME,	"          ");
    strcpy(datadesc.FINTER,	"     ");
    strcpy(datadesc.FDESC,	"                                      ");

    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    datadesc.REC_DESC_1	= 	3;
    strncpy(datadesc.FIELD_NAME,text+2,10);
    strncpy(buffer,		text+12,3);
	buffer[3] = NULL;
    	datadesc.FWIDTH =	atoi(buffer);
    strncpy(datadesc.FINTER,	text+15,5);

    if ( (posn_ptr = strrchr(text,'\\')) == NULL)
	fprintf(stderr,
	"WARNING: Line %d - Data Description Record has no divider.\n",line_number);
    else
	strncpy(datadesc.FDESC,	text+20,posn_ptr - (text+20));

    if ( (posn_ptr = strrchr(text,'%')) == NULL)
	fprintf(stderr,
	"WARNING: Line %d - Data Description Record has no terminator.\n",line_number);

    datadesc.CONT_MARK_1 =	*(posn_ptr-1) - '0';
    datadesc.EOR_1 =		*posn_ptr;


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (datadesc.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Data Description Record has continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
	printf("Data Description Record:\n");
	printf("------------------------\n");
	printf("\tField name:\t\t%s\n",datadesc.FIELD_NAME);
	printf("\tField description\t%s\n",datadesc.FDESC);
    }

}
