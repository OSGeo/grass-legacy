
/***********************************************************************************/
/***                                  att_desc()				 ***/
/***  Reads in Attribute Description Record (40) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 28th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

att_desc()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  ATTDESC	attdesc;	/* Attribute description record structure.	*/
    char    		buffer[80],	/* Temporary storage of text data. 		*/
			*posn_ptr1,	/* Position of first divider  in record.	*/
			*posn_ptr2;	/* Position of divider and terminator in record.*/

    /* Initialise all string structures */

    strcpy(attdesc.VAL_TYPE,	"  ");
    strcpy(attdesc.FWIDTH,	"   ");
    strcpy(attdesc.FINTER,	"     ");
    strcpy(attdesc.ATT_NAME,	"                                                                ");
    strcpy(attdesc.FDESC,	"                                                              ");
 
    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    attdesc.REC_DESC_1 = 	5;
    strncpy(attdesc.FWIDTH,	text+2,2);
    strncpy(attdesc.FWIDTH,	text+4,3);
    strncpy(attdesc.FINTER,	text+7,5);

    if ( (posn_ptr1 = strchr(text,'\\')) == NULL)
	fprintf(stderr,
	"WARNING: Line %d - Attribute Description Record has no divider.\n",
									line_number);
    strncpy(attdesc.ATT_NAME,	text+12,posn_ptr1 - (text+12));
    if (*(posn_ptr1+2) != '%')
    {
    	if (( (posn_ptr2 = strrchr(text,'\\')) == NULL) || (posn_ptr2 == posn_ptr1) )
	    fprintf(stderr,
	    "WARNING: Line %d - Attribute Description Record has no second divider.\n",
									line_number);
	strncpy(attdesc.FDESC,	posn_ptr1+1,(posn_ptr2-1)-posn_ptr1);
    }

    if ( (posn_ptr2 = strrchr(text,'%')) == NULL)
	fprintf(stderr,
	"WARNING: Line %d - Attribute Description Record has no terminator.\n",
									line_number);
    attdesc.CONT_MARK_1 =	*(posn_ptr2-1) - '0';
    attdesc.EOR_1 =		*posn_ptr2;


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (attdesc.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Attribute Description Record has continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
	printf("Attribute Description:\n");
	printf("-----------------------\n");
	printf("\tName:\t\t\t%s\n",attdesc.ATT_NAME);
	printf("\tDescription:\t\t%s\n",attdesc.FDESC);
    }

}

