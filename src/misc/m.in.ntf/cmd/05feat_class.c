
/***********************************************************************************/
/***                                  feat_class()				 ***/
/***  Reads in Feature Classification Record (05) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

feat_class()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  FEATCLASS	featclass;	/* Feature classification record structure.	*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			*posn_ptr;	/* Position of terminator in record.		*/

    /* Initialise all string structures */

    strcpy(featclass.CODE_COM,	"          ");
    strcpy(featclass.STCLASS,	"                    ");
    strcpy(featclass.FEATDES,	"                                           ");
 
    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    featclass.REC_DESC_1	= 	5;
    strncpy(buffer,		text+2,4);
	buffer[4] = NULL;
    	featclass.FEAT_CODE =	atoi(buffer);
    strncpy(featclass.CODE_COM,	text+6,10);
    strncpy(featclass.STCLASS,	text+16,20);

    if ( (posn_ptr = strrchr(text,'%')) == NULL)
	fprintf(stderr,
	"WARNING: Line %d - Feature Classification Record has no terminator.\n",
									line_number);

						/* Ignore last \ in description record.	*/
    strncpy(featclass.FEATDES,	text+36,(posn_ptr-2) - (text+36));
    featclass.CONT_MARK_1 =	*(posn_ptr-1) - '0';
    featclass.EOR_1 =		*posn_ptr;


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (featclass.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Feature Classification Record has continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
	printf("Feature Classification:\n");
	printf("-----------------------\n");
	printf("\tCode:\t\t\t%d\n",featclass.FEAT_CODE);
	printf("\tDescription\t\t%s\n",featclass.FEATDES);
    }

				/* Store feature description and code.			*/
    if (outfile)
       G_set_cat(featclass.FEAT_CODE,featclass.FEATDES,&feature_desc);

}
