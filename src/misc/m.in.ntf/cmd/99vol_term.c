
/***********************************************************************************/
/***                                  vol_term()				 ***/
/***    Reads in Volume Termination Record (99) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

vol_term()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  VOLTERM	volterm;	/* Feature classification record structure.	*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			*posn_ptr;	/* Points to record termination mark.		*/

    strcpy(volterm.FREE_TEXT,	"                                                           ");

    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    volterm.REC_DESC_1 =	99;

    if ( (posn_ptr = strrchr(text,'%')) == NULL)
	fprintf(stderr,
	"WARNING: Line %d - Volume Termination Record has no terminator.\n",
									line_number);

    strncpy(volterm.FREE_TEXT,	text+2, (posn_ptr-1) - (text+2) );
    volterm.CONT_MARK_1 = 	*(posn_ptr-1) - '0';
    volterm.EOR_1 =	 	*posn_ptr;



    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (volterm.CONT_MARK_1 != 0)
 	if (read_line() != 1)	/* Read following volume header record.		*/
	    fprintf(stderr,
	    "WARNING: Line %d - Volume Header Record is missing.\n",line_number);



    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
       printf("\n");
       printf("************************************************************************\n");
       printf("\t%s\n",volterm.FREE_TEXT);
       printf("************************************************************************\n");
    }


    return (volterm.CONT_MARK_1);
}
