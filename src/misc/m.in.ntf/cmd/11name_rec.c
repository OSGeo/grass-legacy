/***********************************************************************************/
/***                                name_rec()					 ***/
/***  	Reads in the Name Record (11) into structure for processing		 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 16th June 1993                               ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

name_rec()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  NAMEREC	namerec;	/* Name record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			rec_desc,	/* Record descriptor.				*/
			*posn_ptr;	/* Points to line terminator.			*/

    /* Initialise all string structures */

    strcpy(namerec.TEXT,"                                                                                                   ");


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    namerec.REC_DESC_1 = 	11;

    /********* NOTE : INCOMPLETE **********/

    posn_ptr = strrchr(text,'%');
    namerec.CONT_MARK_1 =	*(posn_ptr-1) - '0';
    namerec.EOR_1 =		*(posn_ptr);
  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (namerec.CONT_MARK_1 != 0)
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
}
