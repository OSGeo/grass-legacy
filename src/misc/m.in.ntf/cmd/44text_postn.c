/***********************************************************************************/
/***                                 text_postn()				 ***/
/***  	Reads in the Text Position Record (44) into structure for processing 	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 4th July 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

text_postn()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  TEXTPOS	textpos;	/* Text position structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			*posn_ptr;	/* Points to line terminator.			*/
    /* INCOMPLETE */


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    /* INCOMPLETE */


    posn_ptr =  strrchr(text,'%');

    textpos.EOR_1 = *posn_ptr;
    textpos.CONT_MARK_1 = *(posn_ptr - 1) - '0';


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/


    if (textpos.EOR_1 != '%')
	fprintf(stderr,
	"WARNING: Line %d - Text position record has no line terminator.\n", line_number);


    if (textpos.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Text position record has second continuation mark.\n",
									 line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }


    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD ?				*/
    /*----------------------------------------------------------------------------------*/

 }
