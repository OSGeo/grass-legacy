/***********************************************************************************/
/***                                 text_rec()					 ***/
/***  	Reads in the Text Record (43) into structure for processing (incomplete) ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 4th July 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

text_rec()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  TEXTREC	textrec;	/* Text structure.				*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			*posn_ptr;	/* Points to line terminator.			*/
    /* INCOMPLETE */


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    /* INCOMPLETE */


    posn_ptr =  strrchr(text,'%');

    textrec.EOR_1 = *posn_ptr;
    textrec.CONT_MARK_1 = *(posn_ptr - 1) - '0';

    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (textrec.EOR_1 != '%')
	fprintf(stderr,
	"WARNING: Line %d - Text record has no line terminator.\n", line_number);

    if (textrec.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Text record has second continuation mark.\n", line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD ?				*/
    /*----------------------------------------------------------------------------------*/

 }
