
/***********************************************************************************/
/***                                 node_rec()					 ***/
/***  	      Reads in the Node Record (16) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 21st June 1993                               ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

node_rec()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  NODEREC	noderec;	/* Node record structure.			*/

    char		buffer[80],	/* Temporary buffer for storing text.		*/
			*posn_ptr,	/* Points to line terminator '%' in string.	*/
			rec_desc;	/* Record descriptor.				*/


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    noderec.REC_DESC_1	= 	16;
    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	noderec.NODE_ID =	atoi(buffer);
    strncpy(buffer,		text+8,6);
	buffer[6] = NULL;
    	noderec.GEOM_ID =	atoi(buffer);
    strncpy(buffer,		text+14,4);
	buffer[4] = NULL;
    	noderec.NUM_LINKS =	atoi(buffer);

    /******** INCOMPLETE  ********/



    posn_ptr = strrchr(text,'%');	/* Search for line terminator.			*/

    noderec.CONT_MARK_1 =	*(posn_ptr-1) - '0';
    noderec.EOR_1 =		*(posn_ptr);
    
 
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (noderec.EOR_1 != '%') 
	fprintf(stderr,
	"WARNING: Line %d - Node data record has no line terminator.\n",
									line_number);
   
    if (noderec.CONT_MARK_1 != 0)
    {
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                       READ GEOMETRY ASSOCIATED WITH NODE                         */
    /*----------------------------------------------------------------------------------*/

    if( (rec_desc = read_line()) != 21)
    {
        if (rec_desc == 99)
            vol_term();                 /* Allow for merged volumes between records     */

        if (read_line() != 21)
        {
            fprintf(stderr,
        "WARNING: Line %d - Node record is not followed by geometry record.\n",
                                                                        line_number);
            return(0);
        }

    }

    /******* Ignore the contents of node geometry for time being ******/

    /* ie, don't call geom_rec() */


    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD ?				*/
    /*----------------------------------------------------------------------------------*/


    /* Should precede a geometry record */
}
