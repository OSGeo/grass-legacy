/***********************************************************************************/
/***                               name_postn()					 ***/
/***  	Reads in the Name Position Record (12) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 16th June 1993                               ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

name_postn()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  NAMPOSTN	nampostn;	/* Name record structure.			*/
    char		rec_desc,	/* Record descriptor.				*/
                        *posn_ptr;      /* Position of terminator in record.		*/


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    nampostn.REC_DESC_1 = 	12;

    /********* NOTE : INCOMPLETE **********/

    if ( (posn_ptr = strrchr(text,'%')) == NULL)
        fprintf(stderr,
        "WARNING: Line %d - Name Position Record has no terminator.\n",
                                                                        line_number);
    nampostn.CONT_MARK_1 =       *(posn_ptr-1) - '0';
    nampostn.EOR_1 =             *posn_ptr;

  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (nampostn.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Name Position Record has unexpected continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                       READ GEOMETRY ASSOCIATED WITH TEXT				*/
    /*----------------------------------------------------------------------------------*/

    if( (rec_desc = read_line()) != 21)
    {
	if (rec_desc == 99)
	    vol_term();			/* Allow for merged volumes between records	*/

	if (read_line() != 21)
	{
	    fprintf(stderr,
	"WARNING: Line %d - Name Position Record is not followed by geometry record.\n",
									line_number);
	    return(0);
	}

    }

    geom_rec(0,TEXT_LABEL);
}
