/***********************************************************************************/
/***                                 text_rep()					 ***/
/*** Reads in the Text Representation Record (45) into structure for processing  ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 4th July 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

text_rep()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  TEXTREP	textrep;	/* Text representation structure.		*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			*posn_ptr;	/* Points to line terminator.			*/

    char                rec_desc;       /* Record descriptor.                           */

    int			maptype;	/* Type of map to reconstruct.			*/

    /* INCOMPLETE */


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    /* INCOMPLETE */


    posn_ptr =  strrchr(text,'%');

    textrep.EOR_1 = *posn_ptr;
    textrep.CONT_MARK_1 = *(posn_ptr - 1) - '0';


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (textrep.EOR_1 != '%')
	fprintf(stderr,
	"WARNING: Line %d - Text representation record has no line terminator.\n",
									 line_number);

    if (textrep.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Text representation record has second continuation mark.\n",
									 line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }


    /*----------------------------------------------------------------------------------*/
    /*                       READ GEOMETRY ASSOCIATED WITH TEXT                         */
    /*----------------------------------------------------------------------------------*/

    if( (rec_desc = read_line()) != 21)
    {
        if (rec_desc == 99)
            vol_term();                 /* Allow for merged volumes between records     */

        if (read_line() != 21)
        {
            fprintf(stderr,
        "WARNING: Line %d - Text Representation Record is not followed by geometry record.\n",
                                                                        line_number);
            return(0);
        }

    }

    if (strncmp(H_mname,"OSCAR",5) == 0)
        maptype = OSCAR;
    else
        if (strncmp(H_mname,"OS_ROUTEPLANNER",15) == 0)
            maptype = ONE625;
        else
            if (strncmp(H_mname,"OS_TRAVELMASTER",15) == 0)
                maptype = ONE250;
            else
                if ((strncmp(H_mname,"Strategi",8) == 0) || (strncmp(H_mname,"STRATEGI",8) ==0))
                    maptype = STRATEGI;
                else
                    maptype = NO_ATTRIB;

    geom_rec(0,maptype);

 }
