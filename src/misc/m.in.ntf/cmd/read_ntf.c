/***********************************************************************************/
/***                               read_ntf()					 ***/
/**    Function to read in Ordnance Survey NTF record description data. 	 ***/
/***  		   Assumes National Transfer Format V2.0 (BS 7567).		 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 19th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

read_ntf()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    char    	rec_desc,	/* Record Descriptor.					*/
    		cont_mark,	/* Continuation code - 0 is no continue, 1 to continue.	*/

    		end_of_file = FALSE;	
				/* Flag indicating all volumes have been read.		*/

    /*----------------------------------------------------------------------------------*/
    /*                            SCAN EACH RECORD IN NTF FILE				*/
    /*----------------------------------------------------------------------------------*/ 

    
    do
    {
    	rec_desc = read_line();

    	switch (rec_desc)
    	{
	    case 1:					/* MAP HEADER INFORMATION 	*/
	    	vol_head();
	    	break;
	    case 2:
	    	db_head();
	   	break;
	    case 3:
	    	data_desc();
	    	break;
	    case 4:
	    	data_fmt();
	    	break;
	    case 5:
	    	feat_class();
	    	break;

	    case 7:					/* SECTION HEADER INFORMATION 	*/
		sect_head();
		break;

 	    case(11):					/* TEXT INFORMATION 		*/
		name_rec();		
		break;
	    case(12):
		name_postn();
		break;
 	    case(43):					
		text_rec();		
		break;
	    case(44):
		text_postn();
		break;
	    case(45):
		text_rep();
		break;


   	    case(14):					/* ATTRIBUTE INFORMATION	*/
		att_rec();
		break;
	    case 40:
		att_desc();
		break;

	    case 15:					/* VECTOR COORD INFORMATION 	*/
		point_rec();
		break;
	    case 16:
		node_rec();
		break;
	    case 21:
		geom_rec(NO_ATTRIB,NO_ATTRIB);
		break;
	    case 22:
		geom_rec2(NO_ATTRIB,NO_ATTRIB);
	  	break;
	    case 23:
		line_rec();
		break;

	    case 50:					/* RASTER INFORMATION 		*/
		grid_head();
		break;

	    case 51:
		grid_rec();
		break;

	    case 99:					/* VOLUME TERMINATION INFO	*/
	    	if (vol_term() == 0)
		    end_of_file = TRUE;
	    	break;

	    case 0:					/* ERRORS			*/
	    	cont_error();
	    	break;
	    default:
	    	unknown_desc(rec_desc);
	    	break;
    	}
    }
    while (end_of_file == FALSE);

}
