/****************************************************************************************/
/***                          	       read_line()				      ***/
/***          Reads a line from an NTF file and returns the record descriptor	      ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993             	                      ***/
/****************************************************************************************/

#include "ntf_in.h"

int read_line()
{
    /* ------ Initialise ------ */

    char rec_desc[3],		/* Stores 2 digit ASCII record descriptor		*/
   	 *posn_ptr=NULL;	/* Points to contrl-M character in string.		*/

    rec_desc[2] =NULL;


    /* ------ Read in a line (record) from the NTF file ------ */

    if (fgets(text,128,ntf_fptr) == NULL)
    {
	fprintf(stderr,"ERROR during read of NTF file\n");
	exit(-1);
    }

    line_number ++;		/* Accumulate line counter.				*/


    /* ------ Strip line of Ctrl-M (Return) Characters ------				*/

    posn_ptr = strchr(text,13);
    if (posn_ptr != NULL)
       *(posn_ptr) = NULL; 	/* Terminate string at first occurance of Ctrl-M.	*/


    /* ------ Find and return the Record Descriptor ------ */

    strncpy(rec_desc,text,2);		/* Copy first two characters.			*/
    return (atoi(rec_desc));		/* Convert into integer.			*/
}

/***********************************************************************************/
/***                                  cont_error()				 ***/
/***    Identifies and reports error if unexpected continuation code is found.	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

cont_error()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/

    char	*posn_ptr;	/* Position in string of continuation code.		*/
    char	cont_mark;	/* Continuation code.					*/

    fprintf(stderr,
	"WARNING: Line %d - Record contains unexpected continuation code.\n",
									line_number);


    /*----------------------------------------------------------------------------------*/
    /*                            IDENTIFY CONTINUATION CODE				*/
    /*----------------------------------------------------------------------------------*/             
    if ((posn_ptr = strrchr(text,'%')) == NULL)
        fprintf(stderr,
	"WARNING: Line %d - No record terminator found in line.\n",line_number);

    cont_mark = *(posn_ptr-1) - '0';
	
    /*----------------------------------------------------------------------------------*/
    /*                              READ CONTINUATION LINES				*/
    /*----------------------------------------------------------------------------------*/ 
    if (cont_mark != 0)
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;

}


/***********************************************************************************/
/***                               unknown_desc()				 ***/
/***    Identifies and reports error if unknown description code is found.	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

unknown_desc(rec_desc)
    char rec_desc;		/* The unknown record descriptor.			*/
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/

    char	*posn_ptr;	/* Position in string of continuation code.		*/
    char	cont_mark;	/* Continuation code.					*/

    fprintf(stderr,
	"WARNING: Line %d - Unknown record descriptor [%d].\n",line_number,rec_desc);

    /*----------------------------------------------------------------------------------*/
    /*                            IDENTIFY CONTINUATION CODE				*/
    /*----------------------------------------------------------------------------------*/             
    if ((posn_ptr = strrchr(text,'%')) == NULL)
        fprintf(stderr,
	"WARNING: Line %d - No record terminator found in line.\n",line_number);

    cont_mark = *(posn_ptr-1) - '0';

    /*----------------------------------------------------------------------------------*/
    /*                              READ CONTINUATION LINES				*/
    /*----------------------------------------------------------------------------------*/ 
    if (cont_mark != 0)
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
}


/***********************************************************************************/
/***                                read_blank()				 ***/
/***         Reads in an NTF record when the contents are to be ignored.	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

read_blank()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             


    char   cont_mark,		/* Continuation code ('0' or '1').			*/
    	   rec_desc_txt[3],	/* Stores 2 digit ASCII record descriptor		*/
	   rec_desc,		/* Integer record descriptor.				*/
	   *posn_ptr;		/* Used to find position of characters in string.	*/

    rec_desc_txt[2] = NULL;

    /*----------------------------------------------------------------------------------*/
    /*                            READ A LINE FROM THE NTF FILE				*/
    /*----------------------------------------------------------------------------------*/             

    /* ------ Read in a line (record) from the NTF file ------ */

    if (fgets(text,128,ntf_fptr) == NULL)
    {
	fprintf(stderr,"ERROR during read of NTF line.\n");
	exit(-1);
    }
    line_number++;		/* Accumulate line counter.				*/

    /*----------------------------------------------------------------------------------*/
    /*                              CHECK RECORD DESCRIPTOR				*/
    /*----------------------------------------------------------------------------------*/

    strncpy(rec_desc_txt,text,2);	/* Copy first two characters.			*/
    rec_desc = atoi(rec_desc_txt);	/* Convert into integer.			*/
    if (rec_desc != 0)			
    	fprintf(stderr,
	"WARNING: Line %d - Record descriptor is %d for continuation line.\n",
								line_number,rec_desc);

    /*----------------------------------------------------------------------------------*/
    /*                            IDENTIFY CONTINUATION CODE				*/
    /*----------------------------------------------------------------------------------*/

    if ((posn_ptr = strrchr(text,'%')) == NULL)
        fprintf(stderr,
	"WARNING: Line %d - No record terminator found in continuation line.\n",
								line_number);

    cont_mark = *(posn_ptr-1) - '0'; 

    return(cont_mark);
}
