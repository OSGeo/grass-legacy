
/***********************************************************************************/
/***                                 att_rec()					 ***/
/***  	      Reads in the Node Record (14) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 21st June 1993                               ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

att_rec()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  ATTREC	attrec;		/* Attribute record structure.			*/

    char		buffer[80],	/* Temporary buffer for storing text.		*/
			*posn_ptr,	/* Points to line terminator '%' in string.	*/
			offset,loffset,	/* Offset along text line.			*/
			label[80],	/* Category labels for vector object.		*/
			vect_name[80],
			writecats = TRUE;

    int			value;		/* Value of attribute (Height only)		*/

    /* Initialise string structures */

    strcpy(attrec.VAL_TYPE,"  ");
    label[0] = NULL;
    strcpy(vect_name," ");


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    attrec.REC_DESC_1	= 	14;
    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	attrec.ATT_ID =		atoi(buffer);

    offset = 8;				/* First value type in text string.		*/
    loffset =0;
					/* Scan through value types.			*/
    do
    {
    	strncpy(attrec.VAL_TYPE,text+offset,2);

    	if (strcmp(attrec.VAL_TYPE,"FC") == 0)				/* Feature Code */
	{
    	    strncpy(buffer,text+offset+2,4);
	    buffer[4] = NULL;
	    V_featcode = atoi(buffer);

	    offset += 6;
    	}


   	else if (strcmp(attrec.VAL_TYPE,"HT") == 0)			/* Height	*/
	{
    	    strncpy(buffer,text+offset+2,11);
	    buffer[11] = NULL;
	    value = atoi(buffer);

	    if (Write_vect == TRUE)
	    {
	    	write_vector(geom_type,value);
		Write_vect = FALSE;
	    }
	    sprintf(buffer,"%dm",value);
	    G_set_cat(value,buffer,&cats);

	    offset += 13;
	    writecats = FALSE;

    	}


    	else if (strcmp(attrec.VAL_TYPE,"SC") == 0)			/* Scale 	*/
	{
    	    switch(*(text+offset+2))
	    {
		case 'A':
		    H_scale = 2500;
		    break;

		case 'B':
		    H_scale = 10000;
		    break;

		case 'C':
		    H_scale = 50000;
		    break;

		case 'D':
		    H_scale = 100000;
		    break;

		default:
		    fprintf(stderr, "WARNING: Line %d - Unknown digitization scale.\n",
										line_number);
		    break;
	    }
	    offset += 3;
    	}

   	else if (strcmp(attrec.VAL_TYPE,"SY") == 0)		/* Date (ignored) 	*/
	    offset +=8;

  	else if (strcmp(attrec.VAL_TYPE,"LL") == 0)		/* Link Length (ignored)*/
	    offset +=7;

  	else if (strcmp(attrec.VAL_TYPE,"PN") == 0)		/* Proper Name		*/
	{
	    do
	    {
		vect_name[loffset] = *(text + offset + loffset + 2);
		loffset++;
	    }
	    while(*(text + offset + loffset + 2) != '\\');

	    vect_name[loffset] = ' ';
	    loffset++;
	    vect_name[loffset] = NULL;
	    offset += loffset + 2;
	}

  	else if (strcmp(attrec.VAL_TYPE,"RN") == 0) 		/* Road Number.		*/
	{
	    strcat(vect_name," "); 
	    strncat(vect_name,text+offset+2,8);
	    vect_name[loffset+9] = NULL;
	    offset+=10;
	}

  	else if (strcmp(attrec.VAL_TYPE,"FW") == 0) 		/* Form of way (ignord)	*/
	    offset += 3;

 	else if (strcmp(attrec.VAL_TYPE,"RB") == 0)		/* Rep. point (bounded)	*/
	    offset += 3;

 	else if (strcmp(attrec.VAL_TYPE,"RU") == 0)		/* Rep. pnt (unbounded)	*/
	    offset += 3;		 	
 	    	    	    	    	
	else if (strcmp(attrec.VAL_TYPE,"OR") == 0)		/* Orientation (ignord)	*/
	    offset += 6;	

	else if (strcmp(attrec.VAL_TYPE,"NU") == 0)		/* Numbered feature.	*/
	{
	    do
	    {
		vect_name[loffset] = *(text + offset + loffset + 2);
		loffset++;
	    }
	    while(*(text + offset + loffset + 2) != '\\');

	    vect_name[loffset] = ' ';
	    loffset++;
	    vect_name[loffset] = NULL;
	    offset += loffset + 2;
	}

	else if (strcmp(attrec.VAL_TYPE,"TC") == 0)		/* Orientation (ignord)	*/
	    offset += 6;	

	else if (strcmp(attrec.VAL_TYPE,"TL") == 0)		/* Orientation (ignord)	*/
	    offset += 4;

	else if (strcmp(attrec.VAL_TYPE,"TX") == 0)		/* Text string		*/
	{
	    loffset=0;
	    do
	    {
		vect_name[loffset] = *(text + offset + loffset + 2);
		loffset++;
	    }
	    while(*(text + offset + loffset + 2) != '\\');

	    vect_name[loffset] = NULL;
	    loffset++;
	    offset += loffset + 2;
	}
				    	    
	else
	{
	    fprintf(stderr, "WARNING: Line %d - Unrecognised attribute value type.\n",
									line_number);
	    offset +=1;
	}			

	if ( (*(text+offset+1) == '%') && (*(text+offset) == '1'))
	{
	    read_line();			/* If at the end of line and it has a	*/
	    offset = 2;				/* continuation code read next line.	*/
	}
    }
    while( *(text+offset+1) != '%');
	
    posn_ptr = strrchr(text,'%');	/* Search for line terminator.			*/

    attrec.CONT_MARK_1 =	*(posn_ptr-1) - '0';
    attrec.EOR_1 =		*(posn_ptr);
    
 
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (attrec.EOR_1 != '%') 
	fprintf(stderr,
	"WARNING: Line %d - Attribute record has no line terminator.\n",
									line_number);
   
    if (attrec.CONT_MARK_1 != 0)
    {
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD ?				*/
    /*----------------------------------------------------------------------------------*/

    if (writecats == TRUE)
    {
	strcpy(label," ");
    	strcpy(label,vect_name);
    	strcat(label,"- ");
    	strcat(label,G_get_cat(V_featcode,&feature_desc));

    	G_set_cat(attrec.ATT_ID,label,&cats);
    }
}
