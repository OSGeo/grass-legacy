/***********************************************************************************/
/***                                line_rec()					 ***/
/***  	Reads in the Line Record (23) into structure for processing		 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

line_rec()
{
    /*----------------------------------------------------------------------------------*/
    /*		IDENTIFY WHICH VARIATION OF THE LINE RECORD STRUCTURE IS REQUIRED	*/
    /*----------------------------------------------------------------------------------*/

    if (text[21] == '%')		/* Contour Variation.				*/
    	line_rec_c();
    else
   	if (text[23] == '%')		/* Default (OSCAR, 1:250k, 1:625k)		*/
	    line_rec_def();	
	else
	    if(text[36] == '%')		/* Land-Line Variation.				*/
	    	line_rec_l();
   	    else
	    	fprintf(stderr,"WARNING: Line %d - Can't identify Line Record Type.\n"
									,line_number);
}
	
/****************************************************************************************/
/***										      ***/
/*** Line record structure - Default format.				      	      ***/
/***										      ***/
/****************************************************************************************/

line_rec_def()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  LINEREC	linerec;	/* Line record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			rec_desc,	/* Record descriptor.				*/
			maptype;	/* Type of map data to read.			*/



    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    linerec.REC_DESC_1 = 	23;

    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	linerec.LINE_ID =	atoi(buffer);
    strncpy(buffer,		text+8,6);
	buffer[6] = NULL;
    	linerec.GEOM_ID =	atoi(buffer);
    strncpy(buffer,		text+14,2);
	buffer[2] = NULL;
    	linerec.NUM_ATT =	atoi(buffer);
    strncpy(buffer,		text+16,6);
	buffer[6] = NULL;
    	linerec.ATT_ID =	atoi(buffer);
 
    linerec.CONT_MARK_1 =	text[22] - '0';
    linerec.EOR_1 =		text[23];
  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (linerec.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Line record has unexpected continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                       READ GEOMETRY ASSOCIATED WITH POINT			*/
    /*----------------------------------------------------------------------------------*/

    if( (rec_desc = read_line()) != 21)
    {
	if (rec_desc == 99)
	    vol_term();			/* Allow for merged volumes between records	*/

	if (read_line() != 21)
	{
	    fprintf(stderr,
	"WARNING: Line %d - Line record is not followed by geometry record.\n",
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

    geom_rec(linerec.GEOM_ID,maptype);
}


/****************************************************************************************/
/***										      ***/
/*** Line record structure - Contour variation.					      ***/
/***										      ***/
/****************************************************************************************/

line_rec_c()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  LINEREC_C	linerec_c;	/* Line record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data. 		*/
			rec_desc;	/* Record descriptor.				*/

    /* Initialise string structure */

    strcpy(linerec_c.VAL_TYPE,	"  ");


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    linerec_c.REC_DESC_1 = 	23;
    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	linerec_c.LINE_ID =	atoi(buffer);
    strncpy(linerec_c.VAL_TYPE,text+8,2);
    strncpy(buffer,		text+10,6);
	buffer[6] = NULL;
    	linerec_c.VALUE =	atoi(buffer);
    strncpy(buffer,		text+16,4);
	buffer[4] = NULL;
    	linerec_c.FEAT_CODE =	atoi(buffer);

    linerec_c.CONT_MARK_1 =	text[20] - '0';
    linerec_c.EOR_1 =		text[21];
  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (linerec_c.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Line record has unexpected continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                       READ GEOMETRY ASSOCIATED WITH LINE				*/
    /*----------------------------------------------------------------------------------*/

    if( (rec_desc = read_line()) != 21)
    {
	if (rec_desc == 99)
	    vol_term();			/* Allow for merged volumes between records	*/

	if (read_line() != 21)
	{
	    fprintf(stderr,
	"WARNING: Line %d - Line record is not followed by geometry record.\n",
									line_number);
	    return(0);
	}

    }
    
    V_featcode = linerec_c.FEAT_CODE;

    geom_rec(linerec_c.VALUE,CONTOUR);
}


/****************************************************************************************/
/***										      ***/
/*** Line record structure - Land-Line variation.				      ***/
/***										      ***/
/****************************************************************************************/

line_rec_l()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  LINEREC_L	linerec_l;	/* Line record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data. 		*/
			rec_desc;	/* Record descriptor.				*/

    /* Initialise string structure */

    strcpy(linerec_l.VAL_TYPE,	"  ");


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    linerec_l.REC_DESC_1 = 	23;
    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	linerec_l.LINE_ID =	atoi(buffer);
    strncpy(linerec_l.VAL_TYPE,text+8,2);
    strncpy(buffer,		text+10,6);
	buffer[6] = NULL;
    	linerec_l.VALUE =	atoi(buffer);
    strncpy(buffer,		text+16,4);
	buffer[4] = NULL;
    	linerec_l.FEAT_CODE =	atoi(buffer);


    /********** INCOMPLETE **********/


    linerec_l.CONT_MARK_1 =	text[35] - '0';
    linerec_l.EOR_1 =		text[36];
  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (linerec_l.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Line record has unexpected continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                       READ GEOMETRY ASSOCIATED WITH LINE				*/
    /*----------------------------------------------------------------------------------*/

    if( ((rec_desc = read_line()) != 21) && (rec_desc != 22))
    {
	if (rec_desc == 99)
	    vol_term();			/* Allow for merged volumes between records	*/

	if (((rec_desc=read_line()) != 21) && (rec_desc != 22))
	{
	    fprintf(stderr,
	"WARNING: Line %d - Line record is not followed by geometry record.\n",
									line_number);
	    return(0);
	}

    }

    V_featcode = linerec_l.FEAT_CODE;

    if (rec_desc == 21)
        geom_rec(linerec_l.LINE_ID,DONT_KNOW);	 /* 2D Geometry */
		/* Have Changd from FEAT_CODE */
    else
	geom_rec2(linerec_l.FEAT_CODE,LANDLINE); 	 /* 3D Geometry */
}

