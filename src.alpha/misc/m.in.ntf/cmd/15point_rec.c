/***********************************************************************************/
/***                                point_rec()					 ***/
/***  	Reads in the Point Record (15) into structure for processing		 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.1 15th June 1993                               ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

point_rec()
{
    /*----------------------------------------------------------------------------------*/
    /*		IDENTIFY WHICH VARIATION OF THE POINT RECORD STRUCTURE IS REQUIRED	*/
    /*----------------------------------------------------------------------------------*/

    if (text[21] == '%')		/* Contour Variation.				*/
    	point_rec_c();
    else
    	if (text[23] == '%')		/* Default (OSCAR, 1:250k, 1:625k).		*/
	    point_rec_def();
	else
    	    if (text[36] == '%')	/* Land-Line Variation.				*/
		point_rec_l();
	    else
	    	fprintf(stderr,"WARNING: Line %d - Can't identify Point Record Type.\n"
									,line_number);
}
	

/****************************************************************************************/
/***										      ***/
/*** Point record structure - Default format.				      	      ***/
/***										      ***/
/****************************************************************************************/

point_rec_def()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  POINTREC	pointrec;	/* Point record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			rec_desc,	/* Record descriptor.				*/
			maptype;	/* Type of map data to read.			*/

    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    pointrec.REC_DESC_1 = 	15;

    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	pointrec.POINT_ID =	atoi(buffer);
    strncpy(buffer,		text+8,6);
	buffer[6] = NULL;
    	pointrec.GEOM_ID =	atoi(buffer);
    strncpy(buffer,		text+14,2);
	buffer[2] = NULL;
    	pointrec.NUM_ATT =	atoi(buffer);
    strncpy(buffer,		text+16,6);
	buffer[6] = NULL;
    	pointrec.ATT_ID =	atoi(buffer);
 
    pointrec.CONT_MARK_1 =	text[22] - '0';
    pointrec.EOR_1 =		text[23];
  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (pointrec.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Point record has unexpected continuation mark.\n",
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

	if (rec_desc == 14)
	{
					/* Ignore Attribute description record if it 	*/
					/* does not follow a geometry record.		*/
	    return(0);
	}
	    

	if (read_line() != 21)
	{
	    fprintf(stderr,
	"WARNING: Line %d - Point record is not followed by geometry record.\n",
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

    geom_rec(pointrec.GEOM_ID,maptype);
}


/****************************************************************************************/
/***										      ***/
/*** Point record structure - Contour variation.				      ***/
/***										      ***/
/****************************************************************************************/

point_rec_c()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  POINTREC_C	pointrec_c;	/* Point record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			rec_desc;	/* Record descriptor.				*/

    /* Initialise all string structures */

    strcpy(pointrec_c.VAL_TYPE,	"  ");


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    pointrec_c.REC_DESC_1 = 	15;
    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	pointrec_c.POINT_ID =	atoi(buffer);
    strncpy(pointrec_c.VAL_TYPE,text+8,2);
    strncpy(buffer,		text+10,6);
	buffer[6] = NULL;
    	pointrec_c.VALUE =	atoi(buffer);
    strncpy(buffer,		text+16,4);
	buffer[4] = NULL;
    	pointrec_c.FEAT_CODE =	atoi(buffer);

    pointrec_c.CONT_MARK_1 =	text[20] - '0';
    pointrec_c.EOR_1 =		text[21];
  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (pointrec_c.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Point record has unexpected continuation mark.\n",
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
	"WARNING: Line %d - Point record is not followed by geometry record.\n",
									line_number);
	    return(0);
	}

    }

    V_featcode = pointrec_c.FEAT_CODE;

    geom_rec(pointrec_c.VALUE,CONTOUR);
}


/****************************************************************************************/
/***										      ***/
/*** Point record structure - Land-Line variation.				      ***/
/***										      ***/
/****************************************************************************************/

point_rec_l()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  POINTREC_L	pointrec_l;	/* Point record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			rec_desc;	/* Record descriptor.				*/

    /* Initialise all string structures */

    strcpy(pointrec_l.VAL_TYPE,	"  ");


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    pointrec_l.REC_DESC_1 = 	15;
    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	pointrec_l.POINT_ID =	atoi(buffer);
    strncpy(pointrec_l.VAL_TYPE,text+8,2);
    strncpy(buffer,		text+10,6);
	buffer[6] = NULL;
    	pointrec_l.VALUE =	atoi(buffer)/10.0;
    strncpy(buffer,		text+16,4);
	buffer[4] = NULL;
    	pointrec_l.FEAT_CODE =	atoi(buffer);

    /******** INCOMPLETE **********/

    pointrec_l.CONT_MARK_1 =	text[35] - '0';
    pointrec_l.EOR_1 =		text[36];
  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (pointrec_l.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Point record has unexpected continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                       READ GEOMETRY ASSOCIATED WITH POINT			*/
    /*----------------------------------------------------------------------------------*/

    if( ((rec_desc = read_line()) != 21) && (rec_desc != 22))
    {
	if (rec_desc == 99)
	    vol_term();			/* Allow for merged volumes between records	*/

	if (((rec_desc=read_line() != 21)) && (rec_desc != 22))
	{
	    fprintf(stderr,
	"WARNING: Line %d - Point record is not followed by geometry record.\n",
									line_number);
	    return(0);
	}

    }

    V_featcode = pointrec_l.FEAT_CODE;

    if (rec_desc == 21)
        geom_rec(pointrec_l.POINT_ID,DONT_KNOW);  /* 2D Geometry Variation */
			/* Have chaged from FEAT_CODE */
    else
        geom_rec2(pointrec_l.FEAT_CODE,LANDLINE); /* 3D Geometry Variation */
}
