
/***********************************************************************************/
/***                                 sect_head()				 ***/
/***  	Reads in the Section Header Record (07) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 28th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

sect_head()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  SECHREC	sechrec;	/* Section header structure.			*/
    char    		buffer[80];	/* Temporary storage of text data 		*/

    /* Initialise all string structures */

    strcpy(sechrec.SECT_REF,	"          ");
    strcpy(sechrec.SURV_DATE,	"        ");
    strcpy(sechrec.LAST_AMND,	"        ");
    strcpy(sechrec.COPYRIGHT,	"        ");

    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    sechrec.REC_DESC_1	= 	7;
    strncpy(sechrec.SECT_REF,	text+2,10);
    sechrec.COORD_TYPE =	text[12] - '0';
    sechrec.STRUC_TYPE =	text[13] - '0';
    strncpy(buffer,		text+14,5);
	buffer[5] = NULL;
    	sechrec.XY_LEN =	atoi(buffer);
    sechrec.XY_UNIT =		text[19] - '0';
    strncpy(buffer,		text+20,10);
	buffer[10] = NULL;
    	sechrec.XY_MULT =	atoi(buffer)/1000.0;
    strncpy(buffer,		text+30,5);
	buffer[5] = NULL;
    	sechrec.Z_LEN =		atoi(buffer);
    sechrec.Z_UNIT =		text[35] - '0';
    strncpy(buffer,		text+36,10);
	buffer[10] = NULL;
    	sechrec.Z_MULT =	atoi(buffer)/1000.0;
    strncpy(buffer,		text+46,10);
	buffer[10] = NULL;
    	sechrec.X_ORIG =	atoi(buffer);
    strncpy(buffer,		text+56,10);
	buffer[10] = NULL;
    	sechrec.Y_ORIG =	atoi(buffer);
    strncpy(buffer,		text+66,10);
	buffer[10] = NULL;
    	sechrec.Z_DATUM =	atoi(buffer);
    sechrec.CONT_MARK_1 =	text[76] - '0';
    sechrec.EOR_1 =		text[77];

    if (sechrec.CONT_MARK_1 == 1)
    {
	if ( (sechrec.REC_DESC_2 = read_line()) != 0)
	    fprintf(stderr,
    "WARNING: Line %d - Continuation of Section header has unexpected descriptor [%d] \n"
							,line_number,sechrec.REC_DESC_2);
    	strncpy(buffer,		text+2,10);
	    buffer[10] = NULL;
    	    sechrec.XMIN =	atoi(buffer);
    	strncpy(buffer,		text+12,10);
	    buffer[10] = NULL;
    	    sechrec.YMIN =	atoi(buffer);
    	strncpy(buffer,		text+22,10);
	    buffer[10] = NULL;
    	    sechrec.XMAX =	atoi(buffer);
    	strncpy(buffer,		text+32,10);
	    buffer[10] = NULL;
    	    sechrec.YMAX =	atoi(buffer);
    	strncpy(buffer,		text+42,5);
	    buffer[5] = NULL;
    	    sechrec.XY_ACC =	atoi(buffer)/100.0;
    	strncpy(buffer,		text+47,5);
	    buffer[5] = NULL;
    	    sechrec.Z_ACC =	atoi(buffer)/100.0;
	strncpy(sechrec.SURV_DATE,text+52,8);
	strncpy(sechrec.LAST_AMND,text+60,8);
	strncpy(sechrec.COPYRIGHT,text+68,8);
    	sechrec.CONT_MARK_2 =	text[76] - '0';
    	sechrec.EOR_2 =		text[77];

    	if (sechrec.CONT_MARK_2 == 1)
    	{
	    if ( (sechrec.REC_DESC_3 = read_line()) != 0)
	    	fprintf(stderr,
    "WARNING: Line %d - Continuation of Section header has unexpected descriptor [%d] \n"
							,line_number,sechrec.REC_DESC_3);
	    strncpy(sechrec.SQNAME,	text+2,20);
	    strncpy(sechrec.SQDATE,	text+22,8);
    	    strncpy(buffer,		text+30,9);
	    	buffer[9] = NULL;
	    	sechrec.SCALE =		atoi(buffer);
    	    strncpy(buffer,		text+39,10);
	    	buffer[10] = NULL;
	    	sechrec.GRID_OR_X =	atoi(buffer)/1000.0;
    	    strncpy(buffer,		text+49,10);
	    	buffer[10] = NULL;
	    	sechrec.GRID_OR_Y =	atoi(buffer)/1000.0;
    	    strncpy(buffer,		text+59,8);
	    	buffer[8] = NULL;
	    	sechrec.PROJ_OR_LAT =	atoi(buffer)/10.0;
    	    strncpy(buffer,		text+67,8);
	    	buffer[8] = NULL;
	    	sechrec.PROJ_OR_LNG =	atoi(buffer)/10.0;
    	    sechrec.CONT_MARK_3 =	text[75] - '0';
    	    sechrec.EOR_3 =		text[76];
	}
	else
	{
	    if (strncmp(H_mname,"OSCAR",5) == 0)
		sechrec.SCALE = 10000;
    	    else
    		if (strncmp(H_mname,"OS_ROUTEPLANNER",15) == 0)
	   	    sechrec.SCALE = 625000;
    		else
    	    	    if (strncmp(H_mname,"OS_TRAVELMASTER",15) == 0)
	   		sechrec.SCALE = 250000;
		    else
	    		sechrec.SCALE = 0;

	    sechrec.CONT_MARK_3 = 0;
	    sechrec.EOR_3 = '%';
	}
    }
    else
    {
	sechrec.EOR_3 =	'%';
	sechrec.CONT_MARK_3 = 0;
    }


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (sechrec.EOR_3 != '%') 
	fprintf(stderr,
	"WARNING: Line %d - Section header record has no line terminator.\n",
									line_number);

    if (sechrec.CONT_MARK_3 != 0)
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;				/* NOTE: INCOMPLETE - NOT READING CONTINUATION	*/
					/*                    DATA INTO STRUCTURE.	*/

  

    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)			/* Print out log if requested.			*/
    {
	printf("\n");
	printf("Section Header:\n");
	printf("---------------\n");
	printf("\tDatabase Name:\t\t%s\n",sechrec.SECT_REF);
	printf("\tMeasurement units:");
	if (sechrec.XY_UNIT == 2)
	    printf("\tmetres.\n");
	else
	    printf("\tunknown.\n");
	printf("\tOrigin (SW corner):\t(%10d,%10d)\n",sechrec.X_ORIG,sechrec.Y_ORIG);
	printf("\tHorizontal Accuracy:");
	if (sechrec.XY_ACC == 0.0)
	    printf("\tnot recorded.\n");
	else
	    printf("\t%.2f\n",sechrec.XY_ACC);
	printf("\tVertical Accuracy:");
	if (sechrec.Z_ACC == 0.0)
	    printf("\tnot recorded.\n");
	else
	    printf("\t%.2f\n",sechrec.Z_ACC);
    }

    if (outfile)			
    {
	close_vect();			/* Close any previously opened vectors.		*/
	
	X_origin = sechrec.X_ORIG;	/* Identify map boundaries.			*/
	Y_origin = sechrec.Y_ORIG;

	X_min = sechrec.X_ORIG + (sechrec.XMIN*sechrec.XY_MULT);
	Y_min = sechrec.Y_ORIG + (sechrec.YMIN*sechrec.XY_MULT);
	X_max = sechrec.X_ORIG + ( (sechrec.XMAX - sechrec.XMIN)*sechrec.XY_MULT);
	Y_max = sechrec.Y_ORIG + ( (sechrec.YMAX - sechrec.YMIN)*sechrec.XY_MULT);

	XY_mult = sechrec.XY_MULT;
	Z_mult = sechrec.Z_MULT;

    	strcat(H_mname," - ");
	strcat(H_mname,sechrec.SECT_REF);
	strcpy(H_mdate,sechrec.SURV_DATE);
	get_date(H_mdate);
	H_scale = sechrec.SCALE;
    }
}

