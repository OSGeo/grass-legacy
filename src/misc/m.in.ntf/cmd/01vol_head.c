/***********************************************************************************/
/***                                  vol_head()				 ***/
/***  	Reads in the Volume header Record (01) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 27th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

vol_head()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  VOLHDREC	volhdrec;	/* Volume header structure.			*/
    char    		buffer[80];	/* Temporary storage of text data 		*/

    /* Initialise all string structures */

    strcpy(volhdrec.DONOR,	"                    ");
    strcpy(volhdrec.RECIPIENT,	"                    ");
    strcpy(volhdrec.TRANDATE,	"        ");

    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    volhdrec.REC_DESC_1	= 	1;
    strncpy(volhdrec.DONOR,	text+2,20);
    strncpy(volhdrec.RECIPIENT,	text+22,20);
    strncpy(volhdrec.TRANDATE,	text+42,8);
    strncpy(buffer,		text+50,4);
	buffer[4] = NULL;
    	volhdrec.SERIAL =	atoi(buffer);
    strncpy(buffer,		text+54,2);
	buffer[2] = NULL;
    	volhdrec.VOLNUM =	atoi(buffer);
    volhdrec.NTFLEVEL =		text[56] - '0';
    strncpy(buffer,		text+57,4);
	buffer[4] = NULL;
    	volhdrec.NTFVER =	atoi(buffer) / 100.0;
    volhdrec.NTFOR =		text[61];
    volhdrec.EOR_1 =		text[62];

    if (volhdrec.EOR_1 != '%')
    {
    	volhdrec.DIVIDER =	text[63];
    	volhdrec.CONT_MARK_1 = 	text[64] - '0';
  	volhdrec.EOR_2 =	text[65];
    }
    else
    {
    	volhdrec.DIVIDER =	' ';
    	volhdrec.CONT_MARK_1 = 	0;
  	volhdrec.EOR_2 =	'%';
    }

    /*----------------------------------------------------------------------------------*/
    /*                                CHECK NTF VERSION					*/
    /*----------------------------------------------------------------------------------*/

    if (volhdrec.NTFVER < 2.0)
    	old2new();
    else
	if (volhdrec.NTFVER != 2.0)
	    fprintf(stderr,
	    "WARNING: Line %d - Volume header indicates unknown NTF version [%.2f].\n",
								line_number,volhdrec.NTFVER);


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (volhdrec.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Volume header record has continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    if (volhdrec.EOR_2 != '%')
	fprintf(stderr,
	"WARNING: Line %d - Volume header record has no line terminator.\n",
									line_number);


    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
	printf("\n\n");
	printf("      National Transfer Format - Conversion Log\n");
	printf("      =========================================\n");
	printf("\n");
	printf("Donor:			%s\n",volhdrec.DONOR);
	printf("Processed for		%s\n",volhdrec.RECIPIENT);
	printf("Extracted from volume	%d\n",volhdrec.VOLNUM);
	printf("Uses NTF		Version %.2f\n",volhdrec.NTFVER);
	printf("			Level %d\n",volhdrec.NTFLEVEL); 
    }

    if (outfile)
    {
	strcpy(H_organ,volhdrec.DONOR);
	strcpy(H_ddate,volhdrec.TRANDATE);
	get_date(H_ddate);
    }
}
