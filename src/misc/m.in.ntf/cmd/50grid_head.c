
/***********************************************************************************/
/***                                 grid_head()				 ***/
/***  	  Reads in the Grid Header Record (50) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 28th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

grid_head()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  GRIDHREC	gridhrec;	/* Grid header structure.			*/
    char    		buffer[80];	/* Temporary storage of text data 		*/


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    gridhrec.REC_DESC_1	= 	50;
    strncpy(buffer,		text+2,10);
	buffer[10] = NULL;
    	gridhrec.GRID_ID =	atoi(buffer);
    strncpy(buffer,		text+12,4);
	buffer[4] = NULL;
    	gridhrec.N_COLUMNS =	atoi(buffer);
    strncpy(buffer,		text+16,4);
	buffer[4] = NULL;
    	gridhrec.N_ROWS =	atoi(buffer);
    strncpy(buffer,		text+20,4);
	buffer[4] = NULL;
    	gridhrec.N_PLANES =	atoi(buffer);
    strncpy(buffer,		text+24,10);
	buffer[10] = NULL;
    	gridhrec.X_COORD_1 =	atoi(buffer);
    strncpy(buffer,		text+34,10);
	buffer[10] = NULL;
    	gridhrec.Y_COORD_1 =	atoi(buffer);
    strncpy(buffer,		text+44,6);
	buffer[6] = NULL;
    	gridhrec.Z_COORD_1 =	atoi(buffer);
    strncpy(buffer,		text+50,10);
	buffer[10] = NULL;
    	gridhrec.X_COORD_2 =	atoi(buffer);
    strncpy(buffer,		text+60,10);
	buffer[10] = NULL;
    	gridhrec.Y_COORD_2 =	atoi(buffer);
    strncpy(buffer,		text+70,6);
	buffer[6] = NULL;
    	gridhrec.Z_COORD_2 =	atoi(buffer);

    gridhrec.CONT_MARK_1 =	text[76] - '0';
    gridhrec.EOR_1 =		text[77];

    if (gridhrec.CONT_MARK_1 == 1)
    {
	if ( (gridhrec.REC_DESC_2 = read_line()) != 0)
	    fprintf(stderr,
    "WARNING: Line %d - Continuation of Grid header has unexpected descriptor [%d] \n",
							gridhrec.REC_DESC_2,line_number);
    	strncpy(buffer,		text+2,10);
	    buffer[10] = NULL;
    	    gridhrec.X_COORD_3 =atoi(buffer);
    	strncpy(buffer,		text+12,10);
	    buffer[10] = NULL;
    	    gridhrec.Y_COORD_3 =atoi(buffer);
    	strncpy(buffer,		text+22,6);
	    buffer[6] = NULL;
    	    gridhrec.Z_COORD_3 =atoi(buffer);
    	strncpy(buffer,		text+28,10);
	    buffer[10] = NULL;
    	    gridhrec.X_COORD_4 =atoi(buffer);
    	strncpy(buffer,		text+38,10);
 	    buffer[10] = NULL;
    	    gridhrec.Y_COORD_4 =atoi(buffer);
    	strncpy(buffer,		text+48,6);
	    buffer[6] = NULL;
    	    gridhrec.Z_COORD_4 =atoi(buffer);

    	gridhrec.CONT_MARK_2 =	text[54] - '0';
    	gridhrec.EOR_2 =	text[55];

    	if (gridhrec.CONT_MARK_2 == 1)
    	{
	    if ( (gridhrec.REC_DESC_3 = read_line()) != 0)
	    	fprintf(stderr,
    "WARNING: Line %d - Continuation of Grid header has unexpected descriptor [%d] \n",
							gridhrec.REC_DESC_3,line_number);
    	    strncpy(buffer,		text+2,10);
	    	buffer[10] = NULL;
    	    	gridhrec.X_COORD_5 =atoi(buffer);
    	    strncpy(buffer,		text+12,10);
	    	buffer[10] = NULL;
    	    	gridhrec.Y_COORD_5 =atoi(buffer);
    	    strncpy(buffer,		text+22,6);
	    	buffer[6] = NULL;
    	    	gridhrec.Z_COORD_5 =atoi(buffer);
    	    strncpy(buffer,		text+28,10);
	    	buffer[10] = NULL;
    	    	gridhrec.X_COORD_6 =atoi(buffer);
    	    strncpy(buffer,		text+38,10);
 	    	buffer[10] = NULL;
    	    	gridhrec.Y_COORD_6 =atoi(buffer);
    	    strncpy(buffer,		text+48,6);
	    	buffer[6] = NULL;
    	    	gridhrec.Z_COORD_6 =atoi(buffer);

    	    gridhrec.CONT_MARK_3 =	text[54] - '0';
    	    gridhrec.EOR_3 =		text[55];

    	    if (gridhrec.CONT_MARK_3 == 1)
    	    {
	    	if ( (gridhrec.REC_DESC_4 = read_line()) != 0)
	    	    fprintf(stderr,
    "WARNING: Line %d - Continuation of Grid header has unexpected descriptor [%d] \n",
							gridhrec.REC_DESC_4,line_number);
    	    	strncpy(buffer,		text+2,10);
	    	    buffer[10] = NULL;
    	    	    gridhrec.X_COORD_7 =atoi(buffer);
    	    	strncpy(buffer,		text+12,10);
	    	    buffer[10] = NULL;
    	    	    gridhrec.Y_COORD_7 =atoi(buffer);
    	    	strncpy(buffer,		text+22,6);
	    	    buffer[6] = NULL;
    	    	    gridhrec.Z_COORD_7 =atoi(buffer);
    	    	strncpy(buffer,		text+28,10);
	    	    buffer[10] = NULL;
    	    	    gridhrec.X_COORD_8 =atoi(buffer);
    	    	strncpy(buffer,		text+38,10);
 	    	    buffer[10] = NULL;
    	    	    gridhrec.Y_COORD_8 =atoi(buffer);
    	    	strncpy(buffer,		text+48,6);
	    	    buffer[6] = NULL;
    	    	    gridhrec.Z_COORD_8 =atoi(buffer);

    	    	gridhrec.CONT_MARK_4 =	text[54] - '0';
    	    	gridhrec.EOR_4 =	text[55];
	    }
	}
    }
    else
	fprintf(stderr,
	"WARNING: Line %d - Expected Grid header continuation mark is missing.\n",
									line_number);


    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (gridhrec.CONT_MARK_4 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Grid header record has fourth continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    if (gridhrec.EOR_4 != '%') 
	fprintf(stderr,
	"WARNING: Line %d - Grid header record has no line terminator.\n",
									line_number);
   
    if ( (gridhrec.N_ROWS != 401) || (gridhrec.N_COLUMNS != 401) )
	fprintf(stderr,
	"WARNING: Line %d - Number of rows and cols not equal to 401: (r,c) = (%d,%d).\n",
					line_number,gridhrec.N_ROWS,gridhrec.N_COLUMNS);
   


    /*----------------------------------------------------------------------------------*/
    /*                   OUTPUT CONTENTS OF RECORD TO CONVERSION LOG			*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
	printf("Grid Header:\n");
	printf("----------------\n");
	printf("\tMap Square:\t%10d\n",gridhrec.GRID_ID);
	printf("\tNo of rows:\t%d\n",gridhrec.N_ROWS);
	printf("\tNo of columns:\t%d\n",gridhrec.N_COLUMNS);
	printf("\tCorners:\t(%10d,%10d)\n",gridhrec.X_COORD_1,gridhrec.Y_COORD_1);
	printf("\t\t\t(%10d,%10d)\n",gridhrec.X_COORD_2,gridhrec.Y_COORD_2);
	printf("\t\t\t(%10d,%10d)\n",gridhrec.X_COORD_3,gridhrec.Y_COORD_3);
	printf("\t\t\t(%10d,%10d)\n",gridhrec.X_COORD_4,gridhrec.Y_COORD_4);
	printf("\n");
    }


    /*----------------------------------------------------------------------------------*/
    /*                    WRITE OUT NECESSARY INFORMATION TO GRASS			*/
    /*----------------------------------------------------------------------------------*/

    /* Grid Header Record [50] must mean we need to open a raster */

    if (outfile)
    {
    	if (O_raster)
	    fprintf(stderr,
	    "WARNING: Line %d - More than one Grid Header Record found.\n",line_number);
    	else
    	    open_raster(gridhrec.X_COORD_1,gridhrec.Y_COORD_1,
						gridhrec.N_ROWS,gridhrec.N_COLUMNS);
    }

}

