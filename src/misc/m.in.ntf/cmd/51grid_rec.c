
/***********************************************************************************/
/***                                 grid_rec()					 ***/
/***  	  Reads in the Grid Data Record (51) into structure for processing	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 28th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

grid_rec()
{
    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  GRIDREC	gridrec;	/* Grid data record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			gridvals[1604];	/* Holds a line of the raster.			*/
    int			num_lines,	/* Counts through the body of 21 grid records.	*/
			col;

    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    gridrec.REC_DESC_1	= 	50;
    strncpy(buffer,		text+2,10);
	buffer[10] = NULL;
    	gridrec.GRID_ID =	atoi(buffer);
    strncpy(gridrec.SURVEY,	text+12,7);
    strncpy(gridrec.CHANGE,	text+19,7);
    strncpy(buffer,		text+26,4);
	buffer[4] = NULL;
    	gridrec.COL_START =	atoi(buffer);
    strncpy(buffer,		text+30,4);
	buffer[4] = NULL;
    	gridrec.COL_END =	atoi(buffer);
    strncpy(buffer,		text+34,4);
	buffer[4] = NULL;
    	gridrec.ROW_START =	atoi(buffer);
    strncpy(buffer,		text+38,4);
	buffer[10] = NULL;
    	gridrec.ROW_END =	atoi(buffer);
    strncpy(buffer,		text+42,4);
	buffer[10] = NULL;
    	gridrec.PLA_START =	atoi(buffer);
    strncpy(buffer,		text+46,4);
	buffer[6] = NULL;
    	gridrec.PLA_END =	atoi(buffer);
    gridrec.COL_INV =		text[50] - '0';
    gridrec.ROW_INV =		text[51] - '0';
    gridrec.PLA_INV =		text[52] - '0';
    gridrec.ORDER =		text[53] - '0';
    gridrec.INTERPRET =		text[54] - '0';
    strncpy(buffer,		text+55,10);
	buffer[10] = NULL;
    	gridrec.V_OFFSET =	atoi(buffer);
    strncpy(buffer,		text+65,10);
	buffer[10] = NULL;
    	gridrec.V_SCALE =	atoi(buffer) / 1000.0;
    gridrec.CONT_MARK_1 =	text[75] - '0';
    gridrec.EOR_1 =		text[76];

    if (gridrec.CONT_MARK_1 == 1)
    {
	if ( (gridrec.REC_DESC_2 = read_line()) != 0)
	    fprintf(stderr,
    "WARNING: Line %d - Continuation of Grid data has unexpected descriptor [%d] \n",
							gridrec.REC_DESC_2,line_number);
    	strncpy(buffer,		text+2,8);
	    buffer[8] = NULL;
    	    gridrec.N_GRIDVAL =atoi(buffer);
    	gridrec.CONT_MARK_2 =	text[10] - '0';
    	gridrec.EOR_2 =	text[11];
    }
    else
    {
	fprintf(stderr,
	"WARNING: Line %d - Expected Grid header continuation mark is missing.\n",
									line_number);
    	gridrec.CONT_MARK_2 =	1;
    }

    if (gridrec.CONT_MARK_2 == 1)
    {
	for (num_lines=0; num_lines<21; num_lines++)
	{
	    if ( (gridrec.REC_DESC_3 = read_line()) != 0)
	    	fprintf(stderr,
    "WARNING: Line %d - Continuation of Grid data has unexpected descriptor [%d] \n",
							gridrec.REC_DESC_3,line_number);
    	    strncpy(gridrec.GRIDVAL_1,	text+2,76);
    	    gridrec.CONT_MARK_3 =	text[10] - '0';
    	    gridrec.EOR_3 =		text[11];

	    /* Add text string to gridvals */
	    if (outfile)
		strncpy(gridvals + num_lines*76,gridrec.GRIDVAL_1,76);
	}
	if ( (gridrec.REC_DESC_4 = read_line()) != 0)
	    fprintf(stderr,
    "WARNING: Line %d - Continuation of Grid data has unexpected descriptor [%d] \n",
							gridrec.REC_DESC_4,line_number);
    	strncpy(gridrec.GRIDVAL_2,	text+2,8);
    	gridrec.CONT_MARK_4 =	text[10] - '0';
    	gridrec.EOR_4 =		text[11];

	/* Write out contents to raster array */
	if (outfile)
	    strncpy(gridvals + 1596,gridrec.GRIDVAL_2,8);
    }
    else
	fprintf(stderr,
	"WARNING: Line %d - Expected Grid header continuation mark is missing.\n",
									line_number);

    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (gridrec.CONT_MARK_4 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - Grid data record has too many continuation marks.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    if (gridrec.EOR_4 != '%') 
	fprintf(stderr,
	"WARNING: Line %d - Grid data record has no line terminator.\n",
									line_number);
   


    /*----------------------------------------------------------------------------------*/
    /*                            OUTPUT CONTENTS OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (conversion_log)		/* Print out log if requested.				*/
    {
	printf("Grid Data:\tFirst Row:\tLast Row\tFirst Col\tLast Col\t\n");
	printf("          \t  %d\t\t  %d\t\t  %d\t\t  %d\n",
		gridrec.ROW_START,gridrec.ROW_END,gridrec.COL_START,gridrec.COL_END);
    }

    if (outfile)
    {
	col = gridrec.COL_START;
	printf("%d ",col);
	fill_raster(gridvals,col);

	/* fill_raster(gridvals,gridrec.ROW_START,gridrec.ROW_END,
			     gridrec.COL_START,gridrec.COL_END);
	*/

        num_rlines++;
	if (num_rlines == 401)		/* When entire raster is read, output to GRASS	*/
	    write_raster();
    }

}    



/***********************************************************************************/
/***                                fill_raster()				 ***/
/***  	  Extracts the raster values from a set of 22 Grid Data Records		 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 29th May 1993                                ***/
/***                                                                             ***/
/***********************************************************************************/



fill_raster(gridvals,col)
    char	gridvals[1604];		/* Text string containing raster values.   */
    int col;
{
    int		scany;			/* Scans a line through raster.		   */

    char	gridval_txt[5];		/* Stores a single grid value as text.	   */
    strcpy (gridval_txt,"    ");
printf("col = %d\n",col);


    for (scany=401; scany>=1; scany--)
    {
	strncpy(gridval_txt,gridvals,4);
    	raster[scany-1][col-1] = (CELL)atoi(gridval_txt);
	
	gridvals += 4;			/* Move to next number.			   */

   }


}

/*********************************************************/
/*** -- OLD VERSION WHICH DOESNT SEEM TO WORK ON SGI-- ***/
/*********************************************************/

fill_raster_OLD(gridvals,row_start,row_end,col_start,col_end)
    char	*gridvals;		/* Text string containing raster values.   */
    int		row_start,row_end,	/* Begining and ending row numbers. 	   */
		col_start,col_end;	/* Begining and ending column numbers.	   */
{
    int		row_ch, col_ch;		/* Change in r and c value with scan.	   */
    int		scanx,scany;		/* Scans a line through raster.		   */

    char	gridval_txt[5];		/* Stores a single grid value as text.	   */
    strcpy (gridval_txt,"    ");

    /* row_ch =  1,	col_ch =  0	->  Scan from bottom to top. 		   */
    /* row_ch = -1,	col_ch =  0	->  Scan from top to bottom. 		   */
    /* row_ch =  0,	col_ch =  1	->  Scan from left to right. 		   */
    /* row_ch =  0,	col_ch = -1	->  Scan from right to left. 		   */
    /* Any other combinations are invalid.					   */

    row_ch = (row_end-row_start) / (abs(row_end-row_start) - 0.0000001);
    col_ch = (col_end-col_start) / (abs(col_end-col_start) - 0.0000001);

    if (abs(row_ch + col_ch) != 1)	/* Problem with header data.		   */
	fprintf(stderr,
	"WARNING: Line %d - Row and column numbers in Grid Data Record incorrect.\n",
									line_number);

    for (scanx=col_start,scany=row_end;
			scanx <= col_ch*col_end, scany >= row_ch*row_start; 
						scanx += col_ch, scany -= row_ch)


    scanx=0;
    for (scany=401; scany >=1; scany--)
    {
	strncpy(gridval_txt,gridvals,4);
    	raster[scany-1][scanx-1] = (CELL)atoi(gridval_txt);
	
	gridvals += 4;			/* Move to next number.			   */

    }


}

