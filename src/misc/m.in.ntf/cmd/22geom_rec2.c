/***********************************************************************************/
/***                              geom_rec()					 ***/
/***  	Reads in a three dimensional  Geometry Record (22) into structure	 ***/
/***                                                                             ***/
/***  Jo Wood, Project ASSIST, V1.0 29th July 1993                               ***/
/***                                                                             ***/
/***********************************************************************************/

#include "ntf_in.h"

geom_rec2(value,maptype)
    int	value,				/* Numerical attribute of vector to be read.	*/
        maptype;			/* Type of map being converted.			*/
{	

    /*----------------------------------------------------------------------------------*/
    /*                                   INITIALISE					*/
    /*----------------------------------------------------------------------------------*/             
    struct  GEOMREC2	geomrec;	/* Geometry record structure.			*/
    char    		buffer[80],	/* Temporary storage of text data 		*/
			*posn_ptr;	/* Points to line terminator in string.		*/
    int			coords,		/* Counts through each coordinate pair in object*/
			geom_type;	/* Point line or area type of vector object.	*/
    char		coord_digits=10;/* Number of digits representing a coordinate.	*/


    /*----------------------------------------------------------------------------------*/
    /*		MAKE SURE THAT A POINT OR LINE RECORD HAS JUST BEEN READ		*/
    /*----------------------------------------------------------------------------------*/

    if ( (value==NO_ATTRIB) && (maptype==NO_ATTRIB) )
    {
 	fprintf(stderr,
	"WARNING: Line %d - 3D Geometry record has no preceding point or line record\n",
									 line_number);
	if ( (posn_ptr=strrchr(text,'%')) == NULL)
	    fprintf(stderr,
	    "WARNING: Line %d - 3D Geometry record has no line terminator\n",line_number);

    	if (*(posn_ptr-1) != 0)
	    while (read_blank() != 0)	/* Read following continuation lines.		*/
 	    ;
    	return(0);
    }


    /*----------------------------------------------------------------------------------*/
    /*                        IDENTIFY THE TYPE OF MAP TO CONVERT			*/
    /*----------------------------------------------------------------------------------*/

    switch (maptype)
    {
	case LANDLINE:
	    coord_digits = 6;
	    break;

	default:
 	    fprintf(stderr,
	    "WARNING: Line %d - 3D Geometry record associated with unknown record type\n",
									 line_number);
	    break;
    }


    /*----------------------------------------------------------------------------------*/
    /*                            SCAN RECORD INTO STRUCTURE				*/
    /*----------------------------------------------------------------------------------*/

    geomrec.REC_DESC_1 = 	22;
    strncpy(buffer,		text+2,6);
	buffer[6] = NULL;
    	geomrec.GEOM_ID =	atoi(buffer);
    geomrec.G_TYPE =		text[8] - '0';
    strncpy(buffer,		text+9,4);
	buffer[4] = NULL;
    	geomrec.NUM_COORD =	atoi(buffer);


    /* Allocate enough memory to hold coordinates of segment.	*/

    if ( dig_alloc_points(points,geomrec.NUM_COORD) < 0)
	fprintf(stderr,
	"WARNING: Line %d - Not enough memory to hold entire vector segment.\n",
									line_number);


    posn_ptr = text+13;			/* Set position to first coordinate pair.	*/

    for (coords=0; coords<geomrec.NUM_COORD; coords++)
    {
    	strncpy(buffer,		posn_ptr,coord_digits);
	    buffer[coord_digits] = NULL;
    	    geomrec.X_COORD =	atoi(buffer);

    	if (*(posn_ptr+coord_digits+1) != '%')	
					/* If we are not at the end of the record,	*/
	    posn_ptr += coord_digits;	/*  move to the y coordinate in the record.	*/
	else
	{
	    if (read_line() == 0) 	/* If we are at the end of a record, read	*/
	        posn_ptr = text+2;	/* next record and position ptr at start.	*/
	    else
		fprintf(stderr,
"WARNING: Line %d - 3D Geometry record does not have expected continuation descriptor.\n",
									line_number);
	}

    	strncpy(buffer,		posn_ptr,coord_digits);
	    buffer[coord_digits] = NULL;
    	    geomrec.Y_COORD =	atoi(buffer);

    	if (*(posn_ptr+coord_digits+1) != '%')	
					/* If we are not at the end of the record,	*/
	    posn_ptr += coord_digits;	/*  move to the QPlan element in the record	*/
	else
	{
	    if (read_line() == 0) 	/* If we are at the end of a record, read	*/
	        posn_ptr = text+2;	/* next record and position ptr at start.	*/
	    else
		fprintf(stderr,
"WARNING: Line %d - 3D Geometry record does not have expected continuation descriptor.\n",
									line_number);
	}

    	geomrec.Q_PLAN =	*(posn_ptr);

    	if (*(posn_ptr+2) != '%')	/* If we are not at the end of the record,	*/
	    posn_ptr += 1;		/*  move to the z coordinate in the record.	*/
	else
	    if (*(posn_ptr+1) == '1')	/* If continuation code, read the next line and	*/
	    {				/* set the pointer to the first coordinate pair.*/

	    	if (read_line() == 0)	/* If we are at the end of a record, read	*/
	            posn_ptr = text+2;	/* next record and position ptr at start.	*/
	    	else
		    fprintf(stderr,
"WARNING: Line %d - 3D Geometry record does not have expected continuation descriptor.\n",
									line_number);
	    }
	    else
		break;

    	strncpy(buffer,		posn_ptr,coord_digits);
	    buffer[coord_digits] = NULL;
    	    geomrec.Z_COORD =	atoi(buffer);

    	if (*(posn_ptr+coord_digits+1) != '%')	
					/* If we are not at the end of the record,	*/
	    posn_ptr += coord_digits;	/*  move to the QHT element in the record	*/
	else
	{
	    if (read_line() == 0) 	/* If we are at the end of a record, read	*/
	        posn_ptr = text+2;	/* next record and position ptr at start.	*/
	    else
		fprintf(stderr,
"WARNING: Line %d - 3D Geometry record does not have expected continuation descriptor.\n",
									line_number);
	}

    	geomrec.QHT =	*(posn_ptr);

	if (outfile)
	{
	    points->x[coords] = (double)(X_origin + XY_mult*geomrec.X_COORD);
	    points->y[coords] = (double)(Y_origin + XY_mult*geomrec.Y_COORD);
	    points->n_points = coords+1;
	}

    	if (*(posn_ptr+2) != '%')	/* If we are not at the end of the record,	*/
	    posn_ptr += 1;		/*  move to next coordinate pair in the record.	*/
	else
	    if (*(posn_ptr+1) == '1')	/* If continuation code, read the next line and	*/
	    {				/* set the pointer to the first coordinate pair.*/

	    	if (read_line() == 0)	/* If we are at the end of a record, read	*/
	            posn_ptr = text+2;	/* next record and position ptr at start.	*/
	    	else
		    fprintf(stderr,
"WARNING: Line %d - 3D Geometry record does not have expected continuation descriptor.\n",
									line_number);
	    }
	    
    }
    geomrec.CONT_MARK_1 =	*(posn_ptr+1) - '0';
    geomrec.EOR_1 =		*(posn_ptr+2);

  
    /*----------------------------------------------------------------------------------*/
    /*                            CHECK INTEGRITY OF RECORD				*/
    /*----------------------------------------------------------------------------------*/

    if (geomrec.CONT_MARK_1 != 0)
    {
	fprintf(stderr,
	"WARNING: Line %d - 3D  Geometry record has unexpected continuation mark.\n",
									line_number);
	while (read_blank() != 0)	/* Read following continuation lines.		*/
 	;
    }

    /*----------------------------------------------------------------------------------*/
    /*                          WRITE OUT VECTOR COORDINATES				*/
    /*----------------------------------------------------------------------------------*/

    if (outfile)
    {
	if (geomrec.G_TYPE == 1)
	    geom_type = DOT;
	else
	    if (geomrec.G_TYPE == 2)
		geom_type = LINE;
	    else
	    {
		fprintf(stderr,
	"WARNING: Line %d - Unknown geometry type for vector object (will assume line).\n",
									line_number);
		geom_type = LINE;
	    }
    	write_vector(geom_type,value);

    }
}
