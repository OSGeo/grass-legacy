/*
* $Id$
*
*****************************************************************************
*
* MODULE:   	GRASS gis library - NULL test
* AUTHOR(S):	Justin Hickey - Thailand - jhickey@hpcc.nectec.or.th
* PURPOSE:  	To test the capability of GRASS to set and check null values.
*   	    	This is simply a test program and is not intended for use in
*   	    	normal grass operation.
* COPYRIGHT:    (C) 2000 by the GRASS Development Team
*
*               This program is free software under the GNU General Public
*   	    	License (>=v2). Read the file COPYING that comes with GRASS
*   	    	for details.
*
*****************************************************************************/

/*============================= Include Files ==============================*/

/* System include files */
#include <stdio.h>
#include <limits.h>

/* Grass and local include files */
#include "gis.h"

/*======================= Internal Constants/Defines =======================*/

/* none */

/*========================== Internal Typedefs =============================*/

/* none */

/*====================== Static Variable Declaration =======================*/

/* none */

/*============================== Prototypes ================================*/

/* none */

/*======================= Internal Static Functions ========================*/

/* none */

/*============================ Main Function ===============================*/

int main ( int argc, char *argv[])
{
    CELL    	    cellArr[3];     /* array of CELL values */
    FCELL   	    fcellArr[3];    /* array of FCELL values */
    DCELL   	    dcellArr[3];    /* array of DCELL values */
    unsigned char   *bytePtr;	    /* byte pointer to access arrays by byte */
    int     	    i, j;   	    /* counters */
    
    /* This call initializes the null bit patterns */
    G_gisinit (argv[0]);
    
    /* Set some null values */
    G_set_c_null_value(cellArr, 3);
    G_set_f_null_value(fcellArr, 3);
    G_set_d_null_value(dcellArr, 3);
    
    /* Print out the null values for CELL type */
    fprintf(stderr, "Null value of CELL should be equal to INT_MIN\n");
    fprintf(stderr, "INT_MIN = %X in hex code\n", INT_MIN);
    fprintf(stderr, "Value of null value for type CELL, 3 values\n");
    
    for (i = 0; i < 3; i++)
    {
	fprintf(stderr, "%X", cellArr[i]);
	fprintf(stderr, " ");
    }
    
    fprintf(stderr, "\n\n");

    /* Print out the null values for FCELL type */
    fprintf(stderr, "Null value of FCELL should be all 1's\n");
    fprintf(stderr, "For a 32 bit FCELL value, this is FFFFFFFF\n");
    fprintf(stderr, "Value of null value for type FCELL, 3 values\n");
    
    for (i = 0; i < 3; i++)
    {
	bytePtr = (unsigned char *) &fcellArr[i];
    	
	for (j = 0; j < sizeof(FCELL); j++)
	{
	    fprintf(stderr, "%X", *bytePtr);
	    bytePtr++;
	}
	
	fprintf(stderr, " ");
    }
    
    fprintf(stderr, "\n\n");

    /* Print out the null values for DCELL type */
    fprintf(stderr, "Null value of DCELL should be all 1's\n");
    fprintf(stderr, "For a 64 bit DCELL value, this is FFFFFFFFFFFFFFFF\n");
    fprintf(stderr, "Value of null value for type DCELL, 3 values\n");
    
    for (i = 0; i < 3; i++)
    {
	bytePtr = (unsigned char *) &dcellArr[i];
    	
	for (j = 0; j < sizeof(DCELL); j++)
	{
	    fprintf(stderr, "%X", *bytePtr);
	    bytePtr++;
	}
	
	fprintf(stderr, " ");
    }
    
    fprintf(stderr, "\n\n");

    /* Check if the system detects CELL null values */
    fprintf(stderr, "Test of G_is_c_null_value()\n");
    
    if (G_is_c_null_value(&cellArr[0]))
    {
    	fprintf(stderr, "CELL null value detected\n\n");
    }
    else
    {
    	fprintf(stderr, "CELL null value NOT detected\n\n");
    }

    /* Check if the system detects FCELL null values */
    fprintf(stderr, "Test of G_is_f_null_value()\n");
    
    if (G_is_f_null_value(&fcellArr[0]))
    {
    	fprintf(stderr, "FCELL null value detected\n\n");
    }
    else
    {
    	fprintf(stderr, "FCELL null value NOT detected\n\n");
    }

    /* Check if the system detects DCELL null values */
    fprintf(stderr, "Test of G_is_d_null_value()\n");
    
    if (G_is_d_null_value(&dcellArr[0]))
    {
    	fprintf(stderr, "DCELL null value detected\n");
    }
    else
    {
    	fprintf(stderr, "DCELL null value NOT detected\n");
    }
    
    return(0);
}

