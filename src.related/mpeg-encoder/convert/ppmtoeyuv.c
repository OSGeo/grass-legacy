/*===========================================================================*
 * ppm2eyuv.c								     *
 *									     *
 *	program to convert ppm file to yuv file				     *
 *									     *
 *===========================================================================*/

/*
 * Copyright (c) 1995 The Regents of the University of California.
 * All rights reserved.
 *
 * Permission to use, copy, modify, and distribute this software and its
 * documentation for any purpose, without fee, and without written agreement is
 * hereby granted, provided that the above copyright notice and the following
 * two paragraphs appear in all copies of this software.
 *
 * IN NO EVENT SHALL THE UNIVERSITY OF CALIFORNIA BE LIABLE TO ANY PARTY FOR
 * DIRECT, INDIRECT, SPECIAL, INCIDENTAL, OR CONSEQUENTIAL DAMAGES ARISING OUT
 * OF THE USE OF THIS SOFTWARE AND ITS DOCUMENTATION, EVEN IF THE UNIVERSITY OF
 * CALIFORNIA HAS BEEN ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
 *
 * THE UNIVERSITY OF CALIFORNIA SPECIFICALLY DISCLAIMS ANY WARRANTIES,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY
 * AND FITNESS FOR A PARTICULAR PURPOSE.  THE SOFTWARE PROVIDED HEREUNDER IS
 * ON AN "AS IS" BASIS, AND THE UNIVERSITY OF CALIFORNIA HAS NO OBLIGATION TO
 * PROVIDE MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS, OR MODIFICATIONS.
 */

/*  
 *  $Header: /n/picasso/users/keving/encode/src/RCS/readframe.c,v 1.1 1993/07/22 22:23:43 keving Exp keving $
 *  $Log: readframe.c,v $
 * Revision 1.1  1993/07/22  22:23:43  keving
 * nothing
 *
 */


/*==============*
 * HEADER FILES *
 *==============*/

#include <stdio.h>
#include "ansi.h"


typedef	unsigned char uint8;
typedef char int8;
typedef int boolean;
#define TRUE 1
#define FALSE 0


int	    height, width;
uint8   **ppm_data;
uint8 **orig_y, **orig_cr, **orig_cb;


/*===============================*
 * INTERNAL PROCEDURE prototypes *
 *===============================*/

static int  ReadNextInteger _ANSI_ARGS_((FILE *fpointer));
static boolean	ReadPPM _ANSI_ARGS_((FILE *fpointer));
static void WriteYUV _ANSI_ARGS_((FILE *fpointer));
static void PPMtoYUV _ANSI_ARGS_((void));


void	main(int argc, char **argv)
{
    if ( ! ReadPPM(stdin) )
    {
	fprintf(stderr, "Error reading PPM input file!!!\n");
	exit(1);
    }

    PPMtoYUV();

    WriteYUV(stdout);
}


static boolean	ReadPPM(FILE *fpointer)
{
    char    input[256];
    uint8   junk[4096];
    register int y;
    int  maxVal;

    if ( fread(input, sizeof(char), 2, fpointer) != 2 )
	return FALSE;

    if ( strncmp(input, "P6", 2) != 0 )	    /* magic number */
	return FALSE;

    width = ReadNextInteger(fpointer);
    if ( width == -1 )
        return FALSE;

    height = ReadNextInteger(fpointer);
    if ( height == -1 )
	return FALSE;

    maxVal = ReadNextInteger(fpointer);
    if ( maxVal == -1 )
	return FALSE;

    if ( maxVal != 255 )
    {
	fprintf(stdout, "MAXVAL != 255!!!  Exiting!\n");
	fprintf(stdout, "MAXVAL war %d\n",maxVal);
	exit(1);
    }

    ppm_data = (uint8 **) malloc(sizeof(uint8 *) * height);

    for ( y = 0; y < height; y++ )
    {
        ppm_data[y] = (uint8 *) malloc(3*sizeof(uint8) * width);
    }

    for ( y = 0; y < height; y++ )
    {
	fread(ppm_data[y], sizeof(char), 3*width, fpointer);

	/* read the leftover stuff on the right side */
	fread(junk, sizeof(char), 3*(width-width), fpointer);
    }

    return TRUE;
}


/*=====================*
 * INTERNAL PROCEDURES *
 *=====================*/

static int	ReadNextInteger(FILE *fpointer)
{
    char    input[256];
    int	    index;

    /* skip whitespace */
    while ( fgets(input, 2, fpointer) != NULL ) 
    {
	if ( isspace(input[0]) )
	    continue;

	if ( input[0]=='#' )	
	/* read the comment lines from # to \n */
	   {
	      while ( fgets(input, 2, fpointer) != NULL )
	       {
		if(input[0]=='\n') 
			break;
	        }
	     continue;
	    }


	/* read rest of integer */
	index = 1;
	while ( fgets(&input[index], 2, fpointer) != NULL )
	{
	    if ( isspace(input[index]) )
		break;
	    index++;
	}
	input[index] = '\0';

	return atoi(input);
    }

    return -1;	    /* end of file reached */
}



/*===========================================================================*
 *
 * PPMtoYUV
 *
 *	convert PPM data into YUV data
 *	assumes that ydivisor = 1
 *
 * RETURNS:	nothing
 *
 * SIDE EFFECTS:    none
 *
 *===========================================================================*/
void PPMtoYUV()
{
    register int x, y;
    register uint8 *dy0, *dy1;
    register uint8 *dcr, *dcb;
    register uint8 *src0, *src1;
    register int cdivisor;
    static boolean  first = TRUE;
    static float  mult299[1024], mult587[1024], mult114[1024];
    static float  mult16874[1024], mult33126[1024], mult5[1024];
    static float mult41869[1024], mult08131[1024];

    if ( first )
    {
        register int index;
	register int maxValue;

	maxValue = 255;

        for ( index = 0; index <= maxValue; index++ )
	{
	    mult299[index] = index*0.29900;
	    mult587[index] = index*0.58700;
	    mult114[index] = index*0.11400;
	    mult16874[index] = -0.16874*index;
	    mult33126[index] = -0.33126*index;
	    mult5[index] = index*0.50000;
	    mult41869[index] = -0.41869*index;
	    mult08131[index] = -0.08131*index;
	}
	
	first = FALSE;
    }

    orig_y = (uint8 **) malloc(sizeof(uint8 *) * height);
    for (y = 0; y < height; y++) {
        orig_y[y] = (uint8 *) malloc(sizeof(uint8) * width);
    }

    orig_cr = (uint8 **) malloc(sizeof(int8 *) * height / 2);
    for (y = 0; y < height / 2; y++) {
        orig_cr[y] = (uint8 *) malloc(sizeof(int8) * width / 2);
    }

    orig_cb = (uint8 **) malloc(sizeof(int8 *) * height / 2);
    for (y = 0; y < height / 2; y++) {
        orig_cb[y] = (uint8 *) malloc(sizeof(int8) * width / 2);
    }

    /* assume ydivisor = 1, so cdivisor = 4 */
    cdivisor = 4;

    for (y = 0; y < height; y += 2)
    {
	src0 = ppm_data[y];
	src1 = ppm_data[y + 1];
	dy0 = orig_y[y];
	dy1 = orig_y[y + 1];
	dcr = orig_cr[y / 2];
	dcb = orig_cb[y / 2];

	for ( x = 0; x < width; x += 2, dy0 += 2, dy1 += 2, dcr++,
				   dcb++, src0 += 6, src1 += 6)
	{
	    *dy0 = (mult299[*src0] +
		    mult587[src0[1]] +
		    mult114[src0[2]]);

	    *dy1 = (mult299[*src1] +
		    mult587[src1[1]] +
		    mult114[src1[2]]);

	    dy0[1] = (mult299[src0[3]] +
		      mult587[src0[4]] +
		      mult114[src0[5]]);

	    dy1[1] = (mult299[src1[3]] +
		      mult587[src1[4]] +
		      mult114[src1[5]]);

	    *dcb = ((mult16874[*src0] +
		     mult33126[src0[1]] +
		     mult5[src0[2]] +
		     mult16874[*src1] +
		     mult33126[src1[1]] +
		     mult5[src1[2]] +
		     mult16874[src0[3]] +
		     mult33126[src0[4]] +
		     mult5[src0[5]] +
		     mult16874[src1[3]] +
		     mult33126[src1[4]] +
		     mult5[src1[5]]) / cdivisor) + 128;

	    *dcr = ((mult5[*src0] +
		     mult41869[src0[1]] +
		     mult08131[src0[2]] +
		     mult5[*src1] +
		     mult41869[src1[1]] +
		     mult08131[src1[2]] +
		     mult5[src0[3]] +
		     mult41869[src0[4]] +
		     mult08131[src0[5]] +
		     mult5[src1[3]] +
		     mult41869[src1[4]] +
		     mult08131[src1[5]]) / cdivisor) + 128;
	}
    }
}


static void WriteYUV(FILE *fpointer)
{
    register int y;

    for (y = 0; y < height; y++)                        /* Y */
        fwrite(orig_y[y], 1, width, fpointer);

    for (y = 0; y < height / 2; y++)                    /* U */
        fwrite(orig_cb[y], 1, width / 2, fpointer);

    for (y = 0; y < height / 2; y++)                    /* V */
        fwrite(orig_cr[y], 1, width / 2, fpointer);
}

