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

/*==============*
 * HEADER FILES *
 *==============*/
#include <stdio.h>
#include <stdlib.h>
#include <malloc.h>

typedef unsigned char uint8;

#define CHOP(x)	    ((x < 0) ? 0 : ((x > 255) ? 255 : x))


uint8 **orig_y, **orig_cb, **orig_cr;
uint8 **red, **green, **blue;
int	width = 352;
int	height = 240;

void YUVtoPPM(void);
void WritePPM(FILE *fpointer);
void Usage(void);
void ReadYUV(FILE *fp);
void AllocYCC(void);


void	main(int argc, char **argv)
{
    FILE *fpointer;
    char command[256];
    char src[256], dest[256];

    if ((strcmp(argv[1],"-?") == 0) ||
	(strcmp(argv[1],"-h") == 0) ||
	(strcmp(argv[1],"-help") == 0)) {
      Usage();
    }

    switch (argc) {
    case 1:
      Usage();
      break;
    case 2:
      strcpy(src, argv[1]);
      sprintf(dest, "%s.jpg", argv[1]);
      break;
    case 3:
      strcpy(src, argv[1]);
      strcpy(dest, argv[2]);
      break;
    case 4:
      width = atoi(argv[1]);
      height = atoi(argv[2]);
      if (width==0 || height==0) Usage();
      strcpy(src, argv[3]);
      sprintf(dest, "%s.jpg", argv[3]);
      break;
    case 5:
      width = atoi(argv[1]);
      height = atoi(argv[2]);
      if (width==0 || height==0) Usage();
      strcpy(src, argv[3]);
      strcpy(dest, argv[4]);
      break;
    default:
      Usage();
    }

    AllocYCC();

    fprintf(stdout, "Reading (%dx%d):  %s\n", width, height, src);

    AllocYCC();
    fpointer = fopen(src, "r");
    ReadYUV(fpointer);
    fclose(fpointer);

    fprintf(stdout, "Converting to PPM\n");
    YUVtoPPM();

    fprintf(stdout, "Writing PPM\n");
    fpointer = fopen("/tmp/foobar", "w");
    WritePPM(fpointer);
    fclose(fpointer);

    fprintf(stdout, "Converting to JPEG %s\n", dest);
    sprintf(command, "cjpeg /tmp/foobar > %s", dest);
    system(command);
}


void Usage(void)
{
  printf("USAGE:  eyuvtojpeg yuv [jpeg]  (assumes %d by %d)\n", width, height);
  printf("\teyuvtojpeg width height yuv [jpeg]\n");
  exit(0);
}

void	WritePPM(FILE *fpointer)
{
    register int y, x;

    /* magic number */
    fprintf(fpointer, "P6\n");

    /* width, height */
    fprintf(fpointer, "%d %d\n", width, height);

    /* max value */
    fprintf(fpointer, "255\n");

    for ( y = 0; y < height; y++ )
	for ( x = 0; x < width; x++ )
	{
	    fwrite(&red[y][x], 1, 1, fpointer);
	    fwrite(&green[y][x], 1, 1, fpointer);
	    fwrite(&blue[y][x], 1, 1, fpointer);
	}
}


void YUVtoPPM(void)
{
    long   tempR, tempG, tempB;
    int	    r, g, b;
    int	    **Y, **U, **V;
    int	    x, y;

    /* * first, allocate tons of memory */

    Y = (int **) malloc(sizeof(int *) * height);
    for (y = 0; y < height; y++) {
      Y[y] = (int *) malloc(sizeof(int) * width);
    }

    U = (int **) malloc(sizeof(int *) * height / 2);
    for (y = 0; y < height / 2; y++) {
	U[y] = (int *) malloc(sizeof(int) * width / 2);
    }

    V = (int **) malloc(sizeof(int *) * height / 2);
    for (y = 0; y < height / 2; y++) {
	V[y] = (int *) malloc(sizeof(int) * width / 2);
    }

	for ( y = 0; y < height/2; y ++ )
	    for ( x = 0; x < width/2; x ++ )
	    {
		U[y][x] = orig_cb[y][x] - 128;
		V[y][x] = orig_cr[y][x] - 128;
	    }

	for ( y = 0; y < height; y ++ )
	    for ( x = 0; x < width; x ++ )
	    {
		Y[y][x] = orig_y[y][x] - 16;
	    }

    for ( y = 0; y < height; y++ )
	for ( x = 0; x < width; x++ )
	{
	    /* look at yuvtoppm source for explanation */

	    tempR = 104635*V[y/2][x/2];
	    tempG = -25690*U[y/2][x/2] + -53294 * V[y/2][x/2];
            tempB = 132278*U[y/2][x/2];

	    tempR += (Y[y][x]*76310);
	    tempG += (Y[y][x]*76310);
	    tempB += (Y[y][x]*76310);
	    
	    r = CHOP((int)(tempR >> 16));
	    g = CHOP((int)(tempG >> 16));
	    b = CHOP((int)(tempB >> 16));

	    red[y][x] = r;  green[y][x] = g;	blue[y][x] = b;
	}
}


/*=====================*
 * EXPORTED PROCEDURES *
 *=====================*/

void ReadYUV(fpointer)
     FILE *fpointer;
{
    register int y;

    for (y = 0; y < height; y++)			/* Y */
	fread(orig_y[y], 1, width, fpointer);

    for (y = 0; y < height / 2; y++)			/* U */
	fread(orig_cb[y], 1, width / 2, fpointer);

    for (y = 0; y < height / 2; y++)			/* V */
	fread(orig_cr[y], 1, width / 2, fpointer);
}


void AllocYCC()
{
    register int y;

    /*
     * first, allocate tons of memory
     */
    orig_y = (uint8 **) malloc(sizeof(uint8 *) * height);
    for (y = 0; y < height; y++) {
	orig_y[y] = (uint8 *) malloc(sizeof(uint8) * width);
    }

    orig_cr = (uint8 **) malloc(sizeof(uint8 *) * height / 2);
    for (y = 0; y < height / 2; y++) {
	orig_cr[y] = (uint8 *) malloc(sizeof(uint8) * width / 2);
    }

    orig_cb = (uint8 **) malloc(sizeof(uint8 *) * height / 2);
    for (y = 0; y < height / 2; y++) {
	orig_cb[y] = (uint8 *) malloc(sizeof(uint8) * width / 2);
    }

    /* * first, allocate tons of memory */

    red = (uint8 **) malloc(sizeof(uint8 *) * height);
    for (y = 0; y < height; y++) {
      red[y] = (uint8 *) malloc(sizeof(uint8) * width);
    }

    green = (uint8 **) malloc(sizeof(uint8 *) * height);
    for (y = 0; y < height; y++) {
	green[y] = (uint8 *) malloc(sizeof(uint8) * width);
    }

    blue = (uint8 **) malloc(sizeof(uint8 *) * height);
    for (y = 0; y < height; y++) {
	blue[y] = (uint8 *) malloc(sizeof(uint8) * width);
    }
}


