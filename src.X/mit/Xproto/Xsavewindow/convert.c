/*  %W%  %G%  */
#include <stdio.h>

#define GET(x,nx) if(fread(x, 1, nx, in)!=nx)return(readerr)
#define PUT(x,nx) if(fwrite(x, 1, nx, out)!=nx)return(writerr)

#define alloc (unsigned char *)malloc

char *convert(in, out)
FILE *in;
FILE *out;
{
    char *malloc();

    static char *readerr = "Read Error";
    static char *writerr = "Write Error";
    static char *allocerr = "Out of Memory";
    int i;
    float f;
    float color[3];
    int row;
    int plane;
    int nplanes;
    int ncols;
    int nrows;
    int cellsize;
    int cellbufsize;
    int planebufsize;
    int pblen;
    unsigned char *pbend;
    unsigned char *pb;
    unsigned char *planebuf;
    unsigned char *cellbuf;

	/* copy colortable */

    GET(&i, sizeof i);
    PUT(&i, sizeof i);

    while (i-- > 0) {
        GET(color, sizeof color);
        PUT(color, sizeof color);
    }

	/*
	* get number of planes
	* compute cellsize to contain these planes (1 bit per plane)
	* output cellsize
	*/
    GET(&nplanes, sizeof nplanes);
    cellsize = (nplanes + 7) / 8;
    PUT(&cellsize, sizeof cellsize);

	/* get plane buffer size */

    GET(&planebufsize, sizeof planebufsize);

	/* copy rows and cols */

    GET(&nrows, sizeof nrows);
    PUT(&nrows, sizeof nrows);

    GET(&ncols, sizeof ncols);
    PUT(&ncols, sizeof ncols);

    cellbufsize = ncols * cellsize;

	/* allocate the buffers */

    cellbuf = alloc(cellbufsize);
    if (cellbuf == NULL)
        return (allocerr);

    planebuf = alloc(pblen = nplanes * planebufsize);
    if (planebuf == NULL)
        return (allocerr);

    pbend = planebuf + pblen;

	/*
	* foreach row
	*     zero the cell buffer
	*     foreach plane in the row
	*          read the plane bits
	*          overlay into the cell buffer
	*     write out the cell buffer
	*/
    for (row = 0; row < nrows; row++) {
        for (i = 0; i < cellbufsize; i++)
            cellbuf[i] = 0;

        pb = pbend;
        for (plane = 0; plane < nplanes; plane++) {
            if (pb >= pbend) {
                GET(pb = planebuf, pblen);
            }
            build(plane, pb, cellbuf, ncols, cellsize);
            pb += planebufsize;
        }

        PUT(cellbuf, cellbufsize);
    }

    return (char *) NULL;
}
