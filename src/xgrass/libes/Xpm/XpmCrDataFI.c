/* Copyright 1990,91 GROUPE BULL -- See licence conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrDataFI.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Scan an image and possibly its mask and create an XPM array                *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"
#ifdef VMS
#include "sys$library:string.h"
#else
#ifdef SYSV
#include <string.h>
#else
#include <strings.h>
#endif
#endif

LFUNC(CreateData, int, (char ***data_return,
		    xpmInternAttrib * attrib, XpmAttributes * attributes));

int
XpmCreateDataFromImage(display, data_return, image, shapeimage, attributes)
    Display *display;
    char ***data_return;
    XImage *image;
    XImage *shapeimage;
    XpmAttributes *attributes;
{
    int ErrorStatus;
    xpmInternAttrib attrib;

    /*
     * initialize return values 
     */
    *data_return = NULL;

    xpmInitInternAttrib(&attrib);

    /*
     * Scan image then create data 
     */
    ErrorStatus = xpmScanImage(display, image, shapeimage,
			       attributes, &attrib);

    if (ErrorStatus == XpmSuccess)
	ErrorStatus = CreateData(data_return, &attrib, attributes);

    xpmFreeInternAttrib(&attrib);

    return (ErrorStatus);
}


#undef RETURN
#define RETURN(status) \
  { if (header) { \
        for (a = 0; a < header_nlines; a++) \
	    if (header[a]) \
		free(header[a]); \
	free(header); \
    } \
    return(status); }

static int
CreateData(data_return, attrib, attributes)
    char ***data_return;
    xpmInternAttrib *attrib;
    XpmAttributes *attributes;
{
    /* calculation variables */
    xpmRgbName rgbn[MAX_RGBNAMES];
    int rgbn_max = 0;
    char *colorname;
    unsigned int *iptr;
    unsigned int a, b, c, x, y, key, d, e;
    char buf[BUFSIZ];
    char **header = NULL;
    unsigned int header_size, header_nlines;
    unsigned int data_size, data_nlines;

    /*
     * read the rgb file if any was specified 
     */
    if (attributes && (attributes->valuemask & XpmRgbFilename))
	rgbn_max = xpmReadRgbNames(attributes->rgb_fname, rgbn);

    /*
     * alloc a temporary array of char pointer for the header section which
     * is the hints line + the color table lines 
     */
    header_nlines = 1 + attrib->ncolors;
    header_size = sizeof(char *) * header_nlines;
    header = (char **) calloc(header_size, sizeof(char *));
    if (!header)
	RETURN(XpmNoMemory);

    /*
     * print the hints line 
     */
    sprintf(buf, "%d %d %d %d",
	    attrib->width, attrib->height, attrib->ncolors, attrib->cpp);

    if (attributes && (attributes->valuemask & XpmHotspot))
	sprintf(&buf[strlen(buf)], " %d %d",
		attributes->x_hotspot, attributes->y_hotspot);

    header[0] = (char *) malloc(strlen(buf) + 1);
    if (!header[0])
	RETURN(XpmNoMemory);
    header_size += strlen(buf) + 1;
    strcpy(header[0], buf);

    /*
     * print colors 
     */
    if (attrib->mask_pixel != UNDEF_PIXEL) {	/* transparent pixel */

	for (b = 0; b < attrib->cpp; b++)
	    buf[b] = attrib->colorStrings[0][b];
	buf[b] = '\0';

	if (attributes && (attributes->valuemask & XpmInfos)
	    && attributes->mask_pixel != UNDEF_PIXEL) {
	    for (key = 1; key < NKEYS + 1; key++) {
		if (attributes->colorTable[attributes->mask_pixel][key])
		    sprintf(&buf[strlen(buf)], "\t%s %s",
			    xpmColorKeys[key - 1],
			attributes->colorTable[attributes->mask_pixel][key]
			);
	    }
	} else
	    sprintf(&buf[strlen(buf)], "\tc %s", TRANSPARENT_COLOR);

	header[1] = (char *) malloc(strlen(buf) + 1);
	if (!header[1])
	    RETURN(XpmNoMemory);
	header_size += strlen(buf) + 1;
	strcpy(header[1], buf);

	d = 1;
    } else
	d = 0;

    for (a = d; a < attrib->ncolors; a++) {	/* other colors */
	for (b = 0; b < attrib->cpp; b++)
	    buf[b] = attrib->colorStrings[a][b];
	buf[b] = '\0';

	c = 1;
	if (attributes && (attributes->valuemask & XpmInfos)) {
	    e = 0;
	    for (b = 0; b < attributes->ncolors; b++) {
		if (b == attributes->mask_pixel) {
		    e = 1;
		    continue;
		}
		if (attributes->pixels[b - e] == attrib->xcolors[a].pixel)
		    break;
	    }
	    if (b != attributes->ncolors) {
		c = 0;
		for (key = 1; key < NKEYS + 1; key++) {
		    if (attributes->colorTable[b][key])
			sprintf(&buf[strlen(buf)], "\t%s %s",
				xpmColorKeys[key - 1],
				attributes->colorTable[b][key]);
		}
	    }
	}
	if (c) {
	    colorname = NULL;
	    if (rgbn_max)
		colorname = xpmGetRgbName(rgbn, rgbn_max,
					  attrib->xcolors[a].red,
					  attrib->xcolors[a].green,
					  attrib->xcolors[a].blue);
	    if (colorname)
		sprintf(&buf[strlen(buf)], "\tc %s", colorname);
	    else
		sprintf(&buf[strlen(buf)], "\tc #%04X%04X%04X",
			attrib->xcolors[a].red,
			attrib->xcolors[a].green,
			attrib->xcolors[a].blue);
	}
	header[1 + a] = (char *) malloc(strlen(buf) + 1);
	if (!header[1 + a])
	    RETURN(XpmNoMemory);
	header_size += strlen(buf) + 1;
	strcpy(header[1 + a], buf);
    }

    /*
     * now we know the size needed, alloc the data and copy the header lines 
     */
    data_size = header_size + attrib->height * sizeof(char *)
	+ attrib->height * (attrib->width * attrib->cpp + 1);

    *data_return = (char **) malloc(data_size);
    if (!*data_return)
	RETURN(XpmNoMemory);

    data_nlines = header_nlines + attrib->height;
    (*data_return)[0] = (char *) (*data_return + data_nlines);
    for (a = 0; a < 1 + attrib->ncolors; a++) {
	strcpy((*data_return)[a], header[a]);
	(*data_return)[a + 1] = (*data_return)[a] + strlen(header[a]) + 1;
    }

    /*
     * print pixels 
     */
    (*data_return)[header_nlines] = (char *) *data_return +
	header_size + attrib->height * sizeof(char *);

    iptr = attrib->pixelindex;

    for (y = 0; y < attrib->height; y++) {

	for (x = 0; x < attrib->width; x++, iptr++)
	    for (b = 0; b < attrib->cpp; b++)
		(*data_return)[header_nlines + y][x * attrib->cpp + b] =
		    attrib->colorStrings[*iptr][b];

	(*data_return)[header_nlines + y][attrib->width * attrib->cpp] = '\0';

	if (y < attrib->height - 1)
	    (*data_return)[header_nlines + y + 1] =
		(*data_return)[header_nlines + y] +
		attrib->width * attrib->cpp + 1;
    }

    xpmFreeRgbNames(rgbn, rgbn_max);

    RETURN(XpmSuccess);
}
