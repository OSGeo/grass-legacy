/* Copyright 1990,91 GROUPE BULL -- See licence conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmWrFFrI.c:                                                                *
*                                                                             *
*  XPM library                                                                *
*  Write an image and possibly its mask to an XPM file                        *
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

LFUNC(WriteData, int, (xpmData * mdata,
		    xpmInternAttrib * attrib, XpmAttributes * attributes));

int
XpmWriteFileFromImage(display, filename, image, shapeimage, attributes)
    Display *display;
    char *filename;
    XImage *image;
    XImage *shapeimage;
    XpmAttributes *attributes;
{
    xpmData mdata;
    char *name, *end_name, *begin_name = NULL;
    int ErrorStatus;
    xpmInternAttrib attrib;

    if ((ErrorStatus = xpmWriteFile(filename, &mdata)) != XpmSuccess)
	return (ErrorStatus);

    if (filename) {
#ifdef VMS
	name = filename;
#else
	if (!(name = rindex(filename, '/')))
	    name = filename;
	else
	    name++;
#endif
	if (end_name = index(name, '.')) {
	    begin_name = name;
	    name = (char *) malloc((unsigned int) (end_name - begin_name) + 1);
	    if (!name) {
		begin_name = NULL;
		name = "image_name";
	    } else {
		strncpy(name, begin_name,
			(unsigned int) (end_name - begin_name));
		name[(unsigned int) (end_name - begin_name)] = '\0';
	    }
	}
    } else
	name = "image_name";

    xpmInitInternAttrib(&attrib);

    /*
     * Scan image then write it out 
     */
    ErrorStatus = xpmScanImage(display, image, shapeimage,
			       attributes, &attrib);

    if (ErrorStatus == XpmSuccess) {
	attrib.name = name;
	ErrorStatus = WriteData(&mdata, &attrib, attributes);
    }
    xpmFreeInternAttrib(&attrib);
    XpmDataClose(&mdata);
    if (begin_name)
	free(name);

    return (ErrorStatus);
}


static int
WriteData(mdata, attrib, attributes)
    xpmData *mdata;
    xpmInternAttrib *attrib;
    XpmAttributes *attributes;
{
    /* calculation variables */
    xpmRgbName rgbn[MAX_RGBNAMES];
    int rgbn_max = 0;
    char *colorname;
    unsigned int *iptr;
    unsigned int a, b, c, x, y, n = 0, key, d, e;


    /*
     * read the rgb file if any was specified 
     */
    if (attributes && (attributes->valuemask & XpmRgbFilename))
	rgbn_max = xpmReadRgbNames(attributes->rgb_fname, rgbn);

    /* force output type to the C syntax */
    n = 1;

    /*
     * print the header line 
     */
    fprintf(mdata->stream.file, "%s XPM %s\n", xpmDataTypes[n].Bcmt,
	    xpmDataTypes[n].Ecmt);
    if (n != 0)				/* print the assignment line */
	fprintf(mdata->stream.file, "%s %s %s",
		xpmDataTypes[n].Dec, attrib->name, xpmDataTypes[n].Boa);

    /*
     * print the hints line 
     */
    if (attributes && (attributes->valuemask & XpmInfos)
	&& attributes->hints_cmt)
	/* print hints comment line */
	fprintf(mdata->stream.file, "%s%s%s\n",
	xpmDataTypes[n].Bcmt, attributes->hints_cmt, xpmDataTypes[n].Ecmt);

    if (xpmDataTypes[n].Bos)
	fprintf(mdata->stream.file, "%c", xpmDataTypes[n].Bos);

    fprintf(mdata->stream.file, "%d %d %d %d",
	    attrib->width, attrib->height, attrib->ncolors, attrib->cpp);

    if (attributes && (attributes->valuemask & XpmHotspot))
	fprintf(mdata->stream.file, " %d %d",
		attributes->x_hotspot, attributes->y_hotspot);

    if (xpmDataTypes[n].Eos)
	fprintf(mdata->stream.file, "%c", xpmDataTypes[n].Eos);

    fprintf(mdata->stream.file, xpmDataTypes[n].Strs);

    /*
     * print colors 
     */
    if (attributes && (attributes->valuemask & XpmInfos)
	&& attributes->colors_cmt)
	/* print colors comment line */
	fprintf(mdata->stream.file, "%s%s%s\n",
	xpmDataTypes[n].Bcmt, attributes->colors_cmt, xpmDataTypes[n].Ecmt);

    if (attrib->mask_pixel != UNDEF_PIXEL) {	/* transparent pixel */
	if (xpmDataTypes[n].Bos)
	    fprintf(mdata->stream.file, "%c", xpmDataTypes[n].Bos);

	for (b = 0; b < attrib->cpp; b++)
	    fprintf(mdata->stream.file, "%c", attrib->colorStrings[0][b]);

	if (attributes && (attributes->valuemask & XpmInfos)
	    && attributes->mask_pixel != UNDEF_PIXEL) {
	    for (key = 1; key < NKEYS + 1; key++) {
		if (attributes->colorTable[attributes->mask_pixel][key])
		    fprintf(mdata->stream.file, "\t%s %s",
			    xpmColorKeys[key - 1],
			attributes->colorTable[attributes->mask_pixel][key]
			);
	    }
	} else
	    fprintf(mdata->stream.file, "\tc %s", TRANSPARENT_COLOR);

	if (xpmDataTypes[n].Eos)
	    fprintf(mdata->stream.file, "%c", xpmDataTypes[n].Eos);

	fprintf(mdata->stream.file, xpmDataTypes[n].Strs);
	d = 1;
    } else
	d = 0;

    for (a = d; a < attrib->ncolors; a++) {	/* other colors */
	if (xpmDataTypes[n].Bos)
	    fprintf(mdata->stream.file, "%c", xpmDataTypes[n].Bos);

	for (b = 0; b < attrib->cpp; b++)
	    fprintf(mdata->stream.file, "%c", attrib->colorStrings[a][b]);

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
			fprintf(mdata->stream.file, "\t%s %s",
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
		fprintf(mdata->stream.file, "\tc %s", colorname);
	    else
		fprintf(mdata->stream.file, "\tc #%04X%04X%04X",
			attrib->xcolors[a].red,
			attrib->xcolors[a].green,
			attrib->xcolors[a].blue);
	}
	if (xpmDataTypes[n].Eos)
	    fprintf(mdata->stream.file, "%c", xpmDataTypes[n].Eos);

	fprintf(mdata->stream.file, xpmDataTypes[n].Strs);
    }

    /*
     * print pixels 
     */
    if (attributes && (attributes->valuemask & XpmInfos)
	&& attributes->pixels_cmt)
	/* print pixels comment line */
	fprintf(mdata->stream.file, "%s%s%s\n",
	xpmDataTypes[n].Bcmt, attributes->pixels_cmt, xpmDataTypes[n].Ecmt);

    iptr = attrib->pixelindex;

    for (y = 0; y < attrib->height; y++) {

	if (xpmDataTypes[n].Bos)
	    fprintf(mdata->stream.file, "%c", xpmDataTypes[n].Bos);

	for (x = 0; x < attrib->width; x++, iptr++)
	    for (b = 0; b < attrib->cpp; b++)
		fprintf(mdata->stream.file, "%c",
			attrib->colorStrings[*iptr][b]);

	if (xpmDataTypes[n].Eos)
	    fprintf(mdata->stream.file, "%c", xpmDataTypes[n].Eos);

	if (y < attrib->height - 1)
	    fprintf(mdata->stream.file, xpmDataTypes[n].Strs);
    }

    fprintf(mdata->stream.file, xpmDataTypes[n].Eoa);

    xpmFreeRgbNames(rgbn, rgbn_max);

    return (XpmSuccess);
}
