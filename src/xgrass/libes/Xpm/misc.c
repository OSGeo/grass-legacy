/* Copyright 1990,91 GROUPE BULL -- See licence conditions in file COPYRIGHT */
/*****************************************************************************\
* misc.c:                                                                     *
*                                                                             *
*  XPM library                                                               *
*  Miscellaneous utilities                                                    *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

/*
 * Free the computed color table
 */

xpmFreeColorTable(colorTable, ncolors)
    char ***colorTable;
    int ncolors;
{
    int a, b;

    if (colorTable) {
	for (a = 0; a < ncolors; a++)
	    if (colorTable[a]) {
		for (b = 0; b < (NKEYS + 1); b++)
		    if (colorTable[a][b])
			free(colorTable[a][b]);
		free(colorTable[a]);
	    }
	free(colorTable);
    }
}


/*
 * Intialize the xpmInternAttrib pointers to Null to know
 * which ones must be freed later on.
 */

xpmInitInternAttrib(attrib)
    xpmInternAttrib *attrib;
{
    attrib->ncolors = 0;
    attrib->colorTable = NULL;
    attrib->pixelindex = NULL;
    attrib->xcolors = NULL;
    attrib->colorStrings = NULL;
    attrib->mask_pixel = UNDEF_PIXEL;
}


/*
 * Free the xpmInternAttrib pointers which have been allocated
 */

xpmFreeInternAttrib(attrib)
    xpmInternAttrib *attrib;
{
    unsigned int a;

    if (attrib->colorTable)
	xpmFreeColorTable(attrib->colorTable, attrib->ncolors);
    if (attrib->pixelindex)
	free(attrib->pixelindex);
    if (attrib->xcolors)
	free(attrib->xcolors);
    if (attrib->colorStrings) {
	for (a = 0; a < attrib->ncolors; a++)
	    if (attrib->colorStrings[a])
		free(attrib->colorStrings[a]);
	free(attrib->colorStrings);
    }
}


/*
 * Retuen the XpmAttributes structure size
 */

XpmAttributesSize()
{
    return sizeof(XpmAttributes);
}


/*
 * Free the XpmAttributes structure members
 * but the structure itself
 */

XpmFreeAttributes(attributes)
    XpmAttributes *attributes;
{
    if (attributes) {
	if (attributes->valuemask & XpmReturnPixels && attributes->pixels) {
	    free(attributes->pixels);
	    attributes->pixels = NULL;
	    attributes->npixels = 0;
	}
	if (attributes->valuemask & XpmInfos) {
	    if (attributes->colorTable) {
		xpmFreeColorTable(attributes->colorTable, attributes->ncolors);
		attributes->colorTable = NULL;
		attributes->ncolors = 0;
	    }
	    if (attributes->hints_cmt) {
		free(attributes->hints_cmt);
		attributes->hints_cmt = NULL;
	    }
	    if (attributes->colors_cmt) {
		free(attributes->colors_cmt);
		attributes->colors_cmt = NULL;
	    }
	    if (attributes->pixels_cmt) {
		free(attributes->pixels_cmt);
		attributes->pixels_cmt = NULL;
	    }
	    if (attributes->pixels) {
		free(attributes->pixels);
		attributes->pixels = NULL;
	    }
	}
	attributes->valuemask = 0;
    }
}


/*
 * Store into the XpmAttributes structure the required informations stored in
 * the xpmInternAttrib structure.
 */

xpmSetAttributes(attrib, attributes)
    xpmInternAttrib *attrib;
    XpmAttributes *attributes;
{
    if (attributes) {
	if (attributes->valuemask & XpmReturnInfos) {
	    attributes->cpp = attrib->cpp;
	    attributes->ncolors = attrib->ncolors;
	    attributes->colorTable = attrib->colorTable;

	    attrib->ncolors = 0;
	    attrib->colorTable = NULL;
	}
	attributes->width = attrib->width;
	attributes->height = attrib->height;
	attributes->valuemask |= XpmSize;
    }
}
