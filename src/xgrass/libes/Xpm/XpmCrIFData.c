/* Copyright 1990,91 GROUPE BULL -- See licence conditions in file COPYRIGHT */
/*****************************************************************************\
* XpmCrIFData.c:                                                              *
*                                                                             *
*  XPM library                                                                *
*  Parse an Xpm array and create the image and possibly its mask              *
*                                                                             *
*  Developed by Arnaud Le Hors                                                *
\*****************************************************************************/

#include "xpmP.h"

int
XpmCreateImageFromData(display, data, image_return,
		       shapeimage_return, attributes)
    Display *display;
    char **data;
    XImage **image_return;
    XImage **shapeimage_return;
    XpmAttributes *attributes;
{
    xpmData mdata;
    int ErrorStatus;
    xpmInternAttrib attrib;

    /*
     * initialize return values 
     */
    if (image_return)
	*image_return = NULL;
    if (shapeimage_return)
	*shapeimage_return = NULL;

    if ((ErrorStatus = xpmOpenArray(data, &mdata)) != XpmSuccess)
	return (ErrorStatus);

    xpmInitInternAttrib(&attrib);

    ErrorStatus = xpmParseData(&mdata, &attrib, attributes);

    if (ErrorStatus == XpmSuccess)
	ErrorStatus = xpmCreateImage(display, &attrib, image_return,
				     shapeimage_return, attributes);

    if (ErrorStatus >= 0)
	xpmSetAttributes(&attrib, attributes);
    else if (attributes)
	XpmFreeAttributes(attributes);

    xpmFreeInternAttrib(&attrib);
    XpmDataClose(&mdata);

    return (ErrorStatus);
}
