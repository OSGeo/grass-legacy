/*
 *	isetname and isetcolormap -
 *
 *				Paul Haeberli - 1984
 *
 */
#include	<stdio.h>
#include	<string.h>
#include	"image.h"

void isetname(image,name)
IMAGE *image;
char *name;
{
    strncpy(image->name,name,80);
}

void isetcolormap(image,colormap)
IMAGE *image;
int colormap;
{
    image->colormap = colormap;
}
