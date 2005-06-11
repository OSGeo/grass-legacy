/*
 *	getpix and putpix -
 *
 *				Paul Haeberli - 1984
 *
 */
#include	<stdio.h>
#include	"image.h"

#undef getpix
#undef putpix

unsigned short getpix(IMAGE *image)
{
    if(--(image)->cnt>=0)
    	return *(image)->ptr++;
    else
	return ifilbuf(image);
}

unsigned short putpix(IMAGE *image, unsigned long pix)
{
    if(--(image)->cnt>=0)
        return *(image)->ptr++ = pix;
    else
	return iflsbuf(image,pix);
}
