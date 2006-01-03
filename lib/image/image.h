#ifndef	__GL_IMAGE_H__
#define	__GL_IMAGE_H__
/*
 *	Defines for image files . . . .
 *
 *  			Paul Haeberli - 1984
 *      Look in /usr/people/4Dgifts/iristools/imgtools for example code!
 *
 */

#include <stdio.h>

#define IMAGIC                  0732

/* colormap of images */
#define CM_NORMAL		0	/* file contains rows of values which 
					 * are either RGB values (zsize == 3) 
					 * or greyramp values (zsize == 1) */
#define CM_DITHERED		1
#define CM_SCREEN		2	/* file contains data which is a screen
					 * image; getrow returns buffer which 
					 * can be displayed directly with 
					 * writepixels */
#define CM_COLORMAP		3	/* a colormap file */

/* added by J. Hofierka for Linux users*/

#define _IOWRT                  0002
#define _IOERR                  0040
#define _IOREAD                 0001
#define _IOEOF                  0020
#define _IORW                   0200

#define TYPEMASK		0xff00
#define BPPMASK			0x00ff
#define ITYPE_VERBATIM		0x0000
#define ITYPE_RLE		0x0100
#define ISRLE(type)		(((type) & 0xff00) == ITYPE_RLE)
#define ISVERBATIM(type)	(((type) & 0xff00) == ITYPE_VERBATIM)
#define BPP(type)		((type) & BPPMASK)
#define RLE(bpp)		(ITYPE_RLE | (bpp))
#define VERBATIM(bpp)		(ITYPE_VERBATIM | (bpp))
#define	IBUFSIZE(pixels)	((pixels+(pixels>>6))<<2)
#define	RLE_NOP			0x00

#define	ierror(p)		(((p)->flags&_IOERR)!=0)
#define	ifileno(p)		((p)->file)
#define	getpix(p)		(--(p)->cnt>=0 ? *(p)->ptr++ : ifilbuf(p))
#define putpix(p,x)		(--(p)->cnt>=0 \
				    ? ((int)(*(p)->ptr++=(unsigned)(x))) \
				    : iflsbuf(p,(unsigned)(x)))

typedef struct {
    unsigned short	imagic;		/* stuff saved on disk . . */
    unsigned short 	type;
    unsigned short 	dim;
    unsigned short 	xsize;
    unsigned short 	ysize;
    unsigned short 	zsize;
    unsigned long 	min;
    unsigned long 	max;
    unsigned long	wastebytes;	
    char 		name[80];
    unsigned long	colormap;

    long 		file;		/* stuff used in core only */
    unsigned short 	flags;
    short		dorev;
    short		x;
    short		y;
    short		z;
    short		cnt;
    unsigned short	*ptr;
    unsigned short	*base;
    unsigned short	*tmpbuf;
    unsigned long	offset;
    unsigned long	rleend;		/* for rle images */
    unsigned long	*rowstart;	/* for rle images */
    long		*rowsize;	/* for rle images */
} IMAGE;

IMAGE *iopen();
IMAGE *icreate();
unsigned short *ibufalloc();
int putrow(IMAGE *,unsigned short *,unsigned int,unsigned int);
int iflush(IMAGE *);
int cvtlongs(long *,long);
int cvtshorts(unsigned short *,long);
int cvtimage(long *);
int img_write(IMAGE *,char *,long);
int img_optseek(IMAGE *,unsigned long);
int i_errhdlr();
int getrow(IMAGE *,unsigned short *,unsigned int, unsigned int);
int isetname(IMAGE *,char *);
int ifilbuf(IMAGE *);
int iflsbuf(IMAGE *,unsigned long);
int img_badrow(IMAGE *,int,int);
int img_seek(IMAGE *,unsigned int,unsigned int);
int img_read(IMAGE *,char *,long);
int img_getrowsize(IMAGE *);
int img_setrowsize(IMAGE *,long, long, long);
int img_rle_compact(unsigned short *, int, unsigned short *, int, int);
int img_rle_expand(unsigned short *, int, unsigned short *,int);

#define IMAGEDEF		/* for backwards compatibility */
#endif	/* !__GL_IMAGE_H__ */
