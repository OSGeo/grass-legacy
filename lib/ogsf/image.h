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
#include <sys/types.h>


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
#define	IBUFSIZE(pixels)	(((pixels+(pixels>>6))<<2)*sizeof(char))
#define	RLE_NOP			0x00

#define	ierror(p)		(((p)->flags&_IOERR)!=0)
#define	ifileno(p)		((p)->file)
#define	getpix(p)		(--(p)->cnt>=0 ? *(p)->ptr++ : ifilbuf(p))
#define putpix(p,x)		(--(p)->cnt>=0 \
				    ? ((int)(*(p)->ptr++=(unsigned)(x))) \
				    : iflsbuf(p,(unsigned)(x)))

#if defined(BSDTYPES) || defined(VMS)
typedef unsigned char	u_char;
typedef unsigned short	u_short;
typedef unsigned int	u_int;
typedef unsigned long	u_long;
#endif


typedef struct {
    /* SGI IRIS image header */
    unsigned short	imagic;		/* MAGIC for ioctl()	*/
    unsigned short 	type;		/* storage format	*/
    unsigned short 	dim;		/* number of dimensions	*/
    unsigned short 	xsize;		/* X size in pixels	*/
    unsigned short 	ysize;		/* Y size in pixels	*/
    unsigned short 	zsize;		/* number of channels	*/
    unsigned long 	min;		/* min pixel value	*/
    unsigned long 	max;		/* max pixel value	*/
    unsigned long	wastebytes;	/* reserved		*/
    char 		name[80];	/* image name		*/
    unsigned long	colormap;	/* colormap ID		*/

    /* stuff used in core only */
    int 		file;		/* file descriptor	*/
    unsigned short 	flags;		/* file flags		*/
    short		dorev;		/* endian flag		*/
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


/* close.c */
int iclose(IMAGE *image);
int iflush(IMAGE *image);

/* filbuf.c */
short ifilbuf(IMAGE *image);

/* flsbuf.c */
long iflsbuf(IMAGE *image, u_long c);

/* open.c */
IMAGE *iopen(char *file, char *mode, u_int type, u_int dim,
            u_int xsize, u_int ysize, u_int zsize);
IMAGE *fiopen(int f, char *mode, u_int type, u_int dim,
            u_int xsize, u_int ysize, u_int zsize);

/* row.c */
int putrow(IMAGE *image, u_short *buffer, u_int y, u_int z);
int getrow(IMAGE *image, u_short *buffer, u_int y, u_int z);


#define IMAGEDEF		/* for backwards compatibility */
#endif	/* !__GL_IMAGE_H__ */
