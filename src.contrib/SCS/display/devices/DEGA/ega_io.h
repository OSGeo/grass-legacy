/* File: ega_io.h		Author: Paul W. Carlson		Jan. 1989
 */
#define EGAMAP	('E' << 8)

/***************************************************************************
 *      The following are the command identifiers for the EGA device driver 
 *	ioctl calls.  They are converted by the driver to indices into an 
 *	array of pointers to functions.  
 ****************************************************************************/
#define EGA_DRAW	(EGAMAP |  0)
#define EGA_MOVE	(EGAMAP |  1)
#define EGA_PUTRAST	(EGAMAP |  2)
#define EGA_SETCOLOR	(EGAMAP |  3)
#define EGA_SETPIX	(EGAMAP |  4)
#define EGA_GETPIX	(EGAMAP |  5)
#define EGA_HORLINE	(EGAMAP |  6)
#define EGA_ERASE	(EGAMAP |  7)
#define EGA_PUTPAL	(EGAMAP |  8)
#define EGA_RASTER	(EGAMAP |  9)
#define EGA_XHAIR	(EGAMAP | 10)
#define EGA_RUBBOX	(EGAMAP | 11)
#define EGA_RUBLINE	(EGAMAP | 12)
#define EGA_SETXHAIR	(EGAMAP | 13)
#define EGA_SETRBOX	(EGAMAP | 14)
#define EGA_SETRLINE	(EGAMAP | 15)
#define EGA_TEXTMODE	(EGAMAP | 16)
#define EGA_GRAPHMODE	(EGAMAP | 17)
#define EGA_VERTLINE	(EGAMAP | 18)
#define EGA_CIRCLE	(EGAMAP | 19)
#define EGA_FILLCIRC	(EGAMAP | 20)
#define EGA_BITTEXT	(EGAMAP | 21)
#define EGA_GETRAST	(EGAMAP | 22)
#define EGA_GETCURS	(EGAMAP | 23)
#define EGA_PUTCURS	(EGAMAP | 24)
#define EGA_VERSION	(EGAMAP | 25)
#define EGA_SETORIG	(EGAMAP | 26)
#define EGA_VIDWIDTH	(EGAMAP | 27)
#define EGA_OUTPORT	(EGAMAP | 28)
#define EGA_SETDITH	(EGAMAP | 29)
#define EGA_DITHDRAW	(EGAMAP | 30)
#define EGA_PUTDITH	(EGAMAP | 31)
#define EGA_DITHBOX	(EGAMAP | 32)
#define EGA_DITHLINE	(EGAMAP | 33)
#define EGA_DITHCIRC	(EGAMAP | 34)
#define EGA_DITHOVER	(EGAMAP | 35)
#define EGA_DITHPIX	(EGAMAP | 36)
#define EGA_GETPANEL	(EGAMAP | 37)
#define EGA_PUTPANEL	(EGAMAP | 38)

/***************************************************************************
 *      The following are the command identifiers for the Logitech Bus Mouse
 * 	device driver ioctl calls.  
 ****************************************************************************/
#define MOUSE_INIT	(('M' << 8) | 81)
#define GET_MOUSE	(('M' << 8) | 82)

/******** Define the horizontal and vertical dispaly resolution ******/
#define H_RES	640
#define V_RES	350

/******** Global variables *****************************/
#ifdef GLOBAL
#define GTYPE
#else 
#define GTYPE extern
#endif

GTYPE int egafd;			/* /dev/ega file descriptor */
GTYPE int mousfd;			/* /dev/mouse file descriptor */
GTYPE int ega_is_open;			/* /dev/ega open flag */
GTYPE int mouse_is_open;		/* /dev/mouse open flag */
GTYPE int cur_x, cur_y;			/* current coordinates in window*/
GTYPE unsigned char cur_color; 		/* current color */
GTYPE unsigned char raster_buff[1024];	/* array of pixel patterns */

GTYPE unsigned char plane_0[187];	/* video plane 0 bytes */
GTYPE unsigned char plane_1[187];	/* video plane 1 bytes */
GTYPE unsigned char plane_2[187];	/* video plane 2 bytes */
GTYPE unsigned char plane_3[187];	/* video plane 3 bytes */

GTYPE double _text_size_x;		/* text width */
GTYPE double _text_size_y;		/* text heught */
GTYPE double _text_rotation;		/* text rotation */

GTYPE int SCREEN_LEFT;			/* minimum x coordinate */
GTYPE int SCREEN_TOP;			/* minimum y coordinate */
GTYPE int SCREEN_RIGHT;			/* maximum x coordinate */
GTYPE int SCREEN_BOTTOM;		/* maximum y coordinate */

GTYPE int NCOLORS;			/* number of colors */

GTYPE struct io_args {			/* ioctl structure */
    int arg1;
    int arg2;
    int arg3;
    int arg4;
    int arg5;
    unsigned char *ptr1;
    unsigned char *ptr2;
    unsigned char *ptr3;
    unsigned char *ptr4;
} args;

